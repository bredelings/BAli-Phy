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
#line 511 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2576 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 528 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2582 "parser.cc"
    break;

  case 4: // module: body2
#line 529 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2588 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 531 "parser.y"
                                                                 {drv.push_module_context();}
#line 2594 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 539 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2600 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 540 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2606 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 542 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2612 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 543 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2618 "parser.cc"
    break;

  case 13: // top: semis top1
#line 546 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2624 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 548 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2630 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 549 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2636 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 550 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2642 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 558 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2648 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 559 "parser.y"
                                      {}
#line 2654 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 561 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2660 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 562 "parser.y"
                                      {}
#line 2666 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 563 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2672 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 564 "parser.y"
                                      {}
#line 2678 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 566 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2684 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 567 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2690 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 569 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2696 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 570 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2702 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 571 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2708 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 573 "parser.y"
                                      {}
#line 2714 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 574 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2720 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 575 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2726 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 577 "parser.y"
                   {}
#line 2732 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 578 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2738 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 580 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2744 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 581 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2750 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 583 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2756 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 584 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2762 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 594 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2768 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 596 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2774 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 597 "parser.y"
                         { }
#line 2780 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 599 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2788 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 612 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2794 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 613 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2800 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 615 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2806 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 616 "parser.y"
                               { }
#line 2812 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 618 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2818 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 619 "parser.y"
                               { }
#line 2824 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 623 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2830 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 624 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2836 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 626 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2842 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 627 "parser.y"
                                      {}
#line 2848 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 628 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2854 "parser.cc"
    break;

  case 56: // importlist: ','
#line 629 "parser.y"
                                      {}
#line 2860 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 631 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2866 "parser.cc"
    break;

  case 58: // importlist1: import
#line 632 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2872 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 634 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2878 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 635 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2884 "parser.cc"
    break;

  case 61: // prec: %empty
#line 640 "parser.y"
                   { }
#line 2890 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 641 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2896 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 643 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2902 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 644 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2908 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 645 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2914 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 647 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2920 "parser.cc"
    break;

  case 67: // ops: op
#line 648 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2926 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 652 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2932 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 654 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2938 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 655 "parser.y"
                                            { }
#line 2944 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 657 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2950 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 658 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2956 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 659 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2962 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 660 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2968 "parser.cc"
    break;

  case 75: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 663 "parser.y"
                                                         {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2974 "parser.cc"
    break;

  case 76: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 664 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2980 "parser.cc"
    break;

  case 77: // topdecl: decl_no_th
#line 670 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2986 "parser.cc"
    break;

  case 78: // topdecl: infixexp
#line 672 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2992 "parser.cc"
    break;

  case 79: // cl_decl: "class" tycl_hdr fds where_cls
#line 675 "parser.y"
                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2998 "parser.cc"
    break;

  case 80: // ty_decl: "type" type "=" ktype
#line 678 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3004 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 679 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 3010 "parser.cc"
    break;

  case 82: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 681 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 3016 "parser.cc"
    break;

  case 83: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 682 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3022 "parser.cc"
    break;

  case 84: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 683 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3028 "parser.cc"
    break;

  case 85: // standalone_kind_sig: "type" sks_vars "::" kind
#line 685 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3034 "parser.cc"
    break;

  case 86: // sks_vars: sks_vars "," oqtycon
#line 687 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3040 "parser.cc"
    break;

  case 87: // sks_vars: oqtycon
#line 688 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3046 "parser.cc"
    break;

  case 88: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 691 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3052 "parser.cc"
    break;

  case 89: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 692 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3058 "parser.cc"
    break;

  case 90: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 694 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 3068 "parser.cc"
    break;

  case 91: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 700 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 3078 "parser.cc"
    break;

  case 92: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 706 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3084 "parser.cc"
    break;

  case 93: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 707 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3090 "parser.cc"
    break;

  case 94: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 708 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3096 "parser.cc"
    break;

  case 95: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 709 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3102 "parser.cc"
    break;

  case 96: // overlap_pragma: %empty
#line 710 "parser.y"
                                               {}
#line 3108 "parser.cc"
    break;

  case 106: // where_type_family: %empty
#line 737 "parser.y"
                                                           {}
#line 3114 "parser.cc"
    break;

  case 107: // where_type_family: "where" ty_fam_inst_eqn_list
#line 738 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3120 "parser.cc"
    break;

  case 108: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 740 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3126 "parser.cc"
    break;

  case 109: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 741 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3132 "parser.cc"
    break;

  case 110: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 742 "parser.y"
                                                           {}
#line 3138 "parser.cc"
    break;

  case 111: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 743 "parser.y"
                                                           {}
#line 3144 "parser.cc"
    break;

  case 112: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 745 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3150 "parser.cc"
    break;

  case 113: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 746 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3156 "parser.cc"
    break;

  case 114: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 747 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3162 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqns: %empty
#line 748 "parser.y"
                                                           {}
#line 3168 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn: type "=" ctype
#line 750 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3174 "parser.cc"
    break;

  case 117: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 753 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3180 "parser.cc"
    break;

  case 118: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 755 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3186 "parser.cc"
    break;

  case 119: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 757 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3192 "parser.cc"
    break;

  case 120: // at_decl_cls: "type" ty_fam_inst_eqn
#line 759 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3198 "parser.cc"
    break;

  case 121: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 760 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3204 "parser.cc"
    break;

  case 126: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 768 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3210 "parser.cc"
    break;

  case 127: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 771 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3220 "parser.cc"
    break;

  case 128: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 778 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3230 "parser.cc"
    break;

  case 129: // data_or_newtype: "data"
#line 784 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3236 "parser.cc"
    break;

  case 130: // data_or_newtype: "newtype"
#line 785 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3242 "parser.cc"
    break;

  case 131: // opt_class: %empty
#line 788 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3248 "parser.cc"
    break;

  case 132: // opt_class: qtycon
#line 789 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3254 "parser.cc"
    break;

  case 133: // opt_kind_sig: %empty
#line 793 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3260 "parser.cc"
    break;

  case 134: // opt_kind_sig: "::" kind
#line 794 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3266 "parser.cc"
    break;

  case 135: // opt_datafam_kind_sig: %empty
#line 796 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3272 "parser.cc"
    break;

  case 136: // opt_datafam_kind_sig: "::" kind
#line 797 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3278 "parser.cc"
    break;

  case 137: // opt_tyfam_kind_sig: %empty
#line 799 "parser.y"
                                      {}
#line 3284 "parser.cc"
    break;

  case 138: // opt_tyfam_kind_sig: "::" kind
#line 800 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3290 "parser.cc"
    break;

  case 139: // opt_tyfam_kind_sig: "=" tv_bndr
#line 801 "parser.y"
                                      {}
#line 3296 "parser.cc"
    break;

  case 140: // opt_at_kind_inj_sig: %empty
#line 803 "parser.y"
                                      {}
#line 3302 "parser.cc"
    break;

  case 141: // opt_at_kind_inj_sig: "::" kind
#line 804 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3308 "parser.cc"
    break;

  case 142: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 805 "parser.y"
                                                                  {}
#line 3314 "parser.cc"
    break;

  case 143: // tycl_hdr: context "=>" type
#line 808 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3320 "parser.cc"
    break;

  case 144: // tycl_hdr: type
#line 809 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3326 "parser.cc"
    break;

  case 145: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 812 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3332 "parser.cc"
    break;

  case 146: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 813 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3338 "parser.cc"
    break;

  case 147: // datafam_inst_hdr: context "=>" type
#line 814 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3344 "parser.cc"
    break;

  case 148: // datafam_inst_hdr: type
#line 815 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3350 "parser.cc"
    break;

  case 152: // decl_cls: at_decl_cls
#line 861 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3356 "parser.cc"
    break;

  case 153: // decl_cls: decl
#line 862 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3362 "parser.cc"
    break;

  case 154: // decls_cls: decls_cls ";" decl_cls
#line 864 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3368 "parser.cc"
    break;

  case 155: // decls_cls: decls_cls ";"
#line 865 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3374 "parser.cc"
    break;

  case 156: // decls_cls: decl_cls
#line 866 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3380 "parser.cc"
    break;

  case 157: // decls_cls: %empty
#line 867 "parser.y"
                                           {}
#line 3386 "parser.cc"
    break;

  case 158: // decllist_cls: "{" decls_cls "}"
#line 869 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3392 "parser.cc"
    break;

  case 159: // decllist_cls: "vocurly" decls_cls close
#line 870 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3398 "parser.cc"
    break;

  case 160: // where_cls: "where" decllist_cls
#line 872 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3404 "parser.cc"
    break;

  case 161: // where_cls: %empty
#line 873 "parser.y"
                                           {}
#line 3410 "parser.cc"
    break;

  case 162: // decl_inst: at_decl_inst
#line 875 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3416 "parser.cc"
    break;

  case 163: // decl_inst: decl
#line 876 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3422 "parser.cc"
    break;

  case 164: // decls_inst: decls_inst ";" decl_inst
#line 878 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3428 "parser.cc"
    break;

  case 165: // decls_inst: decls_inst ";"
#line 879 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3434 "parser.cc"
    break;

  case 166: // decls_inst: decl_inst
#line 880 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3440 "parser.cc"
    break;

  case 167: // decls_inst: %empty
#line 881 "parser.y"
                                           {}
#line 3446 "parser.cc"
    break;

  case 168: // decllist_inst: "{" decls_inst "}"
#line 883 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3452 "parser.cc"
    break;

  case 169: // decllist_inst: "vocurly" decls_inst close
#line 884 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3458 "parser.cc"
    break;

  case 170: // where_inst: "where" decllist_inst
#line 886 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3464 "parser.cc"
    break;

  case 171: // where_inst: %empty
#line 887 "parser.y"
                                           {}
#line 3470 "parser.cc"
    break;

  case 172: // decls: decls ";" decl
#line 890 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3476 "parser.cc"
    break;

  case 173: // decls: decls ";"
#line 891 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3482 "parser.cc"
    break;

  case 174: // decls: decl
#line 892 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3488 "parser.cc"
    break;

  case 175: // decls: %empty
#line 893 "parser.y"
                        {}
#line 3494 "parser.cc"
    break;

  case 176: // decllist: "{" decls "}"
#line 895 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3500 "parser.cc"
    break;

  case 177: // decllist: "vocurly" decls close
#line 896 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3506 "parser.cc"
    break;

  case 178: // binds: decllist
#line 898 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3512 "parser.cc"
    break;

  case 179: // wherebinds: "where" binds
#line 900 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3518 "parser.cc"
    break;

  case 180: // wherebinds: %empty
#line 901 "parser.y"
                                 {}
#line 3524 "parser.cc"
    break;

  case 186: // opt_tyconsig: %empty
#line 927 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3530 "parser.cc"
    break;

  case 187: // opt_tyconsig: "::" gtycon
#line 928 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3536 "parser.cc"
    break;

  case 188: // sigtype: ctype
#line 937 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3542 "parser.cc"
    break;

  case 189: // sigtypedoc: ctypedoc
#line 939 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3548 "parser.cc"
    break;

  case 190: // sig_vars: sig_vars "," var
#line 941 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3554 "parser.cc"
    break;

  case 191: // sig_vars: var
#line 942 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3560 "parser.cc"
    break;

  case 192: // sigtypes1: sigtype
#line 944 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3566 "parser.cc"
    break;

  case 193: // sigtypes1: sigtypes1 "," sigtype
#line 945 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3572 "parser.cc"
    break;

  case 194: // ktype: ctype
#line 954 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3578 "parser.cc"
    break;

  case 195: // ktype: ctype "::" kind
#line 955 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3584 "parser.cc"
    break;

  case 196: // ctype: "forall" tv_bndrs "." ctype
#line 957 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3590 "parser.cc"
    break;

  case 197: // ctype: context "=>" ctype
#line 958 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3596 "parser.cc"
    break;

  case 198: // ctype: type
#line 960 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3602 "parser.cc"
    break;

  case 199: // ctypedoc: ctype
#line 962 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3608 "parser.cc"
    break;

  case 200: // context: btype
#line 971 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3614 "parser.cc"
    break;

  case 201: // context_no_ops: btype_no_ops
#line 973 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3620 "parser.cc"
    break;

  case 202: // type: btype
#line 975 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3626 "parser.cc"
    break;

  case 203: // type: btype "->" ctype
#line 976 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3632 "parser.cc"
    break;

  case 204: // typedoc: type
#line 978 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3638 "parser.cc"
    break;

  case 205: // btype: infixtype
#line 981 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3644 "parser.cc"
    break;

  case 206: // infixtype: ftype
#line 983 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3650 "parser.cc"
    break;

  case 207: // infixtype: btype tyop btype
#line 984 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3656 "parser.cc"
    break;

  case 208: // btype_no_ops: atype_docs
#line 986 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3662 "parser.cc"
    break;

  case 209: // btype_no_ops: btype_no_ops atype_docs
#line 987 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3668 "parser.cc"
    break;

  case 210: // ftype: atype
#line 989 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3674 "parser.cc"
    break;

  case 211: // ftype: ftype tyarg
#line 991 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3680 "parser.cc"
    break;

  case 212: // ftype: ftype "@" atype
#line 992 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3686 "parser.cc"
    break;

  case 213: // tyarg: atype
#line 994 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3692 "parser.cc"
    break;

  case 214: // tyop: qtyconop
#line 996 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3698 "parser.cc"
    break;

  case 215: // tyop: tyvarop
#line 997 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3704 "parser.cc"
    break;

  case 216: // atype_docs: atype
#line 1004 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3710 "parser.cc"
    break;

  case 217: // atype: ntgtycon
#line 1011 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3716 "parser.cc"
    break;

  case 218: // atype: tyvar
#line 1012 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3722 "parser.cc"
    break;

  case 219: // atype: "*"
#line 1013 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3728 "parser.cc"
    break;

  case 220: // atype: PREFIX_BANG atype
#line 1014 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3734 "parser.cc"
    break;

  case 221: // atype: PREFIX_TILDE atype
#line 1015 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3740 "parser.cc"
    break;

  case 222: // atype: "{" fielddecls "}"
#line 1016 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3746 "parser.cc"
    break;

  case 223: // atype: "(" ")"
#line 1017 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3752 "parser.cc"
    break;

  case 224: // atype: "(" comma_types1 "," ktype ")"
#line 1018 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3758 "parser.cc"
    break;

  case 225: // atype: "[" ktype "]"
#line 1024 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3764 "parser.cc"
    break;

  case 226: // atype: "(" ktype ")"
#line 1025 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3770 "parser.cc"
    break;

  case 227: // inst_type: sigtype
#line 1028 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3776 "parser.cc"
    break;

  case 230: // comma_types0: comma_types1
#line 1033 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3782 "parser.cc"
    break;

  case 231: // comma_types0: %empty
#line 1034 "parser.y"
                                       { /* default construction OK */ }
#line 3788 "parser.cc"
    break;

  case 232: // comma_types1: ktype
#line 1036 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3794 "parser.cc"
    break;

  case 233: // comma_types1: comma_types1 "," ktype
#line 1037 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3800 "parser.cc"
    break;

  case 234: // tv_bndrs: tv_bndrs tv_bndr
#line 1044 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3806 "parser.cc"
    break;

  case 235: // tv_bndrs: %empty
#line 1045 "parser.y"
                               { /* default construction OK */}
#line 3812 "parser.cc"
    break;

  case 236: // tv_bndr: tv_bndr_no_braces
#line 1047 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3818 "parser.cc"
    break;

  case 237: // tv_bndr: "{" tyvar "}"
#line 1048 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3824 "parser.cc"
    break;

  case 238: // tv_bndr: "{" tyvar "::" kind "}"
#line 1049 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3830 "parser.cc"
    break;

  case 239: // tv_bndr_no_braces: tyvar
#line 1052 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3836 "parser.cc"
    break;

  case 240: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1053 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3842 "parser.cc"
    break;

  case 241: // fds: %empty
#line 1057 "parser.y"
                                    { /* default to empty */ }
#line 3848 "parser.cc"
    break;

  case 242: // fds: "|" fds1
#line 1058 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 3854 "parser.cc"
    break;

  case 243: // fds1: fds1 "," fd
#line 1060 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 3860 "parser.cc"
    break;

  case 244: // fds1: fd
#line 1061 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 3866 "parser.cc"
    break;

  case 245: // fd: varids0 "->" varids0
#line 1064 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 3872 "parser.cc"
    break;

  case 246: // varids0: varids0 tyvar
#line 1066 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3878 "parser.cc"
    break;

  case 247: // varids0: %empty
#line 1067 "parser.y"
                                    { /* default to empty */}
#line 3884 "parser.cc"
    break;

  case 248: // kind: ctype
#line 1072 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3890 "parser.cc"
    break;

  case 249: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1078 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3896 "parser.cc"
    break;

  case 250: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1079 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3902 "parser.cc"
    break;

  case 251: // gadt_constrlist: %empty
#line 1080 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3908 "parser.cc"
    break;

  case 252: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1082 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3914 "parser.cc"
    break;

  case 253: // gadt_constrs: gadt_constr
#line 1083 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3920 "parser.cc"
    break;

  case 254: // gadt_constr: optSemi con_list "::" sigtype
#line 1085 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3926 "parser.cc"
    break;

  case 255: // constrs: "=" constrs1
#line 1087 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3932 "parser.cc"
    break;

  case 256: // constrs1: constrs1 "|" constr
#line 1089 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3938 "parser.cc"
    break;

  case 257: // constrs1: constr
#line 1090 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3944 "parser.cc"
    break;

  case 258: // constr: forall context_no_ops "=>" constr_stuff
#line 1092 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3950 "parser.cc"
    break;

  case 259: // constr: forall constr_stuff
#line 1093 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3956 "parser.cc"
    break;

  case 260: // forall: "forall" tv_bndrs "."
#line 1095 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3962 "parser.cc"
    break;

  case 261: // forall: %empty
#line 1096 "parser.y"
                                {}
#line 3968 "parser.cc"
    break;

  case 262: // constr_stuff: btype_no_ops
#line 1098 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3974 "parser.cc"
    break;

  case 263: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1099 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3984 "parser.cc"
    break;

  case 264: // fielddecls: %empty
#line 1105 "parser.y"
                                {}
#line 3990 "parser.cc"
    break;

  case 265: // fielddecls: fielddecls1
#line 1106 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3996 "parser.cc"
    break;

  case 266: // fielddecls1: fielddecls1 "," fielddecl
#line 1108 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4002 "parser.cc"
    break;

  case 267: // fielddecls1: fielddecl
#line 1109 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4008 "parser.cc"
    break;

  case 268: // fielddecl: sig_vars "::" ctype
#line 1111 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4014 "parser.cc"
    break;

  case 279: // decl_no_th: sigdecl
#line 1130 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4020 "parser.cc"
    break;

  case 280: // decl_no_th: infixexp rhs
#line 1132 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4026 "parser.cc"
    break;

  case 281: // decl: decl_no_th
#line 1134 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4032 "parser.cc"
    break;

  case 282: // rhs: "=" exp wherebinds
#line 1138 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4038 "parser.cc"
    break;

  case 283: // rhs: gdrhs wherebinds
#line 1139 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4044 "parser.cc"
    break;

  case 284: // gdrhs: gdrhs gdrh
#line 1141 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4050 "parser.cc"
    break;

  case 285: // gdrhs: gdrh
#line 1142 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4056 "parser.cc"
    break;

  case 286: // gdrh: "|" guardquals "=" exp
#line 1146 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4062 "parser.cc"
    break;

  case 287: // sigdecl: sig_vars "::" sigtypedoc
#line 1156 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4068 "parser.cc"
    break;

  case 288: // sigdecl: infix prec ops
#line 1157 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4074 "parser.cc"
    break;

  case 289: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1159 "parser.y"
                                                    {}
#line 4080 "parser.cc"
    break;

  case 290: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1160 "parser.y"
                                            {}
#line 4086 "parser.cc"
    break;

  case 291: // sigdecl: "{-# SCC" qvar "#-}"
#line 1161 "parser.y"
                              {}
#line 4092 "parser.cc"
    break;

  case 292: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1162 "parser.y"
                                     {}
#line 4098 "parser.cc"
    break;

  case 293: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1163 "parser.y"
                                                               {}
#line 4104 "parser.cc"
    break;

  case 294: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1164 "parser.y"
                                                                      {}
#line 4110 "parser.cc"
    break;

  case 295: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1165 "parser.y"
                                                     {}
#line 4116 "parser.cc"
    break;

  case 300: // exp: infixexp "::" sigtype
#line 1177 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4122 "parser.cc"
    break;

  case 301: // exp: infixexp
#line 1178 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4128 "parser.cc"
    break;

  case 302: // infixexp: exp10
#line 1182 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4134 "parser.cc"
    break;

  case 303: // infixexp: infixexp qop exp10
#line 1183 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4140 "parser.cc"
    break;

  case 304: // exp10: PREFIX_MINUS fexp
#line 1185 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4146 "parser.cc"
    break;

  case 305: // exp10: fexp
#line 1186 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4152 "parser.cc"
    break;

  case 308: // fexp: fexp aexp
#line 1194 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4158 "parser.cc"
    break;

  case 309: // fexp: fexp "@" atype
#line 1195 "parser.y"
                                 {}
#line 4164 "parser.cc"
    break;

  case 310: // fexp: "static" aexp
#line 1196 "parser.y"
                                 {}
#line 4170 "parser.cc"
    break;

  case 311: // fexp: aexp
#line 1197 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4176 "parser.cc"
    break;

  case 312: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1200 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4182 "parser.cc"
    break;

  case 313: // aexp: PREFIX_TILDE aexp
#line 1201 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4188 "parser.cc"
    break;

  case 314: // aexp: PREFIX_BANG aexp
#line 1202 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4194 "parser.cc"
    break;

  case 315: // aexp: "\\" apats1 "->" exp
#line 1203 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4200 "parser.cc"
    break;

  case 316: // aexp: "let" binds "in" exp
#line 1204 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4206 "parser.cc"
    break;

  case 317: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1206 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4212 "parser.cc"
    break;

  case 318: // aexp: "case" exp "of" altslist
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4218 "parser.cc"
    break;

  case 319: // aexp: "do" stmtlist
#line 1209 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4224 "parser.cc"
    break;

  case 320: // aexp: "mdo" stmtlist
#line 1210 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4230 "parser.cc"
    break;

  case 321: // aexp: aexp1
#line 1212 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4236 "parser.cc"
    break;

  case 322: // aexp1: aexp1 "{" fbinds "}"
#line 1215 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4242 "parser.cc"
    break;

  case 323: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1216 "parser.y"
                                     { }
#line 4248 "parser.cc"
    break;

  case 324: // aexp1: aexp2
#line 1217 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4254 "parser.cc"
    break;

  case 325: // aexp2: qvar
#line 1220 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4260 "parser.cc"
    break;

  case 326: // aexp2: qcon
#line 1221 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4266 "parser.cc"
    break;

  case 327: // aexp2: literal
#line 1222 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4272 "parser.cc"
    break;

  case 328: // aexp2: "(" texp ")"
#line 1223 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4278 "parser.cc"
    break;

  case 329: // aexp2: "(" tup_exprs ")"
#line 1224 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4284 "parser.cc"
    break;

  case 330: // aexp2: "(" projection ")"
#line 1225 "parser.y"
                              {}
#line 4290 "parser.cc"
    break;

  case 331: // aexp2: "[" list "]"
#line 1230 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4296 "parser.cc"
    break;

  case 332: // aexp2: "_"
#line 1231 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4302 "parser.cc"
    break;

  case 335: // texp: exp
#line 1240 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4308 "parser.cc"
    break;

  case 336: // texp: infixexp qop
#line 1241 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4314 "parser.cc"
    break;

  case 337: // texp: qopm infixexp
#line 1242 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4320 "parser.cc"
    break;

  case 338: // tup_exprs: tup_exprs "," texp
#line 1247 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4326 "parser.cc"
    break;

  case 339: // tup_exprs: texp "," texp
#line 1248 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4332 "parser.cc"
    break;

  case 340: // list: texp
#line 1266 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4338 "parser.cc"
    break;

  case 341: // list: lexps
#line 1267 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4344 "parser.cc"
    break;

  case 342: // list: texp ".."
#line 1268 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4350 "parser.cc"
    break;

  case 343: // list: texp "," exp ".."
#line 1269 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4356 "parser.cc"
    break;

  case 344: // list: texp ".." exp
#line 1270 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4362 "parser.cc"
    break;

  case 345: // list: texp "," exp ".." exp
#line 1271 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4368 "parser.cc"
    break;

  case 346: // list: texp "|" squals
#line 1272 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4374 "parser.cc"
    break;

  case 347: // lexps: lexps "," texp
#line 1274 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4380 "parser.cc"
    break;

  case 348: // lexps: texp "," texp
#line 1275 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4386 "parser.cc"
    break;

  case 349: // squals: squals "," qual
#line 1288 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4392 "parser.cc"
    break;

  case 350: // squals: qual
#line 1290 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4398 "parser.cc"
    break;

  case 351: // guardquals: guardquals1
#line 1300 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4404 "parser.cc"
    break;

  case 352: // guardquals1: guardquals1 "," qual
#line 1302 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4410 "parser.cc"
    break;

  case 353: // guardquals1: qual
#line 1303 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4416 "parser.cc"
    break;

  case 354: // altslist: "{" alts "}"
#line 1306 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4422 "parser.cc"
    break;

  case 355: // altslist: "vocurly" alts close
#line 1307 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4428 "parser.cc"
    break;

  case 356: // altslist: "{" "}"
#line 1308 "parser.y"
                                 {}
#line 4434 "parser.cc"
    break;

  case 357: // altslist: "vocurly" close
#line 1309 "parser.y"
                                 {}
#line 4440 "parser.cc"
    break;

  case 358: // alts: alts1
#line 1311 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4446 "parser.cc"
    break;

  case 359: // alts: ";" alts
#line 1312 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4452 "parser.cc"
    break;

  case 360: // alts1: alts1 ";" alt
#line 1314 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4458 "parser.cc"
    break;

  case 361: // alts1: alts1 ";"
#line 1315 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4464 "parser.cc"
    break;

  case 362: // alts1: alt
#line 1316 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4470 "parser.cc"
    break;

  case 363: // alt: pat alt_rhs
#line 1318 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4476 "parser.cc"
    break;

  case 364: // alt_rhs: "->" exp wherebinds
#line 1320 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4482 "parser.cc"
    break;

  case 365: // alt_rhs: gdpats wherebinds
#line 1321 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4488 "parser.cc"
    break;

  case 366: // gdpats: gdpats gdpat
#line 1323 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4494 "parser.cc"
    break;

  case 367: // gdpats: gdpat
#line 1324 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4500 "parser.cc"
    break;

  case 368: // gdpat: "|" guardquals "->" exp
#line 1333 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4506 "parser.cc"
    break;

  case 369: // pat: exp
#line 1335 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4512 "parser.cc"
    break;

  case 370: // bindpat: exp
#line 1337 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4518 "parser.cc"
    break;

  case 371: // apat: aexp
#line 1339 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4524 "parser.cc"
    break;

  case 372: // apats1: apats1 apat
#line 1341 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4530 "parser.cc"
    break;

  case 373: // apats1: apat
#line 1342 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4536 "parser.cc"
    break;

  case 374: // stmtlist: "{" stmts "}"
#line 1345 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4542 "parser.cc"
    break;

  case 375: // stmtlist: "vocurly" stmts close
#line 1346 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4548 "parser.cc"
    break;

  case 376: // stmts: stmts ";" stmt
#line 1348 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4554 "parser.cc"
    break;

  case 377: // stmts: stmts ";"
#line 1349 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4560 "parser.cc"
    break;

  case 378: // stmts: stmt
#line 1350 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4566 "parser.cc"
    break;

  case 379: // stmts: %empty
#line 1351 "parser.y"
                       {}
#line 4572 "parser.cc"
    break;

  case 380: // stmt: qual
#line 1356 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4578 "parser.cc"
    break;

  case 381: // stmt: "rec" stmtlist
#line 1357 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4584 "parser.cc"
    break;

  case 382: // qual: bindpat "<-" exp
#line 1359 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4590 "parser.cc"
    break;

  case 383: // qual: exp
#line 1360 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4596 "parser.cc"
    break;

  case 384: // qual: "let" binds
#line 1361 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4602 "parser.cc"
    break;

  case 385: // fbinds: fbinds1
#line 1366 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4608 "parser.cc"
    break;

  case 386: // fbinds: %empty
#line 1367 "parser.y"
                        {}
#line 4614 "parser.cc"
    break;

  case 387: // fbinds1: fbind "," fbinds1
#line 1369 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4620 "parser.cc"
    break;

  case 388: // fbinds1: fbind
#line 1370 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4626 "parser.cc"
    break;

  case 389: // fbinds1: ".."
#line 1371 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4632 "parser.cc"
    break;

  case 390: // fbind: qvar "=" texp
#line 1373 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4638 "parser.cc"
    break;

  case 391: // fbind: qvar
#line 1374 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4644 "parser.cc"
    break;

  case 392: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1375 "parser.y"
                                                      {}
#line 4650 "parser.cc"
    break;

  case 393: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1376 "parser.y"
                                                      {}
#line 4656 "parser.cc"
    break;

  case 396: // qcon: gen_qcon
#line 1412 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4662 "parser.cc"
    break;

  case 397: // qcon: sysdcon
#line 1413 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4668 "parser.cc"
    break;

  case 398: // gen_qcon: qconid
#line 1415 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4674 "parser.cc"
    break;

  case 399: // gen_qcon: "(" qconsym ")"
#line 1416 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4680 "parser.cc"
    break;

  case 400: // con: conid
#line 1418 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4686 "parser.cc"
    break;

  case 401: // con: "(" consym ")"
#line 1419 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4692 "parser.cc"
    break;

  case 402: // con: sysdcon
#line 1420 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4698 "parser.cc"
    break;

  case 403: // con_list: con_list "," con
#line 1422 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4704 "parser.cc"
    break;

  case 404: // con_list: con
#line 1423 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4710 "parser.cc"
    break;

  case 405: // sysdcon_no_list: "(" ")"
#line 1425 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4716 "parser.cc"
    break;

  case 406: // sysdcon_no_list: "(" commas ")"
#line 1426 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4722 "parser.cc"
    break;

  case 407: // sysdcon_no_list: "(#" "#)"
#line 1427 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4728 "parser.cc"
    break;

  case 408: // sysdcon_no_list: "(#" commas "#)"
#line 1428 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4734 "parser.cc"
    break;

  case 409: // sysdcon: sysdcon_no_list
#line 1430 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4740 "parser.cc"
    break;

  case 410: // sysdcon: "[" "]"
#line 1431 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4746 "parser.cc"
    break;

  case 411: // conop: consym
#line 1433 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4752 "parser.cc"
    break;

  case 412: // conop: "`" conid "`"
#line 1434 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4758 "parser.cc"
    break;

  case 413: // qconop: qconsym
#line 1436 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4764 "parser.cc"
    break;

  case 414: // qconop: "`" qconid "`"
#line 1437 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4770 "parser.cc"
    break;

  case 415: // gtycon: ntgtycon
#line 1440 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4776 "parser.cc"
    break;

  case 416: // gtycon: "(" ")"
#line 1441 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4782 "parser.cc"
    break;

  case 417: // gtycon: "(#" "#)"
#line 1442 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4788 "parser.cc"
    break;

  case 418: // ntgtycon: oqtycon
#line 1444 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4794 "parser.cc"
    break;

  case 419: // ntgtycon: "(" commas ")"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4800 "parser.cc"
    break;

  case 420: // ntgtycon: "(#" commas "#)"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4806 "parser.cc"
    break;

  case 421: // ntgtycon: "(" "->" ")"
#line 1447 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4812 "parser.cc"
    break;

  case 422: // ntgtycon: "[" "]"
#line 1448 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4818 "parser.cc"
    break;

  case 423: // oqtycon: qtycon
#line 1450 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4824 "parser.cc"
    break;

  case 424: // oqtycon: "(" qtyconsym ")"
#line 1451 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4830 "parser.cc"
    break;

  case 425: // oqtycon_no_varcon: qtycon
#line 1453 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4836 "parser.cc"
    break;

  case 426: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1454 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4842 "parser.cc"
    break;

  case 427: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1455 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4848 "parser.cc"
    break;

  case 428: // oqtycon_no_varcon: "(" ":" ")"
#line 1456 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4854 "parser.cc"
    break;

  case 429: // qtyconop: qtyconsym
#line 1459 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4860 "parser.cc"
    break;

  case 430: // qtyconop: "`" qtycon "`"
#line 1460 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4866 "parser.cc"
    break;

  case 431: // qtycondoc: qtycon
#line 1462 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4872 "parser.cc"
    break;

  case 432: // qtycon: "QCONID"
#line 1464 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4878 "parser.cc"
    break;

  case 433: // qtycon: tycon
#line 1465 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4884 "parser.cc"
    break;

  case 434: // tycon: "CONID"
#line 1469 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4890 "parser.cc"
    break;

  case 435: // qtyconsym: "QCONSYM"
#line 1471 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4896 "parser.cc"
    break;

  case 436: // qtyconsym: "QVARSYM"
#line 1472 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4902 "parser.cc"
    break;

  case 437: // qtyconsym: tyconsym
#line 1473 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4908 "parser.cc"
    break;

  case 438: // tyconsym: "CONSYM"
#line 1475 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4914 "parser.cc"
    break;

  case 439: // tyconsym: "VARSYM"
#line 1476 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4920 "parser.cc"
    break;

  case 440: // tyconsym: ":"
#line 1477 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4926 "parser.cc"
    break;

  case 441: // tyconsym: "-"
#line 1478 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4932 "parser.cc"
    break;

  case 442: // op: varop
#line 1483 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4938 "parser.cc"
    break;

  case 443: // op: conop
#line 1484 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4944 "parser.cc"
    break;

  case 444: // varop: varsym
#line 1486 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4950 "parser.cc"
    break;

  case 445: // varop: "`" varid "`"
#line 1487 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4956 "parser.cc"
    break;

  case 446: // qop: qvarop
#line 1489 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4962 "parser.cc"
    break;

  case 447: // qop: qconop
#line 1490 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4968 "parser.cc"
    break;

  case 448: // qopm: qvaropm
#line 1493 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4974 "parser.cc"
    break;

  case 449: // qopm: qconop
#line 1494 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4980 "parser.cc"
    break;

  case 450: // qvarop: qvarsym
#line 1499 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4986 "parser.cc"
    break;

  case 451: // qvarop: "`" qvarid "`"
#line 1500 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4992 "parser.cc"
    break;

  case 452: // qvaropm: qvarsym_no_minus
#line 1502 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4998 "parser.cc"
    break;

  case 453: // qvaropm: "`" qvarid "`"
#line 1503 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5004 "parser.cc"
    break;

  case 454: // tyvar: tyvarid
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5010 "parser.cc"
    break;

  case 455: // tyvarop: "`" tyvarid "`"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5016 "parser.cc"
    break;

  case 456: // tyvarid: "VARID"
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5022 "parser.cc"
    break;

  case 457: // tyvarid: special_id
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5028 "parser.cc"
    break;

  case 458: // tyvarid: "unsafe"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5034 "parser.cc"
    break;

  case 459: // tyvarid: "safe"
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5040 "parser.cc"
    break;

  case 460: // tyvarid: "interruptible"
#line 1515 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5046 "parser.cc"
    break;

  case 461: // var: varid
#line 1518 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5052 "parser.cc"
    break;

  case 462: // var: "(" varsym ")"
#line 1519 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5058 "parser.cc"
    break;

  case 463: // qvar: qvarid
#line 1521 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5064 "parser.cc"
    break;

  case 464: // qvar: "(" varsym ")"
#line 1522 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5070 "parser.cc"
    break;

  case 465: // qvar: "(" qvarsym1 ")"
#line 1523 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5076 "parser.cc"
    break;

  case 466: // field: varid
#line 1525 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5082 "parser.cc"
    break;

  case 467: // qvarid: varid
#line 1527 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5088 "parser.cc"
    break;

  case 468: // qvarid: "QVARID"
#line 1528 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5094 "parser.cc"
    break;

  case 469: // varid: "VARID"
#line 1530 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5100 "parser.cc"
    break;

  case 470: // varid: special_id
#line 1531 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5106 "parser.cc"
    break;

  case 471: // varid: "unsafe"
#line 1532 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5112 "parser.cc"
    break;

  case 472: // varid: "safe"
#line 1533 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5118 "parser.cc"
    break;

  case 473: // varid: "interruptible"
#line 1534 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5124 "parser.cc"
    break;

  case 474: // varid: "forall"
#line 1535 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5130 "parser.cc"
    break;

  case 475: // varid: "family"
#line 1536 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5136 "parser.cc"
    break;

  case 476: // varid: "role"
#line 1537 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5142 "parser.cc"
    break;

  case 477: // qvarsym: varsym
#line 1539 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5148 "parser.cc"
    break;

  case 478: // qvarsym: qvarsym1
#line 1540 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5154 "parser.cc"
    break;

  case 479: // qvarsym_no_minus: varsym_no_minus
#line 1542 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5160 "parser.cc"
    break;

  case 480: // qvarsym_no_minus: qvarsym1
#line 1543 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5166 "parser.cc"
    break;

  case 481: // qvarsym1: "QVARSYM"
#line 1545 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5172 "parser.cc"
    break;

  case 482: // varsym: varsym_no_minus
#line 1547 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5178 "parser.cc"
    break;

  case 483: // varsym: "-"
#line 1548 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5184 "parser.cc"
    break;

  case 484: // varsym_no_minus: "VARSYM"
#line 1550 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5190 "parser.cc"
    break;

  case 485: // varsym_no_minus: special_sym
#line 1551 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5196 "parser.cc"
    break;

  case 486: // special_id: "as"
#line 1553 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5202 "parser.cc"
    break;

  case 487: // special_id: "qualified"
#line 1554 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5208 "parser.cc"
    break;

  case 488: // special_id: "hiding"
#line 1555 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5214 "parser.cc"
    break;

  case 489: // special_id: "export"
#line 1556 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5220 "parser.cc"
    break;

  case 490: // special_id: "label"
#line 1557 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5226 "parser.cc"
    break;

  case 491: // special_id: "dynamic"
#line 1558 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5232 "parser.cc"
    break;

  case 492: // special_id: "stdcall"
#line 1559 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5238 "parser.cc"
    break;

  case 493: // special_id: "ccall"
#line 1560 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5244 "parser.cc"
    break;

  case 494: // special_id: "capi"
#line 1561 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5250 "parser.cc"
    break;

  case 495: // special_id: "prim"
#line 1562 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5256 "parser.cc"
    break;

  case 496: // special_id: "javascript"
#line 1563 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5262 "parser.cc"
    break;

  case 497: // special_id: "group"
#line 1564 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5268 "parser.cc"
    break;

  case 498: // special_id: "stock"
#line 1565 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5274 "parser.cc"
    break;

  case 499: // special_id: "anyclass"
#line 1566 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5280 "parser.cc"
    break;

  case 500: // special_id: "via"
#line 1567 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5286 "parser.cc"
    break;

  case 501: // special_id: "unit"
#line 1568 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5292 "parser.cc"
    break;

  case 502: // special_id: "dependency"
#line 1569 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5298 "parser.cc"
    break;

  case 503: // special_id: "signature"
#line 1570 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5304 "parser.cc"
    break;

  case 504: // special_sym: "."
#line 1572 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5310 "parser.cc"
    break;

  case 505: // special_sym: "*"
#line 1573 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5316 "parser.cc"
    break;

  case 506: // qconid: conid
#line 1577 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5322 "parser.cc"
    break;

  case 507: // qconid: "QCONID"
#line 1578 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5328 "parser.cc"
    break;

  case 508: // conid: "CONID"
#line 1580 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5334 "parser.cc"
    break;

  case 509: // qconsym: consym
#line 1582 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5340 "parser.cc"
    break;

  case 510: // qconsym: "QCONSYM"
#line 1583 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5346 "parser.cc"
    break;

  case 511: // consym: "CONSYM"
#line 1585 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5352 "parser.cc"
    break;

  case 512: // consym: ":"
#line 1586 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5358 "parser.cc"
    break;

  case 513: // literal: "CHAR"
#line 1590 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5364 "parser.cc"
    break;

  case 514: // literal: "STRING"
#line 1591 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5370 "parser.cc"
    break;

  case 515: // literal: "INTEGER"
#line 1592 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5376 "parser.cc"
    break;

  case 516: // literal: "RATIONAL"
#line 1593 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5382 "parser.cc"
    break;

  case 517: // literal: "PRIMINTEGER"
#line 1594 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5388 "parser.cc"
    break;

  case 519: // close: error
#line 1602 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5394 "parser.cc"
    break;

  case 520: // modid: "CONID"
#line 1606 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5400 "parser.cc"
    break;

  case 521: // modid: "QCONID"
#line 1607 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5406 "parser.cc"
    break;

  case 522: // commas: commas ","
#line 1609 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5412 "parser.cc"
    break;

  case 523: // commas: ","
#line 1610 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5418 "parser.cc"
    break;


#line 5422 "parser.cc"

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


  const short parser::yypact_ninf_ = -711;

  const short parser::yytable_ninf_ = -483;

  const short
  parser::yypact_[] =
  {
      46,   -17,  -711,   102,  -711,  -711,  -711,  -711,  -711,   342,
      15,    48,  -711,    52,   -22,   -22,    89,  -711,  -711,  -711,
    -711,   168,  -711,  -711,  -711,    95,  -711,   161,   178,  1459,
     244,   258,   169,  -711,   906,  -711,   -21,  -711,  -711,  -711,
     -17,  -711,   -17,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,  -711,   887,  -711,  -711,  -711,  -711,
    -711,   207,    87,  -711,   203,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,   238,  -711,   -17,  -711,   246,  -711,  2512,  4555,
     336,   242,   311,  2512,  -711,  -711,  -711,   370,   316,  -711,
    3620,   372,   311,  3192,   351,  5002,   226,  3192,  3192,  2784,
    3192,  1832,  1696,   266,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,    40,   351,   299,   169,  -711,  -711,  -711,  -711,    72,
      28,  -711,  -711,   678,  -711,  2920,  -711,   202,  -711,  -711,
    -711,  -711,  -711,  -711,   376,   101,  -711,  -711,  -711,  -711,
     334,  -711,  -711,   357,  -711,  -711,  -711,  -711,   361,  -711,
     369,   378,   390,  -711,  -711,  -711,  4660,  -711,  4697,  -711,
    -711,  -711,  -711,   481,  -711,  1696,   465,   275,  -711,  -711,
    -711,  4555,  4555,  -711,  5101,  3721,  3304,   387,  -711,   436,
     437,  -711,   411,  -711,  3908,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  4555,   414,  -711,  2240,  2240,  -711,   416,   455,
     456,   460,   464,  4009,  1326,  1326,  -711,   527,  4555,  4555,
     113,   467,   706,   123,   505,  -711,  -711,   -12,  5002,  -711,
     325,     5,   441,     2,  -711,   130,  -711,  -711,  -711,  -711,
    3056,  -711,  2920,  -711,  -711,  -711,   848,  -711,  -711,  -711,
     275,    17,   442,   434,  -711,  2512,  -711,  -711,  -711,  -711,
    -711,  -711,  5187,  -711,  -711,   126,    99,   128,   378,   447,
     449,   451,   140,  -711,   279,  4009,  5002,  5002,  -711,   264,
     246,   483,   433,  4555,  4009,  5101,  2512,  2648,   848,  -711,
      39,  -711,  -711,  2512,  -711,  -711,  -711,  -711,  4555,  -711,
    5187,  4939,  3192,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
     454,   457,   450,  -711,   463,    52,   -17,    43,   362,  4009,
    -711,  -711,   383,   139,   470,   453,  -711,  -711,  -711,  -711,
     473,   506,   496,  -711,  -711,   475,  -711,  -711,  -711,  -711,
    -711,  -711,   480,   472,   482,  -711,   263,   290,  -711,   570,
    4555,  4009,  4902,  4555,  -711,  -711,  -711,  4555,  -711,  -711,
     519,  4009,   316,   311,   516,   520,   108,  -711,  -711,    49,
    -711,   585,  -711,  -711,  -711,  -711,  -711,  -711,   584,   173,
    -711,  -711,   678,    51,  2512,  -711,   534,   346,  4009,   224,
    4009,   485,   487,   515,   548,  -711,   551,   518,   167,   226,
     556,  2512,  -711,   514,   525,  2512,  2512,  2648,  1968,  -711,
    1968,   478,  -711,  -711,  5187,  -711,  -711,  1968,  -711,  1968,
     155,  -711,  -711,  -711,  -711,   558,   568,   571,  5048,   539,
    -711,  -711,  -711,  -711,  -711,  4110,     6,   375,  -711,  -711,
    -711,  -711,   626,   581,   549,  -711,   550,   316,  -711,  -711,
    -711,  -711,  -711,  -711,   566,  -711,   559,   602,   586,   589,
    -711,  -711,  -711,  4838,  -711,  -711,  -711,   579,  1496,  -711,
    -711,  2104,  1560,  -711,  -711,   583,  4009,  -711,  5101,  5240,
    -711,  4009,  4009,  -711,  -711,  4009,  -711,  -711,  -711,   575,
    -711,  5382,   367,  -711,  -711,  -711,   572,   580,   573,  -711,
    4009,  -711,  -711,   591,   590,   527,  -711,  2512,  -711,  2240,
    -711,  2512,   394,  -711,  -711,  1326,  -711,  -711,  4009,  4009,
    5349,   624,  -711,  -711,   484,  -711,  -711,  5101,   603,  -711,
    -711,  -711,  -711,   604,   730,   295,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,   598,  -711,   641,  -711,  -711,  -711,
    -711,  -711,  -711,  4009,  4009,   607,   611,   264,  -711,   375,
     633,  -711,  -711,   657,  4009,   707,   709,   729,  -711,  2512,
    2648,  -711,  -711,  -711,  4939,  1968,  5187,  -711,  1496,   -17,
    -711,   203,   628,    92,  -711,  -711,  2376,  -711,   634,   627,
    -711,   415,    52,  -711,  -711,  -711,  -711,  4009,  5475,  5475,
    -711,  -711,  -711,  -711,  -711,   635,  -711,  -711,  -711,  1190,
    1190,  -711,  -711,  -711,  -711,  -711,  4009,  -711,  -711,   416,
    1048,  1048,  -711,  -711,  -711,  -711,  -711,  5475,   720,   669,
    -711,  -711,  -711,  2648,  2512,  -711,    41,    80,  -711,  -711,
    -711,  5154,   709,   729,  4555,  -711,  -711,  -711,   672,  -711,
    4555,   413,   729,   165,  -711,   729,  -711,  -711,  -711,  -711,
    -711,     8,  -711,   647,  -711,  -711,  -711,  4801,  -711,  -711,
    -711,  2512,  2648,  2512,  -711,    47,  -711,  -711,  -711,    20,
     683,  -711,  -711,  5475,   728,  3822,  -711,  -711,   217,  -711,
      57,  -711,   755,  -711,   750,  -711,   750,  -711,   240,  -711,
      67,  -711,   686,   419,  -711,  4009,  -711,  -711,  -711,  4009,
    -711,  4555,  4555,   729,  -711,  -711,  5273,   707,   682,  3410,
    -711,  -711,  -711,   416,   416,  -711,  -711,  -711,  -711,  4196,
     236,   722,  -711,  -711,  -711,  1968,  5187,  -711,  -711,  -711,
     691,   626,  -711,  -711,  4009,  -711,  4009,  -711,  4555,  4555,
    4555,  -711,   431,  -711,  1190,  -711,  2512,  -711,  4555,   483,
    -711,  1048,  -711,  5475,  4282,  4368,  -711,  -711,  -711,  -711,
     688,   573,  -711,  -711,  -711,  4555,   663,  -711,  4555,   256,
    -711,   226,    70,  -711,  -711,   661,   674,  -711,  4555,  -711,
    -711,  -711,  2512,  -711,   684,   675,   519,  -711,   435,  4009,
    4469,  -711,  -711,  -711,  -711,  4110,  -711,  5475,  -711,   689,
     257,  -711,    52,    84,  4555,  3515,  -711,  4555,  -711,   416,
     157,  -711,  4555,  -711,  -711,  -711,  -711,  -711,  -711,  5442,
    -711,  -711,  3304,   711,   712,   375,  -711,  -711,  -711,  4555,
    -711,  -711,  -711,  -711,  4009,  -711,   683,  5475,   709,   729,
    -711,  -711,  -711,   729,  -711,  -711
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   520,   521,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   519,   518,    12,   185,   181,     0,     0,    20,
       0,    46,    41,    15,    14,   184,     0,     6,     7,   486,
       0,   488,     0,   487,   474,   489,   490,   491,   472,   473,
     471,   475,   476,   492,   493,   494,   495,   496,   497,   498,
     499,   500,   501,   503,   502,     0,   469,   434,   468,   432,
      22,     0,    19,    24,    28,    36,   425,   433,    35,   463,
     467,   470,     0,    45,     0,    38,    42,   332,     0,     0,
     129,   131,     0,     0,    63,    64,    65,    96,     0,   130,
       0,     0,     0,     0,   296,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   508,   507,   513,   514,   515,   516,
     517,   296,   296,    61,    68,    71,    72,    73,    74,   151,
       0,    77,   279,    78,   302,   305,   311,   321,   324,   326,
     396,   409,   397,   191,   325,   467,   398,   506,   327,   182,
       0,    27,    26,     0,   483,   505,   504,   484,     0,   481,
       0,     0,     0,   482,   485,    17,     0,    21,    31,    25,
      40,    40,     3,    48,    37,     0,     0,   301,   459,   460,
     458,     0,     0,   219,   264,     0,     0,     0,   456,   241,
       0,   144,   202,   205,   206,   210,   217,   418,   423,   218,
     454,   457,     0,     0,   132,   379,   379,   319,   307,     0,
       0,     0,     0,     0,   175,   175,   178,     0,     0,     0,
       0,     0,   202,   418,     0,   320,   310,     0,     0,   297,
       0,     0,     0,     0,   404,   186,   402,   400,   371,   373,
       0,   313,   304,   314,   512,   410,     0,   511,   510,   335,
     301,   340,     0,   341,   449,     0,   448,   452,   480,   479,
     413,   509,     0,   405,   523,     0,     0,     0,   480,     0,
     479,   413,     0,   407,     0,     0,     0,     0,    62,     0,
      69,   151,     0,     0,     0,     0,     0,     0,     0,   280,
     180,   285,   447,     0,   446,   450,   478,   477,     0,   308,
       0,   386,     0,   183,   428,   427,   426,   465,   464,    23,
       0,     0,    32,    34,     0,     0,     0,    50,     0,     0,
     221,   220,     0,     0,     0,   265,   267,   461,   235,   422,
       0,   194,     0,   198,   440,     0,   441,   223,   439,   438,
     436,   435,   232,     0,     0,   437,     0,     0,   247,   161,
       0,     0,     0,     0,   214,   429,   215,     0,   211,   213,
     135,   231,     0,     0,   383,     0,     0,   378,   380,     0,
     306,     0,    93,    92,    94,    95,   227,   188,   171,     0,
     281,   174,     0,     0,     0,    89,     0,   137,     0,     0,
       0,     0,     0,     0,     0,   291,     0,     0,     0,     0,
       0,     0,   372,     0,     0,   336,   342,     0,     0,   331,
       0,   337,   334,   466,     0,   330,   328,     0,   329,     0,
     464,   399,   406,   522,   408,     0,     0,     0,     0,   288,
     443,    67,   442,   444,   411,     0,     0,   133,   287,   199,
     189,   190,   180,     0,   351,   353,     0,     0,   283,   284,
     303,   309,   323,   389,     0,   385,   388,   391,     0,   467,
     312,    30,    29,     0,     9,    10,    47,     0,    54,    44,
      49,     0,     0,   318,   300,     0,     0,   222,     0,     0,
     225,     0,     0,   421,   226,     0,   424,   419,   420,   242,
     244,     0,     0,    79,   143,   203,     0,     0,   207,   212,
       0,    84,   232,     0,   230,   384,   381,     0,   374,   377,
     375,     0,     0,    88,   176,   173,   177,   316,     0,     0,
       0,   101,   248,    85,     0,    86,    80,     0,     0,   298,
     290,   292,   401,     0,     0,     0,   187,   415,   403,   289,
     315,   453,   414,   344,   346,   350,   335,   348,   347,   333,
     339,   338,   295,     0,     0,     0,     0,     0,   235,   133,
       0,   148,   150,     0,     0,   261,   251,   269,   282,     0,
       0,   451,   179,   322,     0,     0,     0,    33,    54,     0,
      56,    28,     0,    53,    58,   356,     0,   369,     0,   358,
     362,     0,     0,   357,   462,   268,   266,     0,     0,     0,
     234,   236,   239,   195,   197,   233,   247,   247,   246,   157,
     157,   160,   430,   455,   136,    75,     0,   382,   376,   307,
     167,   167,   170,   172,   116,   138,   139,     0,   106,     0,
     299,   416,   417,     0,   343,   192,     0,     0,   445,   412,
      66,     0,   251,   269,     0,   149,   134,   235,   255,   257,
       0,     0,   269,     0,    81,   270,   272,   286,   352,   387,
     390,   393,   395,     0,    60,    59,    51,     0,    55,   359,
     354,   361,     0,     0,   363,   180,   367,   355,   196,     0,
       0,   224,   243,   245,   122,     0,   152,   156,     0,   153,
       0,   233,     0,   129,   124,   162,   124,   166,     0,   163,
       0,   102,     0,     0,    83,     0,   349,   345,   293,     0,
     294,     0,     0,   269,    90,   147,     0,   261,     0,   262,
     208,   216,   259,   307,   307,    82,    99,    97,    98,     0,
       0,   273,   276,   431,   271,     0,     0,    52,    57,   360,
       0,   180,   365,   366,     0,   237,     0,   123,     0,     0,
       0,   120,   140,   158,   155,   159,     0,   125,     0,   151,
     168,   165,   169,     0,   115,   115,   107,    76,   193,   146,
       0,   200,    91,   260,   256,     0,     0,   209,     0,     0,
     253,     0,     0,   277,   204,   228,     0,   274,     0,   275,
     392,   394,     0,   364,     0,     0,   135,   121,   140,     0,
       0,   118,   154,   317,   126,     0,   164,   103,   105,     0,
       0,   114,     0,     0,     0,   262,   258,   263,   249,   307,
       0,   250,     0,   278,   100,   368,   238,   240,   117,     0,
     119,   141,     0,     0,   218,   133,   104,   110,   108,   113,
     111,   109,   145,   252,     0,   229,   218,     0,   251,   269,
     112,   254,   142,   269,   127,   128
  };

  const short
  parser::yypgoto_[] =
  {
    -711,  -711,  -711,  -711,  -711,  -711,  -711,    44,  -711,  -711,
    -711,  -711,   631,   212,  -711,  -711,    -3,   676,  -711,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,   223,  -711,   143,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,   -36,  -711,  -711,  -711,    50,  -193,
    -711,  -711,   116,  -711,   779,  -711,  -531,    18,  -711,    21,
     535,    19,  -264,    68,   218,  -711,  -711,    66,   208,  -711,
    -711,   617,  -711,  -298,  -402,   818,  -711,  -711,  -297,   131,
    -160,   283,  -154,   -24,  -711,   -83,  -711,   -85,  -711,     3,
    -711,  -362,  -711,  -711,  -711,  -625,  -161,   563,    22,  -711,
     479,  -471,   319,  -710,  -711,  -711,   239,   234,  -420,  -598,
     122,    31,  -522,  -711,   134,  -711,    73,  -711,  -711,   379,
    -601,  -711,   198,   132,   822,  -185,  -711,  -711,   574,  -711,
     329,  -711,   235,    29,  -254,  -181,   752,   -62,  -711,  -711,
    -711,  -103,  -711,  -711,  -711,  -711,   191,  -711,  -711,  -407,
    -711,   194,  -711,  -711,   192,  -711,  -711,   630,  -711,   -66,
     660,   364,  -253,  -711,   300,  -711,  -711,  -711,  -711,   476,
      96,  -711,   -97,  -663,  -115,  -711,   486,   -57,  -711,  -711,
    -711,   117,  -711,  -187,  -711,   332,  -711,   643,  -711,  -711,
    -711,  -375,  -711,  -329,  -247,   204,  -243,  -174,   -31,  -711,
    -711,    -5,   -33,   -98,   -89,  -711,  -120,   -99,   -43,  -232,
    -711,  -299,   -30,  -111
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   172,     6,    10,    19,    30,
      71,    72,    73,   169,   311,   312,   581,    86,    11,    20,
      21,    32,    84,   317,   469,   470,   582,   583,   584,   279,
     123,   429,    33,    34,   124,   125,   126,   127,   220,   128,
     213,   730,   789,   628,   701,   807,   704,   766,   810,   811,
     686,   748,   758,   695,   696,   203,   566,   501,   521,   801,
     189,   559,   283,   687,   688,   611,   493,   697,   698,   622,
     513,   379,   216,   217,   448,    27,    36,   400,   376,   438,
     130,   636,   342,   522,   440,   332,   718,   333,   785,   192,
     193,   719,   194,   358,   353,   720,   195,   378,   786,   503,
     343,   479,   600,   601,   349,   489,   490,   491,   523,   652,
     779,   780,   567,   648,   649,   650,   722,   324,   325,   326,
     654,   655,   656,   731,   380,   689,   289,   290,   291,   132,
     228,   229,   249,   177,   134,   781,   135,   136,   137,   138,
     265,   266,   267,   252,   253,   544,   443,   444,   473,   588,
     589,   590,   674,   675,   676,   591,   365,   239,   240,   207,
     366,   367,   368,   454,   455,   456,   661,   139,   140,   234,
     235,   141,   142,   430,   254,   536,   196,   197,    75,   354,
     732,   198,    77,   344,   345,   431,   432,   293,   255,   294,
     256,   199,   356,   200,   143,   144,   458,    79,    80,   295,
     257,   258,   297,   163,    81,   164,   146,   147,   260,   261,
     148,    24,     9,   272
  };

  const short
  parser::yytable_[] =
  {
     201,   397,   274,   145,   191,   355,   190,   237,   251,   236,
     151,   201,   152,   259,   270,   221,   465,   435,   292,   412,
     320,   321,   474,   497,   323,   385,    74,   371,   642,   381,
     381,   330,   162,   359,   445,   355,   225,   643,   441,   450,
     568,   226,   714,   223,   713,   238,   241,   434,   243,    13,
      22,   725,    22,    22,   173,   467,   778,   452,    22,   275,
     161,   603,   292,   133,   505,   592,   447,     1,    22,   271,
     510,    22,   403,   299,   447,   346,   347,   270,   392,   269,
     614,   244,   395,   562,   516,    22,    25,   641,   149,   735,
     833,   281,   201,   201,   777,   406,   201,   201,   150,   625,
     744,   407,    12,   222,   602,   201,     7,   268,   284,   736,
       8,    26,   772,   201,   446,   263,   608,   360,   708,   833,
      17,   264,   393,   287,   201,   745,   404,   247,   296,   201,
     201,   672,   271,   386,   387,   292,   408,   451,   396,   563,
     250,   250,   162,   282,   646,   602,    76,   285,   227,   572,
       2,   450,   778,   327,   545,   468,    23,   710,    23,    23,
     709,   331,   331,    74,    23,   313,    18,   509,   404,   515,
     268,   549,   296,   593,    23,   754,   716,    23,   238,   669,
     299,  -461,    31,   145,   145,   761,   201,   726,   819,   377,
     777,    23,   777,   388,   201,   201,   499,   162,   191,   709,
     190,    29,   839,   -87,   250,   222,   166,   502,   204,   201,
     398,   667,   416,   508,   314,   315,   727,   728,   417,   476,
    -461,   222,   222,   679,   680,   161,   509,   414,    35,   167,
     201,   413,   389,    78,   668,  -462,   526,   844,    37,   415,
     460,   418,   -87,   382,   382,   296,   433,   419,   854,   399,
     853,   377,   855,   422,   327,    38,   635,   635,   285,   423,
     439,   201,   201,   201,   201,   494,   602,   292,   201,   413,
     459,    82,   201,   742,  -462,   533,   399,   729,   514,   534,
     629,   535,    83,    76,   411,    76,   466,    85,    67,   475,
      67,   515,    69,   677,    69,   377,   292,   506,   702,   201,
     237,   201,   236,   300,   848,   547,   301,   548,   608,   231,
     259,   355,   259,   849,   550,   168,   551,   658,   323,   259,
     165,   259,   753,   176,   794,   434,   795,   495,   208,   556,
     623,   605,   525,   662,   232,   754,   524,   331,   233,   793,
     113,   602,   170,   244,   171,   760,   201,    67,   729,   114,
     561,    69,   560,   222,   244,   319,   498,   154,   761,    67,
     155,   818,   838,    69,   174,    67,   331,   156,   154,    69,
      78,   155,    78,   202,   819,   839,   487,   296,   156,   831,
     706,   273,   423,   413,   428,   264,   224,   201,   157,   247,
     201,   755,   201,   201,   424,   288,   201,   555,   423,   157,
     247,   762,   201,   159,   248,   488,   296,    14,    15,   423,
     632,   201,   768,   815,   264,   205,   817,   206,   154,   445,
     214,   155,   215,   346,   347,   834,   519,   520,   156,   201,
     201,   201,   394,   278,   808,   699,   699,   250,   692,   250,
     364,   364,   209,   210,   211,   212,   250,   327,   250,   157,
     276,   277,   595,   159,   602,   564,   565,   846,   604,   227,
     577,   331,   691,   302,   201,   201,   471,   303,   472,   496,
     304,   609,   660,   610,   305,   201,   154,   259,   836,   155,
     426,   427,   306,   821,   145,   316,   156,   434,   318,   721,
     334,   307,   751,   791,   624,   805,   327,   351,   620,   672,
     621,   673,  -200,   308,   336,   457,   264,   157,   201,   201,
     201,   799,   800,   840,   841,   799,   829,   723,   702,   724,
     348,   442,   364,   764,   433,   765,   361,   201,   350,   377,
     377,   352,   372,   373,   370,   338,   339,   374,   201,   340,
     341,   375,   384,   459,   382,   413,   391,   851,   390,   664,
     245,   409,   201,   410,   282,   201,   797,   244,   721,   715,
     420,   201,  -482,   334,   421,   804,   436,   461,   464,   463,
     462,   154,   478,   678,   155,   477,   699,   336,   145,   145,
      76,   156,   480,   434,   355,    76,   481,   482,   483,   145,
     145,   485,   331,   484,   201,   486,   201,   492,   288,   500,
     752,  -370,   157,   247,   250,   507,   159,   248,   338,   339,
     511,   512,   340,   341,   721,   518,   201,   721,   527,   517,
     201,   528,   201,   201,   529,   530,   769,   201,   531,   770,
     201,   532,   790,   539,   541,   552,   540,   259,   382,   382,
     201,   543,   364,   546,   784,   542,   850,   222,   553,   382,
     382,   554,   334,   447,   721,   201,   721,   201,   557,   201,
     201,   201,   569,   796,   386,   798,   336,    78,   570,   201,
     571,   573,    78,   386,   201,   201,   201,   556,   574,   386,
     386,   439,   237,   575,   236,   377,   201,   576,   222,   201,
    -466,   578,   612,   352,   606,    76,   594,   338,   339,   201,
     613,   340,   341,   824,   615,   413,   587,   587,   627,   616,
     201,   201,   630,   329,   222,   771,   201,   633,   201,   634,
     561,   346,   560,   145,   644,   201,   201,   638,   201,   842,
     145,   639,   222,   201,   645,   647,   651,   784,   653,   670,
     201,   666,   617,   201,   364,   671,   619,   703,   681,   705,
     201,   222,   222,   222,   386,   201,   717,   244,   201,   286,
     737,   222,   287,   746,   250,   747,   756,   222,   222,   757,
     733,   154,   763,   775,   155,   788,   624,   792,   457,   814,
     822,   156,    78,   382,    76,   334,   114,   823,   827,   826,
     382,   222,   351,   665,   837,   847,  -239,   309,   288,   336,
     280,   663,   157,   247,   657,   364,   159,   248,   331,   334,
     738,   852,   759,   129,   828,   813,   335,   222,   437,   830,
     377,   587,   802,   336,   835,   222,   352,   806,   690,   700,
     338,   339,   383,    28,   340,   341,   767,   637,   425,   626,
     504,   683,   222,   631,   845,   682,   782,   733,   816,   264,
     843,   774,    39,   734,   338,   339,   131,   596,   340,   341,
      41,   242,   787,   740,   449,   739,   369,   743,   364,   707,
     402,    78,    43,   618,   659,   538,    44,   820,    45,    46,
      47,    48,    49,    50,   537,    51,    52,    53,    54,   640,
      55,    56,    57,   405,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,   587,   364,   741,    87,
      39,    88,    89,    90,    91,     0,    92,     0,    41,    93,
       0,     0,    94,    95,    96,    97,    98,     0,    99,     0,
      43,     0,   100,     0,    44,   101,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,   104,     0,     0,   153,     0,     0,   105,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     154,     0,   106,   155,     0,     0,     0,     0,   107,     0,
     156,   803,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   157,   158,     0,   111,   159,   160,     0,   112,     0,
     113,     0,     0,     0,     0,     0,     0,   825,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,     0,   120,     0,     0,     0,     0,   121,
     122,    87,    39,    88,     0,   693,     0,     0,    92,     0,
      41,    93,     0,     0,    94,    95,    96,     0,    98,     0,
      99,     0,    43,     0,   694,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,   104,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     112,     0,   113,     0,     0,     0,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,     0,     0,     0,
       0,   121,   122,    87,    39,    88,     0,   684,     0,     0,
      92,     0,    41,    93,     0,     0,    94,    95,    96,     0,
      98,     0,     0,     0,    43,     0,   685,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,   104,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   112,     0,   113,     0,     0,     0,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,   121,   122,     0,    92,     0,    41,    93,
       0,     0,    94,    95,    96,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,   104,     0,     0,     0,     0,     0,   105,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   112,     0,
     113,     0,     0,     0,     0,     0,     0,     0,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,    39,   120,     0,     0,    40,     0,   121,
     122,    41,     0,     0,     0,     0,     0,     0,     0,     0,
      42,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,     0,    41,     0,
      59,    60,    61,    62,    63,    64,     0,   579,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,    22,     0,    87,    39,    88,     0,     0,     0,     0,
      92,    65,    41,    93,     0,     0,     0,     0,     0,     0,
      98,    66,    67,     0,    43,    68,    69,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    70,    55,    56,    57,     0,     0,    58,    65,     0,
     103,    59,    60,    61,    62,    63,    64,     0,    66,    67,
       0,     0,    68,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   580,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    23,   111,     0,
       0,     0,   175,     0,   113,     0,     0,     0,   586,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   244,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,   154,
     110,     0,   155,     0,     0,     0,     0,     0,   262,   156,
       0,     0,     0,     0,   111,     0,     0,     0,   175,   263,
     113,     0,     0,     0,     0,   264,   246,     0,    66,   114,
     157,   247,    68,   115,   159,   248,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   244,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,   155,     0,
       0,     0,     0,     0,     0,   156,     0,     0,     0,     0,
     111,   245,     0,     0,   175,     0,   113,     0,     0,     0,
       0,     0,   246,     0,    66,   114,   157,   247,    68,   115,
     159,   248,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   244,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,   155,     0,     0,     0,     0,     0,
       0,   156,     0,     0,     0,     0,   111,     0,     0,     0,
     175,     0,   113,     0,     0,     0,     0,     0,   246,     0,
      66,   114,   157,   247,    68,   115,   159,   248,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   585,
       0,     0,   111,     0,     0,     0,   175,     0,   113,     0,
       0,     0,   586,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
     362,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,   363,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   175,     0,   113,     0,     0,     0,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   175,     0,
     113,     0,     0,     0,   586,     0,     0,     0,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   175,     0,   113,     0,     0,     0,
       0,     0,     0,     0,    66,   114,     0,     0,    68,   115,
       0,     0,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     175,     0,   113,     0,     0,     0,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   175,     0,   113,     0,
       0,     0,     0,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,   298,   108,
       0,     0,     0,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   175,     0,   113,     0,     0,     0,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,   401,     0,     0,   108,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   175,     0,
     113,     0,     0,     0,     0,     0,     0,     0,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   175,     0,   113,     0,    39,     0,
       0,     0,     0,     0,    66,   114,    41,     0,    68,   115,
       0,     0,     0,     0,   116,   117,   118,   119,    43,     0,
     120,     0,   328,     0,    45,    46,    47,   178,   179,   180,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   334,     0,     0,     0,     0,     0,     0,
     335,     0,     0,   181,     0,     0,     0,   336,   182,     0,
     183,     0,     0,     0,     0,     0,     0,     0,   184,     0,
       0,     0,   185,     0,    39,     0,   186,   337,   187,     0,
       0,     0,    41,   264,     0,     0,   188,    67,   338,   339,
       0,    69,   340,   341,    43,     0,     0,     0,     0,     0,
      45,    46,    47,   178,   179,   180,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   244,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,  -201,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,     0,   185,    39,
       0,     0,   186,     0,   187,     0,     0,    41,     0,     0,
     776,     0,   188,    67,     0,   247,     0,    69,     0,    43,
       0,     0,     0,     0,     0,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   244,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,   182,
       0,   183,     0,     0,     0,     0,     0,     0,     0,   184,
       0,     0,     0,   185,    39,     0,     0,   186,     0,   187,
       0,     0,    41,     0,     0,   776,     0,   188,    67,   218,
     247,     0,    69,     0,    43,     0,     0,     0,     0,     0,
      45,    46,    47,   178,   179,   180,     0,   219,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,    39,     0,     0,   185,     0,
       0,     0,   186,    41,   187,     0,     0,     0,     0,     0,
       0,     0,   188,    67,     0,    43,     0,    69,     0,   328,
       0,    45,    46,    47,   178,   179,   180,     0,     0,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,   182,     0,   183,     0,     0,
       0,     0,     0,     0,     0,   184,    39,     0,     0,   185,
     329,     0,     0,   186,    41,   187,     0,     0,     0,     0,
       0,   749,     0,   188,    67,     0,    43,     0,    69,     0,
       0,     0,    45,    46,    47,   178,   179,   180,     0,   750,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,    39,     0,     0,     0,   182,     0,   183,     0,
      41,     0,     0,     0,     0,     0,   184,     0,     0,     0,
     185,     0,    43,     0,   186,     0,   187,     0,    45,    46,
      47,   178,   179,   180,   188,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   357,   181,     0,     0,
       0,     0,   182,     0,   183,     0,     0,     0,     0,     0,
       0,     0,   184,    39,     0,     0,   185,     0,     0,     0,
     186,    41,   187,     0,     0,     0,     0,     0,     0,     0,
     188,    67,     0,    43,     0,    69,     0,   328,     0,    45,
      46,    47,   178,   179,   180,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
       0,     0,     0,   182,     0,   183,     0,     0,     0,     0,
       0,     0,     0,   184,    39,     0,     0,   185,     0,     0,
       0,   186,    41,   187,     0,     0,     0,     0,     0,     0,
       0,   188,    67,     0,    43,     0,    69,     0,   558,     0,
      45,    46,    47,   178,   179,   180,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
      39,     0,     0,     0,   182,     0,   183,     0,    41,     0,
       0,     0,     0,     0,   184,     0,     0,     0,   185,     0,
      43,     0,   186,     0,   187,     0,    45,    46,    47,   178,
     179,   180,   188,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,    39,     0,     0,     0,
     182,     0,   183,     0,    41,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,     0,    43,     0,   186,   783,
     187,     0,    45,    46,    47,   178,   179,   180,   188,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     809,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   181,    39,     0,     0,     0,   182,     0,   183,     0,
      41,     0,     0,     0,     0,     0,   184,     0,     0,     0,
     185,     0,    43,     0,   186,     0,   187,     0,    45,    46,
      47,   178,   179,   180,   188,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   812,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   181,     0,     0,
       0,     0,   182,     0,   183,     0,     0,     0,     0,     0,
       0,     0,   184,    39,     0,     0,   185,     0,     0,     0,
     186,    41,   187,     0,     0,     0,     0,     0,     0,     0,
     188,    67,     0,    43,     0,    69,     0,   328,     0,    45,
      46,    47,   178,   179,   180,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,    39,
       0,     0,     0,   182,     0,   183,     0,    41,     0,     0,
       0,     0,     0,   184,     0,     0,     0,   185,     0,    43,
       0,   832,     0,   187,     0,    45,    46,    47,   178,   179,
     180,   188,    67,     0,    53,    54,    69,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,   182,
       0,   183,     0,     0,     0,     0,     0,     0,     0,   184,
       0,     0,     0,   185,    39,     0,     0,   186,    40,   187,
       0,     0,    41,     0,     0,     0,     0,   188,    67,     0,
       0,    42,    69,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,     0,    41,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,     0,     0,   310,     0,     0,     0,     0,
       0,     0,    66,    67,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,    65,
       0,     0,     0,    41,     0,     0,     0,     0,     0,    66,
      67,     0,   579,    68,    69,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    39,    55,    56,    57,     0,     0,    58,     0,
      41,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,    65,    41,     0,     0,     0,     0,     0,
       0,     0,     0,    66,    67,     0,    43,    68,    69,     0,
       0,     0,    45,    46,    47,   178,   179,   180,     0,     0,
       0,    53,    54,    39,    55,    56,    57,     0,     0,    58,
      65,    41,     0,    59,    60,    61,    62,    63,    64,     0,
      66,    67,     0,    43,    68,    69,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,     0,    41,     0,     0,   453,     0,     0,
       0,     0,     0,     0,   188,    67,    43,     0,     0,    69,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,   230,    39,    59,    60,    61,    62,    63,    64,     0,
      41,    66,     0,     0,     0,    68,     0,     0,     0,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,    39,     0,     0,     0,     0,
       0,     0,     0,    41,   230,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,    43,     0,     0,    68,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    39,     0,
       0,     0,     0,     0,     0,     0,    41,     0,     0,     0,
      66,   114,     0,     0,     0,     0,     0,     0,    43,     0,
       0,     0,     0,     0,    45,    46,    47,   178,   179,   180,
       0,    39,     0,    53,    54,     0,    55,    56,    57,    41,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,    43,     0,   322,     0,    44,     0,    45,    46,    47,
      48,    49,    50,    66,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,    39,   711,     0,     0,     0,     0,
       0,     0,    41,     0,     0,     0,     0,   712,   598,     0,
       0,     0,     0,     0,    43,     0,   599,     0,     0,     0,
      45,    46,    47,   178,   179,   180,   188,    39,     0,    53,
      54,     0,    55,    56,    57,    41,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,    43,     0,     0,
       0,     0,     0,    45,    46,    47,   178,   179,   180,    66,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   597,   598,     0,     0,     0,     0,     0,
       0,     0,   599,    39,     0,     0,     0,     0,     0,     0,
       0,    41,   188,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    43,     0,     0,   773,   598,     0,    45,
      46,    47,   178,   179,   180,   599,    39,     0,    53,    54,
       0,    55,    56,    57,    41,   188,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   178,   179,   180,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,   598,    41,     0,     0,     0,     0,     0,
       0,   599,     0,     0,     0,     0,    43,     0,   607,     0,
       0,   188,    45,    46,    47,   178,   179,   180,     0,    39,
       0,    53,    54,     0,    55,    56,    57,    41,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,    43,
       0,     0,     0,     0,   188,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   599,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188
  };

  const short
  parser::yycheck_[] =
  {
      89,   233,   113,    34,    89,   192,    89,   106,   111,   106,
      40,   100,    42,   111,   112,   100,   315,   281,   133,   262,
     181,   182,   319,   352,   184,   218,    29,   208,   559,   214,
     215,   185,    65,   194,   287,   222,   102,   559,   285,   293,
     442,   103,   643,   100,   642,   107,   108,   279,   110,     5,
       1,   652,     1,     1,    84,    12,   719,   300,     1,    19,
      65,   481,   177,    34,   362,   472,    27,    21,     1,   112,
     369,     1,   246,   135,    27,   186,   187,   175,    90,   112,
     500,    79,    77,    77,   383,     1,   108,   558,   109,    81,
     800,    19,   181,   182,   719,    78,   185,   186,   119,   519,
      80,    84,     0,   100,   479,   194,   123,   112,    80,   101,
     127,   133,   713,   202,   288,   113,   491,   202,    77,   829,
     105,   119,   134,    84,   213,   105,   246,   125,   133,   218,
     219,    84,   175,   218,   219,   250,   119,   298,   133,   133,
     111,   112,   175,    71,   564,   520,    29,   119,   108,   447,
     104,   405,   815,   184,   407,   112,   107,    77,   107,   107,
     119,   185,   186,   166,   107,   168,   118,   118,   288,   118,
     175,   414,   177,   472,   107,   118,   647,   107,   240,   586,
     242,    80,    14,   214,   215,   118,   275,    22,   118,   213,
     815,   107,   817,    80,   283,   284,   357,   230,   283,   119,
     283,   112,   118,    80,   175,   202,   119,   361,    91,   298,
      80,   119,   113,   105,   170,   171,    51,    52,   119,    80,
     119,   218,   219,   598,   599,   230,   118,   101,   133,   142,
     319,   262,   119,    29,   142,    80,   390,    80,    77,   113,
     302,   113,   119,   214,   215,   250,   279,   119,   849,   119,
     848,   275,   853,   113,   285,    77,   553,   554,   119,   119,
     284,   350,   351,   352,   353,   350,   641,   382,   357,   300,
     301,    27,   361,   675,   119,   108,   119,   112,   105,   112,
     527,   114,    24,   166,   255,   168,   316,   118,   123,   322,
     123,   118,   127,   592,   127,   319,   411,   363,   627,   388,
     399,   390,   399,   101,   835,   408,   104,   410,   683,   105,
     408,   498,   410,   835,   417,   112,   419,   570,   478,   417,
     113,   419,   105,    88,   744,   557,   746,   351,    93,   428,
     515,   485,   389,   576,   108,   118,   112,   361,   112,   741,
     114,   716,   104,    79,   106,   105,   435,   123,   112,   123,
     435,   127,   435,   350,    79,    80,   353,    93,   118,   123,
      96,   105,   105,   127,   118,   123,   390,   103,    93,   127,
     166,    96,   168,    37,   118,   118,   113,   382,   103,   799,
     633,   115,   119,   414,   120,   119,    14,   476,   124,   125,
     479,   690,   481,   482,   115,   120,   485,   428,   119,   124,
     125,   700,   491,   128,   129,   115,   411,    65,    66,   119,
     115,   500,   709,   775,   119,   104,   778,   106,    93,   672,
     104,    96,   106,   534,   535,   800,    80,    81,   103,   518,
     519,   520,   228,   134,   763,   620,   621,   408,   619,   410,
     205,   206,    72,    73,    74,    75,   417,   478,   419,   124,
     121,   122,   476,   128,   829,    80,    81,   832,   482,   108,
     463,   485,   616,    87,   553,   554,   104,   133,   106,   352,
     113,   104,   575,   106,   113,   564,    93,   575,   807,    96,
     276,   277,   113,   782,   515,     4,   103,   719,    23,   650,
      79,   113,   685,   736,   518,   759,   527,    86,   104,    84,
     106,    86,    91,   113,    93,   301,   119,   124,   597,   598,
     599,    80,    81,   812,   813,    80,    81,   104,   847,   106,
      84,   286,   287,   104,   557,   106,   112,   616,    91,   553,
     554,   120,    77,    77,   118,   124,   125,    77,   627,   128,
     129,    77,    15,   574,   515,   576,    41,   844,    81,   579,
     109,   109,   641,   119,    71,   644,   749,    79,   719,   644,
     113,   650,   113,    79,   113,   758,   133,   113,   105,   119,
     113,    93,   119,   597,    96,   105,   761,    93,   609,   610,
     463,   103,   109,   815,   771,   468,    80,    91,   113,   620,
     621,   119,   616,   113,   683,   113,   685,    27,   120,    80,
     685,    85,   124,   125,   575,    85,   128,   129,   124,   125,
      25,    27,   128,   129,   775,    81,   705,   778,   133,   384,
     709,   134,   711,   712,   109,    77,   711,   716,    77,   712,
     719,   113,   735,    77,   120,    77,   401,   735,   609,   610,
     729,   406,   407,   408,   729,   120,   839,   644,    80,   620,
     621,    80,    79,    27,   815,   744,   817,   746,   119,   748,
     749,   750,    81,   748,   749,   750,    93,   463,   119,   758,
     120,   105,   468,   758,   763,   764,   765,   776,   119,   764,
     765,   705,   781,    81,   781,   709,   775,   101,   685,   778,
     101,   112,   120,   120,   119,   578,   113,   124,   125,   788,
     120,   128,   129,   788,   113,   736,   471,   472,    84,   119,
     799,   800,   109,   109,   711,   712,   805,   119,   807,    78,
     805,   832,   805,   754,    91,   814,   815,   120,   817,   814,
     761,   120,   729,   822,    77,    28,    27,   822,     9,   105,
     829,   113,   507,   832,   509,   118,   511,    27,   113,    80,
     839,   748,   749,   750,   839,   844,    84,    79,   847,    81,
     113,   758,    84,    80,   735,    37,    11,   764,   765,    19,
     653,    93,    86,    91,    96,    53,   800,    86,   574,    91,
     119,   103,   578,   754,   667,    79,   123,   113,   113,   105,
     761,   788,    86,   581,   105,    84,    84,   166,   120,    93,
     124,   578,   124,   125,   569,   570,   128,   129,   832,    79,
     667,   847,   696,    34,   796,   765,    86,   814,   283,   798,
     844,   586,   754,    93,   805,   822,   120,   761,   610,   621,
     124,   125,   215,    15,   128,   129,   705,   554,   275,   520,
     361,   607,   839,   113,   822,   606,   724,   730,   775,   119,
     819,   717,     4,   655,   124,   125,    34,   478,   128,   129,
      12,   109,   730,   672,   290,   671,   206,   675,   633,   634,
     240,   667,    24,   509,   574,   399,    28,   781,    30,    31,
      32,    33,    34,    35,   398,    37,    38,    39,    40,   557,
      42,    43,    44,   250,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,   671,   672,   673,     3,
       4,     5,     6,     7,     8,    -1,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    19,    20,    -1,    22,    -1,
      24,    -1,    26,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    79,    -1,    -1,    63,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
      93,    -1,    76,    96,    -1,    -1,    -1,    -1,    82,    -1,
     103,   756,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,    -1,   108,   128,   129,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,   792,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,    -1,    -1,    -1,    -1,   143,
     144,     3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      22,    -1,    24,    -1,    26,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,    -1,    -1,    -1,
      -1,   143,   144,     3,     4,     5,    -1,     7,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    24,    -1,    26,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,     3,
       4,     5,    -1,   143,   144,    -1,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,     4,   138,    -1,    -1,     8,    -1,   143,
     144,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      21,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    21,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,   112,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,   122,   123,    -1,    24,   126,   127,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   142,    42,    43,    44,    -1,    -1,    47,   112,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,   118,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,   102,   103,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,   113,
     114,    -1,    -1,    -1,    -1,   119,   120,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    -1,    94,    -1,    96,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,
     108,   109,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,   120,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,   132,   133,   134,   135,    -1,    -1,
     138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,   120,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,   118,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    46,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,   118,    -1,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,
      -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,
     138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    89,
      -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    86,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,   112,    -1,   114,    -1,     4,    -1,
      -1,    -1,    -1,    -1,   122,   123,    12,    -1,   126,   127,
      -1,    -1,    -1,    -1,   132,   133,   134,   135,    24,    -1,
     138,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    -1,    89,    -1,    -1,    -1,    93,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,     4,    -1,   112,   113,   114,    -1,
      -1,    -1,    12,   119,    -1,    -1,   122,   123,   124,   125,
      -1,   127,   128,   129,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,     4,
      -1,    -1,   112,    -1,   114,    -1,    -1,    12,    -1,    -1,
     120,    -1,   122,   123,    -1,   125,    -1,   127,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,
      -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,     4,    -1,    -1,   112,    -1,   114,
      -1,    -1,    12,    -1,    -1,   120,    -1,   122,   123,    19,
     125,    -1,   127,    -1,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,    -1,
      -1,    -1,   112,    12,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    24,    -1,   127,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,
     109,    -1,    -1,   112,    12,   114,    -1,    -1,    -1,    -1,
      -1,    19,    -1,   122,   123,    -1,    24,    -1,   127,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,     4,    -1,    -1,    -1,    94,    -1,    96,    -1,
      12,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,    24,    -1,   112,    -1,   114,    -1,    30,    31,
      32,    33,    34,    35,   122,   123,    -1,    39,    40,   127,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,     4,    -1,    -1,   108,    -1,    -1,    -1,
     112,    12,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    24,    -1,   127,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,     4,    -1,    -1,   108,    -1,    -1,
      -1,   112,    12,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    24,    -1,   127,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
       4,    -1,    -1,    -1,    94,    -1,    96,    -1,    12,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
      24,    -1,   112,    -1,   114,    -1,    30,    31,    32,    33,
      34,    35,   122,   123,    -1,    39,    40,   127,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,     4,    -1,    -1,    -1,
      94,    -1,    96,    -1,    12,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,    24,    -1,   112,   113,
     114,    -1,    30,    31,    32,    33,    34,    35,   122,   123,
      -1,    39,    40,   127,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,     4,    -1,    -1,    -1,    94,    -1,    96,    -1,
      12,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,    24,    -1,   112,    -1,   114,    -1,    30,    31,
      32,    33,    34,    35,   122,   123,    -1,    39,    40,   127,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,     4,    -1,    -1,   108,    -1,    -1,    -1,
     112,    12,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    24,    -1,   127,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,     4,
      -1,    -1,    -1,    94,    -1,    96,    -1,    12,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,    24,
      -1,   112,    -1,   114,    -1,    30,    31,    32,    33,    34,
      35,   122,   123,    -1,    39,    40,   127,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,
      -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,     4,    -1,    -1,   112,     8,   114,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   122,   123,    -1,
      -1,    21,   127,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,    78,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,   112,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,   122,
     123,    -1,    21,   126,   127,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,    -1,    24,   126,   127,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
     112,    12,    -1,    51,    52,    53,    54,    55,    56,    -1,
     122,   123,    -1,    24,   126,   127,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    -1,    -1,    78,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,    24,    -1,    -1,   127,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,   112,     4,    51,    52,    53,    54,    55,    56,    -1,
      12,   122,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    24,    -1,    -1,   126,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,     4,    -1,    39,    40,    -1,    42,    43,    44,    12,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,    -1,   112,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,   122,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,     4,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   103,   104,    -1,
      -1,    -1,    -1,    -1,    24,    -1,   112,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,   122,     4,    -1,    39,
      40,    -1,    42,    43,    44,    12,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,   122,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,   122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,   103,   104,    -1,    30,
      31,    32,    33,    34,    35,   112,     4,    -1,    39,    40,
      -1,    42,    43,    44,    12,   122,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    12,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    24,    -1,    86,    -1,
      -1,   122,    30,    31,    32,    33,    34,    35,    -1,     4,
      -1,    39,    40,    -1,    42,    43,    44,    12,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    24,
      -1,    -1,    -1,    -1,   122,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   146,   147,   148,   151,   123,   127,   357,
     152,   163,     0,   152,    65,    66,   149,   105,   118,   153,
     164,   165,     1,   107,   356,   108,   133,   220,   220,   112,
     154,    14,   166,   177,   178,   133,   221,    77,    77,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   112,   122,   123,   126,   127,
     142,   155,   156,   157,   161,   323,   326,   327,   340,   342,
     343,   349,    27,    24,   167,   118,   162,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    29,    36,    50,    57,    63,    76,    82,    89,    92,
      94,   108,   112,   114,   123,   127,   132,   133,   134,   135,
     138,   143,   144,   175,   179,   180,   181,   182,   184,   199,
     225,   269,   274,   278,   279,   281,   282,   283,   284,   312,
     313,   316,   317,   339,   340,   343,   351,   352,   355,   109,
     119,   357,   357,    79,    93,    96,   103,   124,   125,   128,
     129,   346,   347,   348,   350,   113,   119,   142,   112,   158,
     104,   106,   150,   357,   118,   112,   277,   278,    33,    34,
      35,    89,    94,    96,   104,   108,   112,   114,   122,   205,
     230,   232,   234,   235,   237,   241,   321,   322,   326,   336,
     338,   349,    37,   200,   326,   104,   106,   304,   277,    72,
      73,    74,    75,   185,   104,   106,   217,   218,    19,    37,
     183,   232,   234,   322,    14,   304,   282,   108,   275,   276,
     112,   340,   108,   112,   314,   315,   317,   352,   282,   302,
     303,   282,   281,   282,    79,   109,   120,   125,   129,   277,
     278,   286,   288,   289,   319,   333,   335,   345,   346,   348,
     353,   354,   102,   113,   119,   285,   286,   287,   346,   347,
     348,   353,   358,   115,   358,    19,   275,   275,   134,   174,
     162,    19,    71,   207,    80,   119,    81,    84,   120,   271,
     272,   273,   319,   332,   334,   344,   346,   347,    88,   282,
     101,   104,    87,   133,   113,   113,   113,   113,   113,   157,
      78,   159,   160,   161,   152,   152,     4,   168,    23,    80,
     241,   241,   112,   225,   262,   263,   264,   343,    28,   109,
     227,   228,   230,   232,    79,    86,    93,   113,   124,   125,
     128,   129,   227,   245,   328,   329,   358,   358,    84,   249,
      91,    86,   120,   239,   324,   328,   337,    88,   238,   241,
     232,   112,    20,    46,   277,   301,   305,   306,   307,   305,
     118,   280,    77,    77,    77,    77,   223,   228,   242,   216,
     269,   270,   278,   216,    15,   194,   232,   232,    80,   119,
      81,    41,    90,   134,   340,    77,   133,   354,    80,   119,
     222,    86,   302,   342,   351,   332,    78,    84,   119,   109,
     119,   278,   341,   343,   101,   113,   113,   119,   113,   119,
     113,   113,   113,   119,   115,   242,   340,   340,   120,   176,
     318,   330,   331,   347,   354,   207,   133,   205,   224,   228,
     229,   339,   277,   291,   292,   307,   342,    27,   219,   273,
     279,   241,   341,    78,   308,   309,   310,   340,   341,   343,
     282,   113,   113,   119,   105,   356,   357,    12,   112,   169,
     170,   104,   106,   293,   223,   347,    80,   105,   119,   246,
     109,    80,    91,   113,   113,   119,   113,   113,   115,   250,
     251,   252,    27,   211,   232,   228,   326,   338,   234,   241,
      80,   202,   227,   244,   245,   218,   304,    85,   105,   118,
     356,    25,    27,   215,   105,   118,   356,   277,    81,    80,
      81,   203,   228,   253,   112,   322,   227,   133,   134,   109,
      77,    77,   113,   108,   112,   114,   320,   321,   314,    77,
     277,   120,   120,   277,   290,   307,   277,   286,   286,   341,
     286,   286,    77,    80,    80,   343,   352,   119,    28,   206,
     230,   232,    77,   133,    80,    81,   201,   257,   219,    81,
     119,   120,   218,   105,   119,    81,   101,   161,   112,    21,
     142,   161,   171,   172,   173,   105,   118,   277,   294,   295,
     296,   300,   294,   356,   113,   228,   264,   103,   104,   112,
     247,   248,   336,   253,   228,   227,   119,    86,   336,   104,
     106,   210,   120,   120,   253,   113,   119,   277,   306,   277,
     104,   106,   214,   270,   228,   253,   247,    84,   188,   339,
     109,   113,   115,   119,    78,   223,   226,   226,   120,   120,
     330,   246,   201,   257,    91,    77,   253,    28,   258,   259,
     260,    27,   254,     9,   265,   266,   267,   277,   307,   309,
     286,   311,   341,   171,   357,   158,   113,   119,   142,   294,
     105,   118,    84,    86,   297,   298,   299,   356,   228,   336,
     336,   113,   251,   252,     7,    26,   195,   208,   209,   270,
     209,   227,   280,     7,    26,   198,   199,   212,   213,   270,
     213,   189,   338,    27,   191,    80,   307,   277,    77,   119,
      77,    91,   103,   254,   265,   232,   246,    84,   231,   236,
     240,   241,   261,   104,   106,   265,    22,    51,    52,   112,
     186,   268,   325,   326,   267,    81,   101,   113,   173,   296,
     291,   277,   219,   299,    80,   105,    80,    37,   196,    19,
      37,   194,   232,   105,   118,   356,    11,    19,   197,   197,
     105,   118,   356,    86,   104,   106,   192,   224,   223,   232,
     230,   234,   265,   103,   259,    91,   120,   240,   318,   255,
     256,   280,   255,   113,   232,   233,   243,   268,    53,   187,
     286,   341,    86,   219,   253,   253,   232,   194,   232,    80,
      81,   204,   208,   277,   194,   207,   212,   190,   338,    78,
     193,   194,    78,   193,    91,   236,   261,   236,   105,   118,
     315,   356,   119,   113,   232,   277,   105,   113,   202,    81,
     204,   253,   112,   248,   336,   206,   338,   105,   105,   118,
     356,   356,   232,   256,    80,   243,   336,    84,   201,   257,
     194,   223,   189,   254,   265,   265
  };

  const short
  parser::yyr1_[] =
  {
       0,   145,   146,   147,   147,   148,   149,   149,   149,   150,
     150,   151,   151,   152,   153,   153,   153,   154,   154,   155,
     155,   155,   155,   156,   156,   157,   157,   157,   158,   158,
     158,   159,   159,   160,   160,   161,   161,   162,   162,   163,
     163,   164,   165,   165,   166,   167,   167,   168,   168,   169,
     169,   170,   170,   171,   171,   171,   171,   172,   172,   173,
     173,   174,   174,   175,   175,   175,   176,   176,   177,   178,
     178,   179,   179,   179,   179,   179,   179,   179,   179,   180,
     181,   181,   181,   181,   181,   182,   183,   183,   184,   184,
     184,   184,   185,   185,   185,   185,   185,   186,   186,   186,
     187,   188,   188,   189,   190,   190,   191,   191,   192,   192,
     192,   192,   193,   193,   193,   193,   194,   195,   195,   195,
     195,   195,   196,   196,   197,   197,   198,   198,   198,   199,
     199,   200,   200,   201,   201,   202,   202,   203,   203,   203,
     204,   204,   204,   205,   205,   206,   206,   206,   206,   207,
     207,   207,   208,   208,   209,   209,   209,   209,   210,   210,
     211,   211,   212,   212,   213,   213,   213,   213,   214,   214,
     215,   215,   216,   216,   216,   216,   217,   217,   218,   219,
     219,   220,   220,   221,   221,   221,   222,   222,   223,   224,
     225,   225,   226,   226,   227,   227,   228,   228,   228,   229,
     230,   231,   232,   232,   233,   234,   235,   235,   236,   236,
     237,   237,   237,   238,   239,   239,   240,   241,   241,   241,
     241,   241,   241,   241,   241,   241,   241,   242,   243,   243,
     244,   244,   245,   245,   246,   246,   247,   247,   247,   248,
     248,   249,   249,   250,   250,   251,   252,   252,   253,   254,
     254,   254,   255,   255,   256,   257,   258,   258,   259,   259,
     260,   260,   261,   261,   262,   262,   263,   263,   264,   265,
     265,   266,   266,   267,   267,   267,   268,   268,   268,   269,
     269,   270,   271,   271,   272,   272,   273,   274,   274,   274,
     274,   274,   274,   274,   274,   274,   275,   275,   276,   276,
     277,   277,   278,   278,   279,   279,   280,   280,   281,   281,
     281,   281,   282,   282,   282,   282,   282,   282,   282,   282,
     282,   282,   283,   283,   283,   284,   284,   284,   284,   284,
     284,   284,   284,   285,   285,   286,   286,   286,   287,   287,
     288,   288,   288,   288,   288,   288,   288,   289,   289,   290,
     290,   291,   292,   292,   293,   293,   293,   293,   294,   294,
     295,   295,   295,   296,   297,   297,   298,   298,   299,   300,
     301,   302,   303,   303,   304,   304,   305,   305,   305,   305,
     306,   306,   307,   307,   307,   308,   308,   309,   309,   309,
     310,   310,   310,   310,   311,   311,   312,   312,   313,   313,
     314,   314,   314,   315,   315,   316,   316,   316,   316,   317,
     317,   318,   318,   319,   319,   320,   320,   320,   321,   321,
     321,   321,   321,   322,   322,   323,   323,   323,   323,   324,
     324,   325,   326,   326,   327,   328,   328,   328,   329,   329,
     329,   329,   330,   330,   331,   331,   332,   332,   333,   333,
     334,   334,   335,   335,   336,   337,   338,   338,   338,   338,
     338,   339,   339,   340,   340,   340,   341,   342,   342,   343,
     343,   343,   343,   343,   343,   343,   343,   344,   344,   345,
     345,   346,   347,   347,   348,   348,   349,   349,   349,   349,
     349,   349,   349,   349,   349,   349,   349,   349,   349,   349,
     349,   349,   349,   349,   350,   350,   351,   351,   352,   353,
     353,   354,   354,   355,   355,   355,   355,   355,   356,   356,
     357,   357,   358,   358
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
       0,     1,     1,     1,     1,     5,     7,     1,     1,     4,
       4,     5,     6,     6,     4,     4,     3,     1,     4,     3,
       6,     7,     2,     2,     2,     2,     0,     1,     1,     1,
       2,     0,     2,     3,     2,     1,     0,     2,     3,     3,
       3,     3,     3,     2,     1,     0,     3,     4,     3,     4,
       2,     3,     0,     1,     0,     1,     3,     6,     7,     1,
       1,     0,     1,     0,     2,     0,     2,     0,     2,     2,
       0,     2,     4,     3,     1,     6,     4,     3,     1,     4,
       3,     0,     1,     1,     3,     2,     1,     0,     3,     3,
       2,     0,     1,     1,     3,     2,     1,     0,     3,     3,
       2,     0,     3,     2,     1,     0,     3,     3,     1,     2,
       0,     1,     3,     3,     1,     0,     0,     2,     1,     1,
       3,     1,     1,     3,     1,     3,     4,     3,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     3,     1,     2,
       1,     2,     3,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     3,     2,     5,     3,     3,     1,     1,     3,
       1,     0,     1,     3,     2,     0,     1,     3,     5,     1,
       5,     0,     2,     3,     1,     3,     2,     0,     1,     4,
       4,     0,     3,     1,     4,     2,     3,     1,     4,     2,
       3,     0,     1,     3,     0,     1,     3,     1,     3,     0,
       1,     2,     1,     2,     3,     3,     1,     2,     3,     1,
       2,     1,     3,     2,     2,     1,     4,     3,     3,     4,
       4,     3,     4,     6,     6,     4,     0,     1,     3,     4,
       3,     1,     1,     3,     2,     1,     1,     0,     2,     3,
       2,     1,     3,     2,     2,     4,     4,     8,     4,     2,
       2,     1,     4,     3,     1,     1,     1,     1,     3,     3,
       3,     3,     1,     3,     2,     1,     2,     2,     3,     3,
       1,     1,     2,     4,     3,     5,     3,     3,     3,     3,
       1,     1,     3,     1,     3,     3,     2,     2,     1,     2,
       3,     2,     1,     2,     3,     2,     2,     1,     4,     1,
       1,     1,     2,     1,     3,     3,     3,     2,     1,     0,
       1,     2,     3,     1,     2,     1,     0,     3,     1,     1,
       3,     1,     5,     3,     3,     1,     1,     1,     1,     3,
       1,     3,     1,     3,     1,     2,     3,     2,     3,     1,
       2,     1,     3,     1,     3,     1,     2,     2,     1,     3,
       3,     3,     2,     1,     3,     1,     3,     3,     3,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     1,
       1,     3,     1,     3,     1,     3,     1,     1,     1,     1,
       1,     1,     3,     1,     3,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1
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
  "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"", "TIGHT_INFIX_AT",
  "\"@\"", "PREFIX_TILDE", "\"~\"", "\"=>\"", "PREFIX_MINUS", "\"-\"",
  "PREFIX_BANG", "\"!\"", "\"*\"", "\"-<\"", "\">-\"", "\"-<<\"",
  "\">>-\"", "TIGHT_INFIX_DOT", "PREFIX_DOT", "\".\"", "\"{\"", "\"}\"",
  "\"vocurly\"", "\"vccurly\"", "\"[\"", "\"]\"", "\"[:\"", "\":]\"",
  "\"(\"", "\")\"", "\"(#\"", "\"#)\"", "\"(|\"", "\"|)\"", "\";\"",
  "\",\"", "\"`\"", "\"'\"", "\"VARID\"", "\"CONID\"", "\"VARSYM\"",
  "\"CONSYM\"", "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"", "\"QCONSYM\"",
  "\"IPDUPVARID\"", "\"LABELVARID\"", "\"CHAR\"", "\"STRING\"",
  "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"", "\"PRIMSTRING\"",
  "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"", "\"PRIMDOUBLE\"",
  "','", "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"", "$accept",
  "unit", "module", "missing_module_keyword", "maybemodwarning", "body",
  "body2", "top", "top1", "maybeexports", "exportlist", "exportlist1",
  "export", "export_subspec", "qcnames", "qcnames1", "qcname", "semis1",
  "semis", "importdecls", "importdecls_semi", "importdecl", "optqualified",
  "maybeas", "maybeimpspec", "impspec", "importlist", "importlist1",
  "import", "prec", "infix", "ops", "topdecls", "topdecls_semi", "topdecl",
  "cl_decl", "ty_decl", "standalone_kind_sig", "sks_vars", "inst_decl",
  "overlap_pragma", "deriv_strategy_no_via", "deriv_strategy_via",
  "opt_injective_info", "injectivity_cond", "inj_varids",
  "where_type_family", "ty_fam_inst_eqn_list", "ty_fam_inst_eqns",
  "ty_fam_inst_eqn", "at_decl_cls", "opt_family", "opt_instance",
  "at_decl_inst", "data_or_newtype", "opt_class", "opt_kind_sig",
  "opt_datafam_kind_sig", "opt_tyfam_kind_sig", "opt_at_kind_inj_sig",
  "tycl_hdr", "datafam_inst_hdr", "capi_ctype", "decl_cls", "decls_cls",
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
       0,   511,   511,   528,   529,   531,   535,   536,   537,   539,
     540,   542,   543,   546,   548,   549,   550,   558,   559,   561,
     562,   563,   564,   566,   567,   569,   570,   571,   573,   574,
     575,   577,   578,   580,   581,   583,   584,   588,   589,   591,
     592,   594,   596,   597,   599,   612,   613,   615,   616,   618,
     619,   623,   624,   626,   627,   628,   629,   631,   632,   634,
     635,   640,   641,   643,   644,   645,   647,   648,   652,   654,
     655,   657,   658,   659,   660,   663,   664,   670,   672,   675,
     678,   679,   681,   682,   683,   685,   687,   688,   691,   692,
     693,   699,   706,   707,   708,   709,   710,   712,   713,   714,
     716,   727,   728,   730,   732,   733,   737,   738,   740,   741,
     742,   743,   745,   746,   747,   748,   750,   753,   755,   757,
     759,   760,   762,   762,   764,   764,   768,   770,   777,   784,
     785,   788,   789,   793,   794,   796,   797,   799,   800,   801,
     803,   804,   805,   808,   809,   812,   813,   814,   815,   817,
     818,   819,   861,   862,   864,   865,   866,   867,   869,   870,
     872,   873,   875,   876,   878,   879,   880,   881,   883,   884,
     886,   887,   890,   891,   892,   893,   895,   896,   898,   900,
     901,   909,   910,   912,   913,   914,   927,   928,   937,   939,
     941,   942,   944,   945,   954,   955,   957,   958,   960,   962,
     971,   973,   975,   976,   978,   981,   983,   984,   986,   987,
     989,   991,   992,   994,   996,   997,  1004,  1011,  1012,  1013,
    1014,  1015,  1016,  1017,  1018,  1024,  1025,  1028,  1030,  1031,
    1033,  1034,  1036,  1037,  1044,  1045,  1047,  1048,  1049,  1052,
    1053,  1057,  1058,  1060,  1061,  1064,  1066,  1067,  1072,  1078,
    1079,  1080,  1082,  1083,  1085,  1087,  1089,  1090,  1092,  1093,
    1095,  1096,  1098,  1099,  1105,  1106,  1108,  1109,  1111,  1113,
    1114,  1116,  1117,  1119,  1120,  1121,  1123,  1124,  1125,  1130,
    1132,  1134,  1138,  1139,  1141,  1142,  1146,  1156,  1157,  1159,
    1160,  1161,  1162,  1163,  1164,  1165,  1168,  1169,  1171,  1172,
    1177,  1178,  1182,  1183,  1185,  1186,  1188,  1189,  1194,  1195,
    1196,  1197,  1200,  1201,  1202,  1203,  1204,  1206,  1208,  1209,
    1210,  1212,  1215,  1216,  1217,  1220,  1221,  1222,  1223,  1224,
    1225,  1230,  1231,  1234,  1235,  1240,  1241,  1242,  1247,  1248,
    1266,  1267,  1268,  1269,  1270,  1271,  1272,  1274,  1275,  1288,
    1290,  1300,  1302,  1303,  1306,  1307,  1308,  1309,  1311,  1312,
    1314,  1315,  1316,  1318,  1320,  1321,  1323,  1324,  1333,  1335,
    1337,  1339,  1341,  1342,  1345,  1346,  1348,  1349,  1350,  1351,
    1356,  1357,  1359,  1360,  1361,  1366,  1367,  1369,  1370,  1371,
    1373,  1374,  1375,  1376,  1379,  1380,  1412,  1413,  1415,  1416,
    1418,  1419,  1420,  1422,  1423,  1425,  1426,  1427,  1428,  1430,
    1431,  1433,  1434,  1436,  1437,  1440,  1441,  1442,  1444,  1445,
    1446,  1447,  1448,  1450,  1451,  1453,  1454,  1455,  1456,  1459,
    1460,  1462,  1464,  1465,  1469,  1471,  1472,  1473,  1475,  1476,
    1477,  1478,  1483,  1484,  1486,  1487,  1489,  1490,  1493,  1494,
    1499,  1500,  1502,  1503,  1507,  1509,  1511,  1512,  1513,  1514,
    1515,  1518,  1519,  1521,  1522,  1523,  1525,  1527,  1528,  1530,
    1531,  1532,  1533,  1534,  1535,  1536,  1537,  1539,  1540,  1542,
    1543,  1545,  1547,  1548,  1550,  1551,  1553,  1554,  1555,  1556,
    1557,  1558,  1559,  1560,  1561,  1562,  1563,  1564,  1565,  1566,
    1567,  1568,  1569,  1570,  1572,  1573,  1577,  1578,  1580,  1582,
    1583,  1585,  1586,  1590,  1591,  1592,  1593,  1594,  1599,  1602,
    1606,  1607,  1609,  1610
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
#line 7520 "parser.cc"

#line 1619 "parser.y"


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
    std::vector<Hs::SignatureDecl> sig_decls;
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
            else if (auto S = decl.to<Hs::SignatureDecl>())
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

