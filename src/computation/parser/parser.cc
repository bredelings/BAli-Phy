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

      case symbol_kind::S_impspec: // impspec
        value.YY_MOVE_OR_COPY< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_kind: // kind
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
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

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_kind: // kind
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
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

      case symbol_kind::S_impspec: // impspec
        value.copy< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_export: // export
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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_kind: // kind
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
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

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (that.value);
        break;

      case symbol_kind::S_export: // export
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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_kind: // kind
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Hs::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
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

      case symbol_kind::S_impspec: // impspec
        yylhs.value.emplace< Hs::ImpSpec > ();
        break;

      case symbol_kind::S_export: // export
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

      case symbol_kind::S_RATIONAL: // "RATIONAL"
      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        yylhs.value.emplace< double > ();
        break;

      case symbol_kind::S_kind: // kind
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

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Hs::FieldDecl> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Hs::GuardedRHS> > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
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
#line 2471 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 528 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2477 "parser.cc"
    break;

  case 4: // module: body2
#line 529 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2483 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 531 "parser.y"
                                                                 {drv.push_module_context();}
#line 2489 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 539 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2495 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 540 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2501 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 542 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2507 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 543 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2513 "parser.cc"
    break;

  case 13: // top: semis top1
#line 546 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2519 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 548 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2525 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 549 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2531 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 550 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2537 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 558 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2543 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 559 "parser.y"
                                      {}
#line 2549 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 561 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2555 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 563 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2561 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 564 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2567 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 566 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2573 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 567 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2579 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 569 "parser.y"
                                      {}
#line 2585 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 570 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2591 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 571 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2597 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 573 "parser.y"
                   {}
#line 2603 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 574 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2609 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 576 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2615 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 577 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2621 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 579 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2627 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 580 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2633 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 590 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2639 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 592 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2645 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 593 "parser.y"
                         { }
#line 2651 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 595 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2659 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 608 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2665 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 609 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2671 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 611 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2677 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 612 "parser.y"
                               { }
#line 2683 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 614 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2689 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 615 "parser.y"
                               { }
#line 2695 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 619 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2701 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 620 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2707 "parser.cc"
    break;

  case 49: // prec: %empty
#line 625 "parser.y"
                   { }
#line 2713 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 626 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2719 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 628 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2725 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 629 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2731 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 630 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2737 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 632 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2743 "parser.cc"
    break;

  case 55: // ops: op
#line 633 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2749 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 637 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2755 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 639 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2761 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 640 "parser.y"
                                            { }
#line 2767 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 642 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2773 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 643 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2779 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 644 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2785 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 645 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2791 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 648 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2797 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 649 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2803 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 655 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2809 "parser.cc"
    break;

  case 66: // topdecl: infixexp
#line 657 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2815 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 660 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2821 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 663 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2827 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 664 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2833 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 666 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ())};}
#line 2839 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 667 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2845 "parser.cc"
    break;

  case 72: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 668 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 2851 "parser.cc"
    break;

  case 73: // standalone_kind_sig: "type" sks_vars "::" kind
#line 670 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < expression_ref > ())};}
#line 2857 "parser.cc"
    break;

  case 74: // sks_vars: sks_vars "," oqtycon
#line 672 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2863 "parser.cc"
    break;

  case 75: // sks_vars: oqtycon
#line 673 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2869 "parser.cc"
    break;

  case 76: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 676 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2875 "parser.cc"
    break;

  case 77: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 677 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 2881 "parser.cc"
    break;

  case 78: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 681 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 2887 "parser.cc"
    break;

  case 79: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 682 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 2893 "parser.cc"
    break;

  case 80: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 683 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 2899 "parser.cc"
    break;

  case 81: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 684 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 2905 "parser.cc"
    break;

  case 82: // overlap_pragma: %empty
#line 685 "parser.y"
                                               {}
#line 2911 "parser.cc"
    break;

  case 92: // where_type_family: %empty
#line 712 "parser.y"
                                                           {}
#line 2917 "parser.cc"
    break;

  case 93: // where_type_family: "where" ty_fam_inst_eqn_list
#line 713 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2923 "parser.cc"
    break;

  case 94: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 715 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2929 "parser.cc"
    break;

  case 95: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 716 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2935 "parser.cc"
    break;

  case 96: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 717 "parser.y"
                                                           {}
#line 2941 "parser.cc"
    break;

  case 97: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 718 "parser.y"
                                                           {}
#line 2947 "parser.cc"
    break;

  case 98: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 720 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2953 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 721 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2959 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 722 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 2965 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqns: %empty
#line 723 "parser.y"
                                                           {}
#line 2971 "parser.cc"
    break;

  case 102: // ty_fam_inst_eqn: type "=" ctype
#line 725 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 2977 "parser.cc"
    break;

  case 103: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 728 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 2983 "parser.cc"
    break;

  case 104: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 730 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 2989 "parser.cc"
    break;

  case 105: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 732 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 2995 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" ty_fam_inst_eqn
#line 734 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3001 "parser.cc"
    break;

  case 107: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 735 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3007 "parser.cc"
    break;

  case 112: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 743 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3013 "parser.cc"
    break;

  case 113: // data_or_newtype: "data"
#line 745 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3019 "parser.cc"
    break;

  case 114: // data_or_newtype: "newtype"
#line 746 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3025 "parser.cc"
    break;

  case 115: // opt_kind_sig: %empty
#line 750 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3031 "parser.cc"
    break;

  case 116: // opt_kind_sig: "::" kind
#line 751 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 3037 "parser.cc"
    break;

  case 117: // opt_datafam_kind_sig: %empty
#line 753 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3043 "parser.cc"
    break;

  case 118: // opt_datafam_kind_sig: "::" kind
#line 754 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 3049 "parser.cc"
    break;

  case 119: // opt_tyfam_kind_sig: %empty
#line 756 "parser.y"
                                      {}
#line 3055 "parser.cc"
    break;

  case 120: // opt_tyfam_kind_sig: "::" kind
#line 757 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 3061 "parser.cc"
    break;

  case 121: // opt_tyfam_kind_sig: "=" tv_bndr
#line 758 "parser.y"
                                      {}
#line 3067 "parser.cc"
    break;

  case 122: // opt_at_kind_inj_sig: %empty
#line 760 "parser.y"
                                      {}
#line 3073 "parser.cc"
    break;

  case 123: // opt_at_kind_inj_sig: "::" kind
#line 761 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 3079 "parser.cc"
    break;

  case 124: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 762 "parser.y"
                                                                  {}
#line 3085 "parser.cc"
    break;

  case 125: // tycl_hdr: context "=>" type
#line 766 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3091 "parser.cc"
    break;

  case 126: // tycl_hdr: type
#line 767 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3097 "parser.cc"
    break;

  case 130: // decl_cls: at_decl_cls
#line 813 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3103 "parser.cc"
    break;

  case 131: // decl_cls: decl
#line 814 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3109 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";" decl_cls
#line 816 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3115 "parser.cc"
    break;

  case 133: // decls_cls: decls_cls ";"
#line 817 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3121 "parser.cc"
    break;

  case 134: // decls_cls: decl_cls
#line 818 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3127 "parser.cc"
    break;

  case 135: // decls_cls: %empty
#line 819 "parser.y"
                                           {}
#line 3133 "parser.cc"
    break;

  case 136: // decllist_cls: "{" decls_cls "}"
#line 821 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3139 "parser.cc"
    break;

  case 137: // decllist_cls: "vocurly" decls_cls close
#line 822 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3145 "parser.cc"
    break;

  case 138: // where_cls: "where" decllist_cls
#line 824 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3151 "parser.cc"
    break;

  case 139: // where_cls: %empty
#line 825 "parser.y"
                                           {}
#line 3157 "parser.cc"
    break;

  case 140: // decl_inst: at_decl_inst
#line 827 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3163 "parser.cc"
    break;

  case 141: // decl_inst: decl
#line 828 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3169 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";" decl_inst
#line 830 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3175 "parser.cc"
    break;

  case 143: // decls_inst: decls_inst ";"
#line 831 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3181 "parser.cc"
    break;

  case 144: // decls_inst: decl_inst
#line 832 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3187 "parser.cc"
    break;

  case 145: // decls_inst: %empty
#line 833 "parser.y"
                                           {}
#line 3193 "parser.cc"
    break;

  case 146: // decllist_inst: "{" decls_inst "}"
#line 835 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3199 "parser.cc"
    break;

  case 147: // decllist_inst: "vocurly" decls_inst close
#line 836 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3205 "parser.cc"
    break;

  case 148: // where_inst: "where" decllist_inst
#line 838 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3211 "parser.cc"
    break;

  case 149: // where_inst: %empty
#line 839 "parser.y"
                                           {}
#line 3217 "parser.cc"
    break;

  case 150: // decls: decls ";" decl
#line 842 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3223 "parser.cc"
    break;

  case 151: // decls: decls ";"
#line 843 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3229 "parser.cc"
    break;

  case 152: // decls: decl
#line 844 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3235 "parser.cc"
    break;

  case 153: // decls: %empty
#line 845 "parser.y"
                        {}
#line 3241 "parser.cc"
    break;

  case 154: // decllist: "{" decls "}"
#line 847 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3247 "parser.cc"
    break;

  case 155: // decllist: "vocurly" decls close
#line 848 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3253 "parser.cc"
    break;

  case 156: // binds: decllist
#line 850 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3259 "parser.cc"
    break;

  case 157: // wherebinds: "where" binds
#line 852 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3265 "parser.cc"
    break;

  case 158: // wherebinds: %empty
#line 853 "parser.y"
                                 {}
#line 3271 "parser.cc"
    break;

  case 164: // opt_tyconsig: %empty
#line 879 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3277 "parser.cc"
    break;

  case 165: // opt_tyconsig: "::" gtycon
#line 880 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3283 "parser.cc"
    break;

  case 166: // sigtype: ctype
#line 889 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3289 "parser.cc"
    break;

  case 167: // sigtypedoc: ctypedoc
#line 891 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3295 "parser.cc"
    break;

  case 168: // sig_vars: sig_vars "," var
#line 893 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3301 "parser.cc"
    break;

  case 169: // sig_vars: var
#line 894 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3307 "parser.cc"
    break;

  case 170: // sigtypes1: sigtype
#line 896 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3313 "parser.cc"
    break;

  case 171: // sigtypes1: sigtypes1 "," sigtype
#line 897 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3319 "parser.cc"
    break;

  case 172: // ktype: ctype
#line 906 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3325 "parser.cc"
    break;

  case 173: // ktype: ctype "::" kind
#line 907 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < expression_ref > ())};}
#line 3331 "parser.cc"
    break;

  case 174: // ctype: "forall" tv_bndrs "." ctype
#line 909 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3337 "parser.cc"
    break;

  case 175: // ctype: context "=>" ctype
#line 910 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3343 "parser.cc"
    break;

  case 176: // ctype: type
#line 912 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3349 "parser.cc"
    break;

  case 177: // ctypedoc: ctype
#line 914 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3355 "parser.cc"
    break;

  case 178: // context: btype
#line 923 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3361 "parser.cc"
    break;

  case 179: // context_no_ops: btype_no_ops
#line 925 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3367 "parser.cc"
    break;

  case 180: // type: btype
#line 927 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3373 "parser.cc"
    break;

  case 181: // type: btype "->" ctype
#line 928 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3379 "parser.cc"
    break;

  case 182: // typedoc: type
#line 930 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3385 "parser.cc"
    break;

  case 183: // btype: infixtype
#line 933 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3391 "parser.cc"
    break;

  case 184: // infixtype: ftype
#line 935 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3397 "parser.cc"
    break;

  case 185: // infixtype: btype tyop btype
#line 936 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3403 "parser.cc"
    break;

  case 186: // btype_no_ops: atype_docs
#line 938 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3409 "parser.cc"
    break;

  case 187: // btype_no_ops: btype_no_ops atype_docs
#line 939 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3415 "parser.cc"
    break;

  case 188: // ftype: atype
#line 941 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3421 "parser.cc"
    break;

  case 189: // ftype: ftype tyarg
#line 943 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3427 "parser.cc"
    break;

  case 190: // ftype: ftype "@" atype
#line 944 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3433 "parser.cc"
    break;

  case 191: // tyarg: atype
#line 946 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3439 "parser.cc"
    break;

  case 192: // tyop: qtyconop
#line 948 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3445 "parser.cc"
    break;

  case 193: // tyop: tyvarop
#line 949 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3451 "parser.cc"
    break;

  case 194: // atype_docs: atype
#line 956 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3457 "parser.cc"
    break;

  case 195: // atype: ntgtycon
#line 963 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3463 "parser.cc"
    break;

  case 196: // atype: tyvar
#line 964 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3469 "parser.cc"
    break;

  case 197: // atype: "*"
#line 965 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3475 "parser.cc"
    break;

  case 198: // atype: PREFIX_BANG atype
#line 966 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3481 "parser.cc"
    break;

  case 199: // atype: PREFIX_TILDE atype
#line 967 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3487 "parser.cc"
    break;

  case 200: // atype: "{" fielddecls "}"
#line 968 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3493 "parser.cc"
    break;

  case 201: // atype: "(" ")"
#line 969 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3499 "parser.cc"
    break;

  case 202: // atype: "(" comma_types1 "," ktype ")"
#line 970 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3505 "parser.cc"
    break;

  case 203: // atype: "[" ktype "]"
#line 976 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3511 "parser.cc"
    break;

  case 204: // atype: "(" ktype ")"
#line 977 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3517 "parser.cc"
    break;

  case 205: // inst_type: sigtype
#line 980 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3523 "parser.cc"
    break;

  case 208: // comma_types0: comma_types1
#line 985 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3529 "parser.cc"
    break;

  case 209: // comma_types0: %empty
#line 986 "parser.y"
                                       { /* default construction OK */ }
#line 3535 "parser.cc"
    break;

  case 210: // comma_types1: ktype
#line 988 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3541 "parser.cc"
    break;

  case 211: // comma_types1: comma_types1 "," ktype
#line 989 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3547 "parser.cc"
    break;

  case 212: // tv_bndrs: tv_bndrs tv_bndr
#line 996 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3553 "parser.cc"
    break;

  case 213: // tv_bndrs: %empty
#line 997 "parser.y"
                               { /* default construction OK */}
#line 3559 "parser.cc"
    break;

  case 214: // tv_bndr: tv_bndr_no_braces
#line 999 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3565 "parser.cc"
    break;

  case 215: // tv_bndr: "{" tyvar "}"
#line 1000 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3571 "parser.cc"
    break;

  case 216: // tv_bndr: "{" tyvar "::" kind "}"
#line 1001 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3577 "parser.cc"
    break;

  case 217: // tv_bndr_no_braces: tyvar
#line 1004 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3583 "parser.cc"
    break;

  case 218: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1005 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ())};}
#line 3589 "parser.cc"
    break;

  case 219: // kind: ctype
#line 1023 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3595 "parser.cc"
    break;

  case 220: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1029 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3601 "parser.cc"
    break;

  case 221: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1030 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3607 "parser.cc"
    break;

  case 222: // gadt_constrlist: %empty
#line 1031 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3613 "parser.cc"
    break;

  case 223: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1033 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3619 "parser.cc"
    break;

  case 224: // gadt_constrs: gadt_constr
#line 1034 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3625 "parser.cc"
    break;

  case 225: // gadt_constr: optSemi con_list "::" sigtype
#line 1036 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3631 "parser.cc"
    break;

  case 226: // constrs: "=" constrs1
#line 1038 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3637 "parser.cc"
    break;

  case 227: // constrs1: constrs1 "|" constr
#line 1040 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3643 "parser.cc"
    break;

  case 228: // constrs1: constr
#line 1041 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3649 "parser.cc"
    break;

  case 229: // constr: forall context_no_ops "=>" constr_stuff
#line 1043 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3655 "parser.cc"
    break;

  case 230: // constr: forall constr_stuff
#line 1044 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3661 "parser.cc"
    break;

  case 231: // forall: "forall" tv_bndrs "."
#line 1046 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3667 "parser.cc"
    break;

  case 232: // forall: %empty
#line 1047 "parser.y"
                                {}
#line 3673 "parser.cc"
    break;

  case 233: // constr_stuff: btype_no_ops
#line 1049 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3679 "parser.cc"
    break;

  case 234: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1050 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3689 "parser.cc"
    break;

  case 235: // fielddecls: %empty
#line 1056 "parser.y"
                                {}
#line 3695 "parser.cc"
    break;

  case 236: // fielddecls: fielddecls1
#line 1057 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3701 "parser.cc"
    break;

  case 237: // fielddecls1: fielddecls1 "," fielddecl
#line 1059 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3707 "parser.cc"
    break;

  case 238: // fielddecls1: fielddecl
#line 1060 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3713 "parser.cc"
    break;

  case 239: // fielddecl: sig_vars "::" ctype
#line 1062 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3719 "parser.cc"
    break;

  case 250: // decl_no_th: sigdecl
#line 1081 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3725 "parser.cc"
    break;

  case 251: // decl_no_th: infixexp rhs
#line 1083 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3731 "parser.cc"
    break;

  case 252: // decl: decl_no_th
#line 1085 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3737 "parser.cc"
    break;

  case 253: // rhs: "=" exp wherebinds
#line 1089 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3743 "parser.cc"
    break;

  case 254: // rhs: gdrhs wherebinds
#line 1090 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3749 "parser.cc"
    break;

  case 255: // gdrhs: gdrhs gdrh
#line 1092 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3755 "parser.cc"
    break;

  case 256: // gdrhs: gdrh
#line 1093 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3761 "parser.cc"
    break;

  case 257: // gdrh: "|" guardquals "=" exp
#line 1097 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3767 "parser.cc"
    break;

  case 258: // sigdecl: sig_vars "::" sigtypedoc
#line 1107 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3773 "parser.cc"
    break;

  case 259: // sigdecl: infix prec ops
#line 1108 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3779 "parser.cc"
    break;

  case 260: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1110 "parser.y"
                                                    {}
#line 3785 "parser.cc"
    break;

  case 261: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1111 "parser.y"
                                            {}
#line 3791 "parser.cc"
    break;

  case 262: // sigdecl: "{-# SCC" qvar "#-}"
#line 1112 "parser.y"
                              {}
#line 3797 "parser.cc"
    break;

  case 263: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1113 "parser.y"
                                     {}
#line 3803 "parser.cc"
    break;

  case 264: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1114 "parser.y"
                                                               {}
#line 3809 "parser.cc"
    break;

  case 265: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1115 "parser.y"
                                                                      {}
#line 3815 "parser.cc"
    break;

  case 266: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1116 "parser.y"
                                                     {}
#line 3821 "parser.cc"
    break;

  case 271: // exp: infixexp "::" sigtype
#line 1128 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 3827 "parser.cc"
    break;

  case 272: // exp: infixexp
#line 1129 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3833 "parser.cc"
    break;

  case 273: // infixexp: exp10
#line 1133 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3839 "parser.cc"
    break;

  case 274: // infixexp: infixexp qop exp10
#line 1134 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3845 "parser.cc"
    break;

  case 275: // exp10: PREFIX_MINUS fexp
#line 1136 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3851 "parser.cc"
    break;

  case 276: // exp10: fexp
#line 1137 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3857 "parser.cc"
    break;

  case 279: // fexp: fexp aexp
#line 1145 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3863 "parser.cc"
    break;

  case 280: // fexp: fexp "@" atype
#line 1146 "parser.y"
                                 {}
#line 3869 "parser.cc"
    break;

  case 281: // fexp: "static" aexp
#line 1147 "parser.y"
                                 {}
#line 3875 "parser.cc"
    break;

  case 282: // fexp: aexp
#line 1148 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3881 "parser.cc"
    break;

  case 283: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1151 "parser.y"
                                            {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern(Hs::Var(yystack_[2].value.as < std::string > ()),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3887 "parser.cc"
    break;

  case 284: // aexp: PREFIX_TILDE aexp
#line 1152 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3893 "parser.cc"
    break;

  case 285: // aexp: PREFIX_BANG aexp
#line 1153 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3899 "parser.cc"
    break;

  case 286: // aexp: "\\" apats1 "->" exp
#line 1154 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3905 "parser.cc"
    break;

  case 287: // aexp: "let" binds "in" exp
#line 1155 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3911 "parser.cc"
    break;

  case 288: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1157 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3917 "parser.cc"
    break;

  case 289: // aexp: "case" exp "of" altslist
#line 1159 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 3923 "parser.cc"
    break;

  case 290: // aexp: "do" stmtlist
#line 1160 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3929 "parser.cc"
    break;

  case 291: // aexp: "mdo" stmtlist
#line 1161 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3935 "parser.cc"
    break;

  case 292: // aexp: aexp1
#line 1163 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3941 "parser.cc"
    break;

  case 293: // aexp1: aexp1 "{" fbinds "}"
#line 1166 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 3947 "parser.cc"
    break;

  case 294: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1167 "parser.y"
                                     { }
#line 3953 "parser.cc"
    break;

  case 295: // aexp1: aexp2
#line 1168 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 3959 "parser.cc"
    break;

  case 296: // aexp2: qvar
#line 1171 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 3965 "parser.cc"
    break;

  case 297: // aexp2: qcon
#line 1172 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 3971 "parser.cc"
    break;

  case 298: // aexp2: literal
#line 1173 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 3977 "parser.cc"
    break;

  case 299: // aexp2: "(" texp ")"
#line 1174 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 3983 "parser.cc"
    break;

  case 300: // aexp2: "(" tup_exprs ")"
#line 1175 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 3989 "parser.cc"
    break;

  case 301: // aexp2: "(" projection ")"
#line 1176 "parser.y"
                              {}
#line 3995 "parser.cc"
    break;

  case 302: // aexp2: "[" list "]"
#line 1181 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4001 "parser.cc"
    break;

  case 303: // aexp2: "_"
#line 1182 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4007 "parser.cc"
    break;

  case 306: // texp: exp
#line 1191 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4013 "parser.cc"
    break;

  case 307: // texp: infixexp qop
#line 1192 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4019 "parser.cc"
    break;

  case 308: // texp: qopm infixexp
#line 1193 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4025 "parser.cc"
    break;

  case 309: // tup_exprs: tup_exprs "," texp
#line 1198 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4031 "parser.cc"
    break;

  case 310: // tup_exprs: texp "," texp
#line 1199 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4037 "parser.cc"
    break;

  case 311: // list: texp
#line 1217 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4043 "parser.cc"
    break;

  case 312: // list: lexps
#line 1218 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4049 "parser.cc"
    break;

  case 313: // list: texp ".."
#line 1219 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4055 "parser.cc"
    break;

  case 314: // list: texp "," exp ".."
#line 1220 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4061 "parser.cc"
    break;

  case 315: // list: texp ".." exp
#line 1221 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4067 "parser.cc"
    break;

  case 316: // list: texp "," exp ".." exp
#line 1222 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4073 "parser.cc"
    break;

  case 317: // list: texp "|" squals
#line 1223 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4079 "parser.cc"
    break;

  case 318: // lexps: lexps "," texp
#line 1225 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4085 "parser.cc"
    break;

  case 319: // lexps: texp "," texp
#line 1226 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4091 "parser.cc"
    break;

  case 320: // squals: squals "," qual
#line 1239 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4097 "parser.cc"
    break;

  case 321: // squals: qual
#line 1241 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4103 "parser.cc"
    break;

  case 322: // guardquals: guardquals1
#line 1251 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4109 "parser.cc"
    break;

  case 323: // guardquals1: guardquals1 "," qual
#line 1253 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4115 "parser.cc"
    break;

  case 324: // guardquals1: qual
#line 1254 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4121 "parser.cc"
    break;

  case 325: // altslist: "{" alts "}"
#line 1257 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4127 "parser.cc"
    break;

  case 326: // altslist: "vocurly" alts close
#line 1258 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4133 "parser.cc"
    break;

  case 327: // altslist: "{" "}"
#line 1259 "parser.y"
                                 {}
#line 4139 "parser.cc"
    break;

  case 328: // altslist: "vocurly" close
#line 1260 "parser.y"
                                 {}
#line 4145 "parser.cc"
    break;

  case 329: // alts: alts1
#line 1262 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4151 "parser.cc"
    break;

  case 330: // alts: ";" alts
#line 1263 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4157 "parser.cc"
    break;

  case 331: // alts1: alts1 ";" alt
#line 1265 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4163 "parser.cc"
    break;

  case 332: // alts1: alts1 ";"
#line 1266 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4169 "parser.cc"
    break;

  case 333: // alts1: alt
#line 1267 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4175 "parser.cc"
    break;

  case 334: // alt: pat alt_rhs
#line 1269 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4181 "parser.cc"
    break;

  case 335: // alt_rhs: "->" exp wherebinds
#line 1271 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4187 "parser.cc"
    break;

  case 336: // alt_rhs: gdpats wherebinds
#line 1272 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4193 "parser.cc"
    break;

  case 337: // gdpats: gdpats gdpat
#line 1274 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4199 "parser.cc"
    break;

  case 338: // gdpats: gdpat
#line 1275 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4205 "parser.cc"
    break;

  case 339: // gdpat: "|" guardquals "->" exp
#line 1284 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4211 "parser.cc"
    break;

  case 340: // pat: exp
#line 1286 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4217 "parser.cc"
    break;

  case 341: // bindpat: exp
#line 1288 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4223 "parser.cc"
    break;

  case 342: // apat: aexp
#line 1290 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4229 "parser.cc"
    break;

  case 343: // apats1: apats1 apat
#line 1292 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4235 "parser.cc"
    break;

  case 344: // apats1: apat
#line 1293 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4241 "parser.cc"
    break;

  case 345: // stmtlist: "{" stmts "}"
#line 1296 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4247 "parser.cc"
    break;

  case 346: // stmtlist: "vocurly" stmts close
#line 1297 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4253 "parser.cc"
    break;

  case 347: // stmts: stmts ";" stmt
#line 1299 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4259 "parser.cc"
    break;

  case 348: // stmts: stmts ";"
#line 1300 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4265 "parser.cc"
    break;

  case 349: // stmts: stmt
#line 1301 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4271 "parser.cc"
    break;

  case 350: // stmts: %empty
#line 1302 "parser.y"
                       {}
#line 4277 "parser.cc"
    break;

  case 351: // stmt: qual
#line 1307 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4283 "parser.cc"
    break;

  case 352: // stmt: "rec" stmtlist
#line 1308 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4289 "parser.cc"
    break;

  case 353: // qual: bindpat "<-" exp
#line 1310 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4295 "parser.cc"
    break;

  case 354: // qual: exp
#line 1311 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4301 "parser.cc"
    break;

  case 355: // qual: "let" binds
#line 1312 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4307 "parser.cc"
    break;

  case 356: // fbinds: fbinds1
#line 1317 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4313 "parser.cc"
    break;

  case 357: // fbinds: %empty
#line 1318 "parser.y"
                        {}
#line 4319 "parser.cc"
    break;

  case 358: // fbinds1: fbind "," fbinds1
#line 1320 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4325 "parser.cc"
    break;

  case 359: // fbinds1: fbind
#line 1321 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4331 "parser.cc"
    break;

  case 360: // fbinds1: ".."
#line 1322 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4337 "parser.cc"
    break;

  case 361: // fbind: qvar "=" texp
#line 1324 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4343 "parser.cc"
    break;

  case 362: // fbind: qvar
#line 1325 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4349 "parser.cc"
    break;

  case 363: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1326 "parser.y"
                                                      {}
#line 4355 "parser.cc"
    break;

  case 364: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1327 "parser.y"
                                                      {}
#line 4361 "parser.cc"
    break;

  case 367: // qcon: gen_qcon
#line 1363 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4367 "parser.cc"
    break;

  case 368: // qcon: sysdcon
#line 1364 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4373 "parser.cc"
    break;

  case 369: // gen_qcon: qconid
#line 1366 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4379 "parser.cc"
    break;

  case 370: // gen_qcon: "(" qconsym ")"
#line 1367 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4385 "parser.cc"
    break;

  case 371: // con: conid
#line 1369 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4391 "parser.cc"
    break;

  case 372: // con: "(" consym ")"
#line 1370 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4397 "parser.cc"
    break;

  case 373: // con: sysdcon
#line 1371 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4403 "parser.cc"
    break;

  case 374: // con_list: con_list "," con
#line 1373 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4409 "parser.cc"
    break;

  case 375: // con_list: con
#line 1374 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4415 "parser.cc"
    break;

  case 376: // sysdcon_no_list: "(" ")"
#line 1376 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4421 "parser.cc"
    break;

  case 377: // sysdcon_no_list: "(" commas ")"
#line 1377 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4427 "parser.cc"
    break;

  case 378: // sysdcon_no_list: "(#" "#)"
#line 1378 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4433 "parser.cc"
    break;

  case 379: // sysdcon_no_list: "(#" commas "#)"
#line 1379 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4439 "parser.cc"
    break;

  case 380: // sysdcon: sysdcon_no_list
#line 1381 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4445 "parser.cc"
    break;

  case 381: // sysdcon: "[" "]"
#line 1382 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4451 "parser.cc"
    break;

  case 382: // conop: consym
#line 1384 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4457 "parser.cc"
    break;

  case 383: // conop: "`" conid "`"
#line 1385 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4463 "parser.cc"
    break;

  case 384: // qconop: qconsym
#line 1387 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4469 "parser.cc"
    break;

  case 385: // qconop: "`" qconid "`"
#line 1388 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4475 "parser.cc"
    break;

  case 386: // gtycon: ntgtycon
#line 1391 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4481 "parser.cc"
    break;

  case 387: // gtycon: "(" ")"
#line 1392 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4487 "parser.cc"
    break;

  case 388: // gtycon: "(#" "#)"
#line 1393 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4493 "parser.cc"
    break;

  case 389: // ntgtycon: oqtycon
#line 1395 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4499 "parser.cc"
    break;

  case 390: // ntgtycon: "(" commas ")"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4505 "parser.cc"
    break;

  case 391: // ntgtycon: "(#" commas "#)"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4511 "parser.cc"
    break;

  case 392: // ntgtycon: "(" "->" ")"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4517 "parser.cc"
    break;

  case 393: // ntgtycon: "[" "]"
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4523 "parser.cc"
    break;

  case 394: // oqtycon: qtycon
#line 1401 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4529 "parser.cc"
    break;

  case 395: // oqtycon: "(" qtyconsym ")"
#line 1402 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4535 "parser.cc"
    break;

  case 396: // oqtycon_no_varcon: qtycon
#line 1404 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4541 "parser.cc"
    break;

  case 397: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1405 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4547 "parser.cc"
    break;

  case 398: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1406 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4553 "parser.cc"
    break;

  case 399: // oqtycon_no_varcon: "(" ":" ")"
#line 1407 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4559 "parser.cc"
    break;

  case 400: // qtyconop: qtyconsym
#line 1410 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4565 "parser.cc"
    break;

  case 401: // qtyconop: "`" qtycon "`"
#line 1411 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4571 "parser.cc"
    break;

  case 402: // qtycondoc: qtycon
#line 1413 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4577 "parser.cc"
    break;

  case 403: // qtycon: "QCONID"
#line 1415 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4583 "parser.cc"
    break;

  case 404: // qtycon: tycon
#line 1416 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4589 "parser.cc"
    break;

  case 405: // tycon: "CONID"
#line 1420 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4595 "parser.cc"
    break;

  case 406: // qtyconsym: "QCONSYM"
#line 1422 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4601 "parser.cc"
    break;

  case 407: // qtyconsym: "QVARSYM"
#line 1423 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4607 "parser.cc"
    break;

  case 408: // qtyconsym: tyconsym
#line 1424 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4613 "parser.cc"
    break;

  case 409: // tyconsym: "CONSYM"
#line 1426 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4619 "parser.cc"
    break;

  case 410: // tyconsym: "VARSYM"
#line 1427 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4625 "parser.cc"
    break;

  case 411: // tyconsym: ":"
#line 1428 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4631 "parser.cc"
    break;

  case 412: // tyconsym: "-"
#line 1429 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4637 "parser.cc"
    break;

  case 413: // op: varop
#line 1434 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4643 "parser.cc"
    break;

  case 414: // op: conop
#line 1435 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4649 "parser.cc"
    break;

  case 415: // varop: varsym
#line 1437 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4655 "parser.cc"
    break;

  case 416: // varop: "`" varid "`"
#line 1438 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4661 "parser.cc"
    break;

  case 417: // qop: qvarop
#line 1440 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4667 "parser.cc"
    break;

  case 418: // qop: qconop
#line 1441 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4673 "parser.cc"
    break;

  case 419: // qopm: qvaropm
#line 1444 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4679 "parser.cc"
    break;

  case 420: // qopm: qconop
#line 1445 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4685 "parser.cc"
    break;

  case 421: // qvarop: qvarsym
#line 1450 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4691 "parser.cc"
    break;

  case 422: // qvarop: "`" qvarid "`"
#line 1451 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4697 "parser.cc"
    break;

  case 423: // qvaropm: qvarsym_no_minus
#line 1453 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4703 "parser.cc"
    break;

  case 424: // qvaropm: "`" qvarid "`"
#line 1454 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4709 "parser.cc"
    break;

  case 425: // tyvar: tyvarid
#line 1458 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4715 "parser.cc"
    break;

  case 426: // tyvarop: "`" tyvarid "`"
#line 1460 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4721 "parser.cc"
    break;

  case 427: // tyvarid: "VARID"
#line 1462 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4727 "parser.cc"
    break;

  case 428: // tyvarid: special_id
#line 1463 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4733 "parser.cc"
    break;

  case 429: // tyvarid: "unsafe"
#line 1464 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4739 "parser.cc"
    break;

  case 430: // tyvarid: "safe"
#line 1465 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4745 "parser.cc"
    break;

  case 431: // tyvarid: "interruptible"
#line 1466 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4751 "parser.cc"
    break;

  case 432: // var: varid
#line 1469 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4757 "parser.cc"
    break;

  case 433: // var: "(" varsym ")"
#line 1470 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4763 "parser.cc"
    break;

  case 434: // qvar: qvarid
#line 1472 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4769 "parser.cc"
    break;

  case 435: // qvar: "(" varsym ")"
#line 1473 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4775 "parser.cc"
    break;

  case 436: // qvar: "(" qvarsym1 ")"
#line 1474 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4781 "parser.cc"
    break;

  case 437: // field: varid
#line 1476 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4787 "parser.cc"
    break;

  case 438: // qvarid: varid
#line 1478 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4793 "parser.cc"
    break;

  case 439: // qvarid: "QVARID"
#line 1479 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4799 "parser.cc"
    break;

  case 440: // varid: "VARID"
#line 1481 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4805 "parser.cc"
    break;

  case 441: // varid: special_id
#line 1482 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4811 "parser.cc"
    break;

  case 442: // varid: "unsafe"
#line 1483 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4817 "parser.cc"
    break;

  case 443: // varid: "safe"
#line 1484 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4823 "parser.cc"
    break;

  case 444: // varid: "interruptible"
#line 1485 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4829 "parser.cc"
    break;

  case 445: // varid: "forall"
#line 1486 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4835 "parser.cc"
    break;

  case 446: // varid: "family"
#line 1487 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4841 "parser.cc"
    break;

  case 447: // varid: "role"
#line 1488 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4847 "parser.cc"
    break;

  case 448: // qvarsym: varsym
#line 1490 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4853 "parser.cc"
    break;

  case 449: // qvarsym: qvarsym1
#line 1491 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4859 "parser.cc"
    break;

  case 450: // qvarsym_no_minus: varsym_no_minus
#line 1493 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4865 "parser.cc"
    break;

  case 451: // qvarsym_no_minus: qvarsym1
#line 1494 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4871 "parser.cc"
    break;

  case 452: // qvarsym1: "QVARSYM"
#line 1496 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4877 "parser.cc"
    break;

  case 453: // varsym: varsym_no_minus
#line 1498 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4883 "parser.cc"
    break;

  case 454: // varsym: "-"
#line 1499 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4889 "parser.cc"
    break;

  case 455: // varsym_no_minus: "VARSYM"
#line 1501 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4895 "parser.cc"
    break;

  case 456: // varsym_no_minus: special_sym
#line 1502 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4901 "parser.cc"
    break;

  case 457: // special_id: "as"
#line 1504 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4907 "parser.cc"
    break;

  case 458: // special_id: "qualified"
#line 1505 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4913 "parser.cc"
    break;

  case 459: // special_id: "hiding"
#line 1506 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4919 "parser.cc"
    break;

  case 460: // special_id: "export"
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4925 "parser.cc"
    break;

  case 461: // special_id: "label"
#line 1508 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4931 "parser.cc"
    break;

  case 462: // special_id: "dynamic"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4937 "parser.cc"
    break;

  case 463: // special_id: "stdcall"
#line 1510 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4943 "parser.cc"
    break;

  case 464: // special_id: "ccall"
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4949 "parser.cc"
    break;

  case 465: // special_id: "capi"
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4955 "parser.cc"
    break;

  case 466: // special_id: "prim"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4961 "parser.cc"
    break;

  case 467: // special_id: "javascript"
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4967 "parser.cc"
    break;

  case 468: // special_id: "group"
#line 1515 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4973 "parser.cc"
    break;

  case 469: // special_id: "stock"
#line 1516 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4979 "parser.cc"
    break;

  case 470: // special_id: "anyclass"
#line 1517 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4985 "parser.cc"
    break;

  case 471: // special_id: "via"
#line 1518 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4991 "parser.cc"
    break;

  case 472: // special_id: "unit"
#line 1519 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4997 "parser.cc"
    break;

  case 473: // special_id: "dependency"
#line 1520 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5003 "parser.cc"
    break;

  case 474: // special_id: "signature"
#line 1521 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5009 "parser.cc"
    break;

  case 475: // special_sym: "."
#line 1523 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5015 "parser.cc"
    break;

  case 476: // special_sym: "*"
#line 1524 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5021 "parser.cc"
    break;

  case 477: // qconid: conid
#line 1528 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5027 "parser.cc"
    break;

  case 478: // qconid: "QCONID"
#line 1529 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5033 "parser.cc"
    break;

  case 479: // conid: "CONID"
#line 1531 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5039 "parser.cc"
    break;

  case 480: // qconsym: consym
#line 1533 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5045 "parser.cc"
    break;

  case 481: // qconsym: "QCONSYM"
#line 1534 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5051 "parser.cc"
    break;

  case 482: // consym: "CONSYM"
#line 1536 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5057 "parser.cc"
    break;

  case 483: // consym: ":"
#line 1537 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5063 "parser.cc"
    break;

  case 484: // literal: "CHAR"
#line 1541 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5069 "parser.cc"
    break;

  case 485: // literal: "STRING"
#line 1542 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5075 "parser.cc"
    break;

  case 486: // literal: "INTEGER"
#line 1543 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5081 "parser.cc"
    break;

  case 487: // literal: "RATIONAL"
#line 1544 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 5087 "parser.cc"
    break;

  case 488: // literal: "PRIMINTEGER"
#line 1545 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5093 "parser.cc"
    break;

  case 490: // close: error
#line 1553 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5099 "parser.cc"
    break;

  case 491: // modid: "CONID"
#line 1557 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5105 "parser.cc"
    break;

  case 492: // modid: "QCONID"
#line 1558 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5111 "parser.cc"
    break;

  case 493: // commas: commas ","
#line 1560 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5117 "parser.cc"
    break;

  case 494: // commas: ","
#line 1561 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5123 "parser.cc"
    break;


#line 5127 "parser.cc"

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


  const short parser::yypact_ninf_ = -642;

  const short parser::yytable_ninf_ = -454;

  const short
  parser::yypact_[] =
  {
      68,   240,  -642,    87,  -642,  -642,  -642,  -642,  -642,   146,
      13,   -40,  -642,    56,   -28,   -28,    77,  -642,  -642,  -642,
    -642,   223,  -642,  -642,  -642,    35,  -642,   170,   177,  4419,
     256,   281,   210,  -642,   854,  -642,   -27,  -642,  -642,  -642,
    -642,   240,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,
    -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,
    -642,  -642,  -642,  -642,   426,  -642,  -642,  -642,  -642,   220,
     236,  -642,   246,  -642,  -642,  -642,  -642,  -642,  -642,  -642,
      -5,  -642,   240,  -642,   248,  -642,  2368,  4314,   334,   296,
     132,  2368,  -642,  -642,  -642,   376,   324,  -642,  3476,   401,
     132,  3048,   314,  4658,   190,  3048,  3048,  2640,  3048,  1688,
    1552,   266,  -642,  -642,  -642,  -642,  -642,  -642,  -642,    25,
     314,   303,   210,  -642,  -642,  -642,  -642,   370,    98,  -642,
    -642,   612,  -642,  2776,  -642,    78,  -642,  -642,  -642,  -642,
    -642,  -642,   358,   101,  -642,  -642,  -642,  -642,   331,  -642,
     372,  -642,  -642,  -642,  -642,   389,  -642,   396,   399,   403,
    -642,  -642,  -642,  4419,  4456,  -642,  -642,  -642,  -642,   465,
    -642,  1552,   459,   411,  -642,  -642,  -642,  4314,  4314,  -642,
    4757,  3577,  3160,   373,  -642,   462,   417,  -642,   705,  -642,
    3764,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  4314,  3865,
    2096,  2096,  -642,   407,   440,   444,   449,   451,  3865,  1280,
    1280,  -642,   515,  4314,  4314,   108,   452,   835,   114,   491,
    -642,  -642,   -18,  4658,  -642,   283,   -19,   428,   131,  -642,
     115,  -642,  -642,  -642,  -642,  2912,  -642,  2776,  -642,  -642,
    -642,  4557,  -642,  -642,  -642,   411,    18,   432,   429,  -642,
    2368,  -642,  -642,  -642,  -642,  -642,  -642,  4794,  -642,  -642,
     252,   165,   255,   399,   436,   439,   446,   269,  -642,   279,
    3865,  4658,  4658,  -642,   277,   248,   420,  4314,  3865,  4757,
    2368,  2504,  4557,  -642,    20,  -642,  -642,  2368,  -642,  -642,
    -642,  -642,  4314,  -642,  4794,  4621,  3048,  -642,  -642,  -642,
    -642,  -642,  -642,  -642,   447,   448,   445,  -642,   458,    56,
     240,    52,   328,  3865,  -642,  -642,   333,   126,   460,   450,
    -642,  -642,  -642,  -642,   461,   488,   483,  -642,  -642,   463,
    -642,  -642,  -642,  -642,  -642,  -642,   464,   456,   470,  -642,
     297,   302,   338,  -642,  4314,  3865,   727,  4314,  -642,  -642,
    -642,  4314,  -642,  -642,   504,  -642,   473,   468,   324,   132,
     503,   506,   -21,  -642,  -642,    34,  -642,   564,  -642,  -642,
    -642,  -642,  -642,  -642,   568,   130,  -642,  -642,   612,    38,
    2368,  -642,   519,   393,  3865,   154,  3865,   471,   467,   502,
     536,  -642,   537,   505,   234,   190,   538,  2368,  -642,   496,
     499,  2368,  2368,  2504,  1824,  -642,  1824,   707,  -642,  -642,
    4794,  -642,  -642,  1824,  -642,  1824,   134,  -642,  -642,  -642,
    -642,   544,   548,   552,  4720,   517,  -642,  -642,  -642,  -642,
    -642,   -10,   395,  -642,  -642,  -642,  -642,   597,   553,   518,
    -642,   522,   324,  -642,  -642,  -642,  -642,  -642,  -642,   540,
    -642,   532,   572,   554,   555,  -642,  -642,  -642,  4520,  -642,
    -642,  -642,   542,  4419,  -642,  -642,  1960,  1416,  -642,  -642,
     546,  3865,  -642,  4757,  4850,  -642,  3865,  3865,  -642,  -642,
    3865,  -642,  -642,  -642,  1008,  1008,  -642,  -642,  -642,   541,
     545,   215,  -642,  3865,  -642,  -642,  3865,   515,  -642,  2368,
    -642,  2096,  -642,  2368,   348,  -642,  -642,  1280,  -642,  -642,
    3865,  3865,  4963,   573,  -642,  -642,     0,  -642,  -642,  4757,
     558,  -642,  -642,  -642,  -642,   562,    94,   312,  -642,  -642,
    -642,  -642,  -642,  -642,  -642,  -642,   543,  -642,   586,  -642,
    -642,  -642,  -642,  -642,  -642,  3865,  3865,   559,   560,   277,
    -642,   596,  3865,   646,   654,   673,  -642,  2368,  2504,  -642,
    -642,  -642,  4621,  1824,  4794,  -642,  4419,   571,  -642,  2232,
    -642,   581,   576,  -642,   369,    56,  -642,  -642,  -642,  -642,
    3865,  5049,  5049,  -642,  -642,  -642,  -642,  -642,   585,   665,
    3678,  -642,  -642,   157,  -642,    41,  -642,  -642,  -642,  -642,
    -642,  -642,   407,  1144,  1144,  -642,  -642,  -642,  -642,  -642,
    5049,   677,   626,  -642,  -642,  -642,  2504,  2368,  -642,    11,
      23,  -642,  -642,  -642,  -642,  -642,  -642,   625,  -642,  4314,
     356,   673,   164,  -642,   673,  -642,  -642,  -642,  -642,  -642,
     151,  -642,   599,  -642,  -642,  -642,  2368,  2504,  2368,  -642,
      36,  -642,  -642,  -642,     1,   630,  -642,  -642,  4314,  4314,
    4314,  -642,   400,  -642,  1008,  -642,   702,   695,  -642,  -642,
     229,  -642,    51,  -642,   631,   366,  -642,  3865,  -642,  -642,
    -642,  3865,  -642,  4887,   646,   628,  3266,  -642,  -642,  -642,
     407,   407,  -642,  -642,  -642,  -642,  3951,   218,   663,  -642,
    -642,  -642,  1824,  4794,  -642,  -642,   634,   597,  -642,  -642,
    3865,  -642,  3865,   504,  -642,   416,  3865,  4056,  -642,  -642,
    2368,  -642,  4314,  -642,  1144,  -642,  5049,  4142,  4228,  -642,
    -642,  -642,  -642,  -642,  4314,   598,  -642,  4314,   232,  -642,
     190,    53,  -642,  -642,   603,   613,  -642,  4314,  -642,  -642,
    -642,  2368,  -642,   620,   616,  -642,  4996,  -642,  -642,  3160,
     649,   651,  -642,  -642,  -642,  5049,  -642,   637,   233,  -642,
      56,    59,  3371,  -642,  4314,  -642,   407,   144,  -642,  4314,
    -642,  -642,  -642,  -642,  -642,   630,  5049,  -642,  -642,  -642,
    4314,  -642,  -642,  -642,  3865,  -642,  -642,  -642,  -642
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   491,   492,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   490,   489,    12,   163,   159,     0,     0,     0,
       0,    42,    37,    15,    14,   162,     0,     6,     7,   457,
     459,     0,   458,   445,   460,   461,   462,   443,   444,   442,
     446,   447,   463,   464,   465,   466,   467,   468,   469,   470,
     471,   472,   474,   473,     0,   440,   405,   439,   403,     0,
      19,    21,    24,    32,   396,   404,    31,   434,   438,   441,
       0,    41,     0,    34,    38,   303,     0,     0,   113,     0,
       0,     0,    51,    52,    53,    82,     0,   114,     0,     0,
       0,     0,   267,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   479,   478,   484,   485,   486,   487,   488,   267,
     267,    49,    56,    59,    60,    61,    62,   129,     0,    65,
     250,    66,   273,   276,   282,   292,   295,   297,   367,   380,
     368,   169,   296,   438,   369,   477,   298,   160,     0,    23,
       0,   454,   476,   475,   455,     0,   452,     0,     0,     0,
     453,   456,    17,     0,    27,    22,    36,    36,     3,    44,
      33,     0,     0,   272,   430,   431,   429,     0,     0,   197,
     235,     0,     0,     0,   427,   139,     0,   126,   180,   183,
     184,   188,   195,   389,   394,   196,   425,   428,     0,   209,
     350,   350,   290,   278,     0,     0,     0,     0,     0,   153,
     153,   156,     0,     0,     0,     0,     0,   180,   389,     0,
     291,   281,     0,     0,   268,     0,     0,     0,     0,   375,
     164,   373,   371,   342,   344,     0,   284,   275,   285,   483,
     381,     0,   482,   481,   306,   272,   311,     0,   312,   420,
       0,   419,   423,   451,   450,   384,   480,     0,   376,   494,
       0,     0,     0,   451,     0,   450,   384,     0,   378,     0,
       0,     0,     0,    50,     0,    57,     0,     0,     0,     0,
       0,     0,     0,   251,   158,   256,   418,     0,   417,   421,
     449,   448,     0,   279,     0,   357,     0,   161,   399,   398,
     397,   436,   435,    20,     0,     0,    28,    30,     0,     0,
       0,    46,     0,     0,   199,   198,     0,     0,     0,   236,
     238,   432,   213,   393,     0,   172,     0,   176,   411,     0,
     412,   201,   410,   409,   407,   406,   210,     0,     0,   408,
       0,     0,     0,    67,     0,     0,     0,     0,   192,   400,
     193,     0,   189,   191,   117,   210,     0,   208,     0,     0,
     354,     0,     0,   349,   351,     0,   277,     0,    79,    78,
      80,    81,   205,   166,   149,     0,   252,   152,     0,     0,
       0,    77,     0,   119,     0,     0,     0,     0,     0,     0,
       0,   262,     0,     0,     0,     0,     0,     0,   343,     0,
       0,   307,   313,     0,     0,   302,     0,   308,   305,   437,
       0,   301,   299,     0,   300,     0,   435,   370,   377,   493,
     379,     0,     0,     0,     0,   259,   414,    55,   413,   415,
     382,     0,   115,   258,   177,   167,   168,   158,     0,   322,
     324,     0,     0,   254,   255,   274,   280,   294,   360,     0,
     356,   359,   362,     0,   438,   283,    26,    25,     0,     9,
      10,    43,     0,     0,    40,    45,     0,     0,   289,   271,
       0,     0,   200,     0,     0,   203,     0,     0,   392,   204,
       0,   395,   390,   391,   135,   135,   138,   125,   181,     0,
       0,   185,   190,     0,    72,    63,     0,   355,   352,     0,
     345,   348,   346,     0,     0,    76,   154,   151,   155,   287,
       0,     0,     0,    87,   219,    73,     0,    74,    68,     0,
       0,   269,   261,   263,   372,     0,     0,     0,   165,   386,
     374,   260,   286,   424,   385,   315,   317,   321,   306,   319,
     318,   304,   310,   309,   266,     0,     0,     0,     0,     0,
     128,     0,     0,   232,   222,   240,   253,     0,     0,   422,
     157,   293,     0,     0,     0,    29,     0,     0,   327,     0,
     340,     0,   329,   333,     0,     0,   328,   433,   239,   237,
       0,     0,     0,   212,   214,   217,   173,   175,   211,   108,
       0,   130,   134,     0,   131,     0,   401,   426,   118,   211,
     353,   347,   278,   145,   145,   148,   150,   102,   120,   121,
       0,    92,     0,   270,   387,   388,     0,   314,   170,     0,
       0,   416,   383,    54,   127,   116,   213,   226,   228,     0,
       0,   240,     0,    69,   241,   243,   257,   323,   358,   361,
     364,   366,     0,    47,   330,   325,   332,     0,     0,   334,
     158,   338,   326,   174,     0,     0,   202,   109,     0,     0,
       0,   106,   122,   136,   133,   137,     0,   110,   140,   144,
       0,   141,     0,    88,     0,     0,    71,     0,   320,   316,
     264,     0,   265,     0,   232,     0,   233,   186,   194,   230,
     278,   278,    70,    85,    83,    84,     0,     0,   244,   247,
     402,   242,     0,     0,    48,   331,     0,   158,   336,   337,
       0,   215,     0,   117,   107,   122,     0,     0,   104,   132,
       0,   111,     0,   146,   143,   147,     0,   101,   101,    93,
      64,   171,   231,   227,     0,     0,   187,     0,     0,   224,
       0,     0,   248,   182,   206,     0,   245,     0,   246,   363,
     365,     0,   335,     0,     0,   103,     0,   105,   123,     0,
       0,   196,   288,   112,   142,    89,    91,     0,     0,   100,
       0,     0,   233,   229,   234,   220,   278,     0,   221,     0,
     249,    86,   339,   216,   218,   196,     0,    90,    96,    94,
      99,    97,    95,   223,     0,   207,   124,    98,   225
  };

  const short
  parser::yypgoto_[] =
  {
    -642,  -642,  -642,  -642,  -642,  -642,  -642,    26,  -642,  -642,
    -401,  -642,   580,  -642,  -642,  -642,  -143,   622,  -642,  -642,
    -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,
    -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,  -642,
    -642,   -41,  -642,  -642,  -642,    19,  -212,  -642,  -642,  -642,
    -642,  -642,  -642,    33,  -642,    39,   472,  -642,    84,   265,
    -642,  -642,    31,   152,  -642,  -642,   563,  -642,  -310,  -407,
     748,  -642,  -642,  -306,    88,  -162,   222,  -144,    47,  -642,
     -46,  -642,   -79,  -642,   -52,  -642,  -452,  -642,  -642,  -642,
    -636,  -152,   507,    -7,  -642,   577,   149,   276,  -487,  -442,
    -642,    99,    16,  -642,  -642,   109,  -642,    60,  -642,  -642,
     322,   166,  -642,   167,   102,   768,  -204,  -642,  -642,   520,
    -642,   379,  -642,   286,    -1,  -251,  -189,   698,   -35,  -642,
    -642,  -642,   -92,  -642,  -642,  -642,  -642,   160,  -642,  -642,
    -402,  -642,   162,  -642,  -642,   159,  -642,  -642,   578,  -642,
     -80,   610,   311,  -252,  -642,   253,  -642,  -642,  -642,  -642,
     419,    76,  -642,   -94,  -641,   -88,  -642,   423,   -75,  -642,
    -642,  -642,   -20,  -642,  -164,  -642,   270,  -642,   575,  -642,
    -642,  -642,  -397,  -642,  -330,  -247,   168,  -235,  -128,    -6,
    -642,  -642,   -24,   -49,   -97,   -87,  -642,   -86,  -102,   -54,
    -225,  -642,  -226,   -14,  -107
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   168,     6,    10,    19,    30,
      69,    70,    71,   165,   305,   306,    72,    84,    11,    20,
      21,    32,    82,   311,   464,   465,   274,   121,   425,    33,
      34,   122,   123,   124,   125,   215,   126,   208,   697,   748,
     611,   673,   765,   676,   729,   768,   769,   591,   658,   722,
     668,   127,   554,   494,   513,   718,   185,   277,   592,   593,
     486,   343,   669,   670,   605,   505,   375,   211,   212,   443,
      27,    36,   396,   372,   433,   128,   619,   336,   514,   435,
     326,   685,   327,   744,   188,   189,   686,   190,   352,   347,
     687,   191,   374,   745,   356,   337,   474,   583,   584,   515,
     631,   738,   739,   555,   627,   628,   629,   689,   318,   319,
     320,   633,   634,   635,   698,   376,   594,   283,   284,   285,
     130,   223,   224,   244,   173,   132,   740,   133,   134,   135,
     136,   260,   261,   262,   247,   248,   536,   438,   439,   468,
     571,   572,   573,   649,   650,   651,   574,   361,   234,   235,
     202,   362,   363,   364,   449,   450,   451,   640,   137,   138,
     229,   230,   139,   140,   426,   249,   528,   192,   193,    73,
     348,   699,   194,    75,   338,   339,   427,   428,   287,   250,
     288,   251,   195,   350,   196,   141,   142,   453,    77,    78,
     289,   252,   253,   291,   160,    79,   161,   144,   145,   255,
     256,   146,    24,     9,   267
  };

  const short
  parser::yytable_[] =
  {
     197,   381,   232,   393,   269,   377,   377,   469,   187,    74,
     231,   197,   254,   265,   367,   159,   490,   246,   317,   216,
     220,   307,   408,   218,   349,   314,   315,   149,   143,   440,
     556,    13,   436,   131,   586,    22,   445,   324,   353,    22,
     158,   186,    22,   286,   270,   737,   217,   442,   497,   430,
     736,   598,    22,   349,    22,   355,   266,    22,   391,   447,
      22,   264,   567,   442,   462,   575,   221,   550,   169,   608,
     233,   236,   388,   238,   265,   340,   341,   585,    18,   328,
      25,   710,   147,   460,   500,   286,   263,    12,   680,     1,
     197,   197,   148,   330,   197,   197,   402,   501,   293,   166,
     682,   167,   403,   197,   281,    26,   711,   290,   245,   245,
     625,   197,   197,   399,   392,   585,   389,   266,    17,   354,
     647,   197,   159,   551,   332,   333,   197,   197,   334,   335,
     681,   737,   560,   222,   382,   383,   736,   404,   736,   502,
     446,    23,   681,    74,    74,    23,   217,   263,    23,   290,
     445,   537,   501,   508,   441,   400,   507,   286,    23,   664,
      23,   217,   217,    23,   463,   642,    23,   644,    35,   724,
     245,   776,     2,   328,   321,   541,   159,   790,   278,   294,
     329,  -432,   295,   197,   654,   655,   693,   330,   384,    29,
     197,   197,   308,   309,   -75,   394,   400,    76,   187,   492,
     233,   158,   293,   143,   143,   197,   471,   614,   378,   378,
     239,    14,    15,   259,  -433,   694,   695,   279,   332,   333,
    -432,   290,   334,   335,   794,   429,   197,   385,   325,   325,
     760,   186,   702,   -75,   395,   506,   200,    31,   201,   618,
     618,   576,   518,   708,   258,   279,   325,    37,   507,   407,
     259,   409,   703,  -433,    38,   373,   242,   197,   197,   197,
     197,   455,   663,   395,   197,   487,   516,   470,   753,   760,
     754,   226,   612,   321,   758,   664,   696,    66,   412,   498,
     674,    68,   772,    80,   413,   774,   585,    66,   409,   454,
     286,    68,   217,   232,   328,   491,   461,   197,   227,   197,
     752,   231,   228,   606,   111,    81,   637,   254,   330,   254,
     517,   317,   539,   112,   540,   565,   254,   373,   254,   286,
     761,   542,   548,   543,   430,   434,   489,   349,    83,   641,
     696,    76,    76,   162,   723,   346,   588,   775,   789,   332,
     333,    66,   525,   334,   335,    68,   526,   724,   527,   652,
     776,   790,   599,   410,   290,   163,   239,    66,   164,   585,
     373,    68,   785,     7,   678,   411,   170,     8,   414,   665,
     151,   198,   172,   152,   415,   731,   151,   203,   661,   152,
     153,   268,   418,   290,   197,   259,   153,   197,   419,   197,
     197,   390,   488,   197,   420,   440,   766,   424,   419,   671,
     671,   154,   242,   245,   409,   245,   197,   154,   199,   197,
     482,   156,   245,   666,   245,   219,   419,   483,   547,   340,
     341,   419,   222,   197,   197,   197,   151,   615,   209,   152,
     210,   259,   466,   325,   467,   787,   153,   273,    74,   422,
     423,   276,   484,    74,   485,   296,   725,   714,   204,   205,
     206,   207,   603,   647,   604,   648,   674,   154,   197,   197,
     690,   430,   691,   452,   297,   197,   254,   321,   750,   310,
     727,   639,   728,   511,   512,   552,   553,   688,   143,   143,
     716,   717,   312,   378,   378,   298,   360,   360,   798,   342,
     239,   313,   259,   197,   197,   197,   716,   756,   271,   272,
     429,   143,   299,   197,   151,   150,   378,   152,   344,   300,
     763,   662,   301,   321,   153,   778,   302,   368,   578,   151,
     671,   369,   152,   197,   587,   366,   370,   325,   371,   153,
     380,   282,   387,   386,   688,   154,   242,   240,   217,   156,
     243,   405,   197,   325,   791,   792,    74,   430,   406,   416,
     154,   155,  -453,   431,   156,   157,   454,   607,   409,   417,
     456,   457,   245,   459,   458,   472,   437,   360,   476,   473,
     475,   197,   197,   197,   477,   480,   478,   479,   797,   713,
     382,   715,   688,   481,   493,   688,   495,   496,  -341,   503,
     197,   499,   373,   373,   197,   504,   197,   143,   143,   197,
     510,   520,   378,   378,   519,   254,   217,   217,   217,   197,
     749,   521,   700,   522,   523,   531,   533,   743,   524,   534,
     688,   544,   688,   197,   442,   197,    76,   653,   545,   197,
     197,    76,   546,   548,   557,   197,   549,   558,   232,   197,
     197,   197,   559,   382,   217,   561,   231,   197,   382,   382,
     197,   562,   340,   563,   566,   564,  -437,   610,   143,   577,
     197,   596,   616,   378,   617,   597,   509,   613,   781,   197,
     217,   323,   197,   624,   626,   217,   217,   700,   197,   621,
     622,   630,   632,   532,   643,   197,   645,   197,   535,   360,
     538,   239,   197,   280,   646,   217,   281,   409,   656,   197,
     743,   245,   657,   197,   675,   151,   677,   197,   152,   684,
     712,   382,   704,   720,   721,   153,   747,   726,   143,   734,
     751,   112,   779,   378,   434,   783,   780,   217,   373,   784,
     452,    39,   282,   786,    76,  -217,   154,   242,   217,    40,
     156,   243,   788,   303,   275,   796,   755,   771,   719,   432,
     595,    42,   570,   570,   757,   764,   672,    44,    45,    46,
     174,   175,   176,    28,   607,   730,    52,    53,   620,    54,
      55,    56,   795,   379,    57,   683,   357,   421,    58,    59,
      60,    61,    62,    63,   328,   600,   239,   360,   609,   602,
     741,   345,   793,   733,   773,   579,  -178,   692,   330,   746,
     151,   701,   129,   152,   444,   237,   325,   706,   705,   709,
     153,   365,   601,   398,   530,   638,   777,   529,     0,   623,
     401,     0,     0,     0,     0,   346,     0,   282,     0,   332,
     333,   154,   242,   334,   335,   156,   243,     0,     0,     0,
       0,   373,     0,   636,   360,     0,     0,     0,     0,   184,
      66,     0,     0,     0,    68,   570,     0,    85,    39,    86,
      87,    88,    89,     0,    90,     0,    40,    91,     0,     0,
      92,    93,    94,    95,    96,     0,    97,     0,    42,     0,
      98,     0,    43,    99,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,   360,   679,   101,    58,    59,    60,    61,    62,
      63,   102,     0,     0,   328,     0,     0,   103,     0,     0,
       0,   345,     0,     0,     0,     0,     0,     0,   330,     0,
     104,     0,   570,   360,   707,     0,   105,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   107,     0,   108,     0,
       0,     0,     0,     0,     0,   346,     0,     0,     0,   332,
     333,     0,   109,   334,   335,     0,   110,     0,   111,     0,
       0,     0,     0,     0,     0,     0,    65,   112,     0,     0,
      67,   113,     0,     0,     0,     0,   114,   115,   116,   117,
       0,     0,   118,     0,     0,     0,   119,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   762,     0,     0,     0,
       0,    85,    39,    86,     0,   589,     0,     0,    90,     0,
      40,    91,     0,     0,    92,    93,    94,     0,    96,     0,
       0,     0,    42,     0,   590,     0,    43,   782,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,   102,     0,     0,     0,     0,
       0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   104,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,     0,   106,     0,     0,
     107,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
     110,     0,   111,     0,     0,     0,     0,     0,     0,     0,
      65,   112,     0,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
     119,   120,     0,     0,    90,     0,    40,    91,     0,     0,
      92,    93,    94,     0,    96,     0,     0,     0,    42,     0,
     667,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,   102,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     104,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   107,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,   110,     0,   111,     0,
       0,     0,     0,     0,     0,     0,    65,   112,     0,     0,
      67,   113,     0,     0,     0,     0,   114,   115,   116,   117,
       0,     0,   118,    85,    39,    86,   119,   120,     0,     0,
      90,     0,    40,    91,     0,     0,    92,    93,    94,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,   102,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   104,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,   106,
       0,     0,   107,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,   110,     0,   111,     0,     0,     0,     0,     0,
       0,     0,    65,   112,     0,     0,    67,   113,     0,     0,
       0,     0,   114,   115,   116,   117,     0,    22,   118,    85,
      39,    86,   119,   120,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,     0,   106,     0,     0,   107,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    23,   109,     0,     0,     0,   171,     0,
     111,     0,     0,     0,   569,     0,     0,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   239,     0,     0,   105,     0,     0,     0,     0,     0,
       0,   106,     0,     0,   107,   151,   108,     0,   152,     0,
       0,     0,     0,     0,   257,   153,     0,     0,     0,     0,
     109,     0,     0,     0,   171,   258,   111,     0,     0,     0,
       0,   259,   241,     0,    65,   112,   154,   242,    67,   113,
     156,   243,     0,     0,   114,   115,   116,   117,     0,     0,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   239,     0,     0,
     105,     0,     0,     0,     0,     0,     0,   106,     0,     0,
     107,     0,   108,     0,   152,     0,     0,     0,     0,     0,
       0,   153,     0,     0,     0,     0,   109,   240,     0,     0,
     171,     0,   111,     0,     0,     0,     0,     0,   241,     0,
      65,   112,   154,   242,    67,   113,   156,   243,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   239,     0,     0,   105,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   107,     0,   108,     0,
     152,     0,     0,     0,     0,     0,     0,   153,     0,     0,
       0,     0,   109,     0,     0,     0,   171,     0,   111,     0,
       0,     0,     0,     0,   241,     0,    65,   112,   154,   242,
      67,   113,   156,   243,     0,     0,   114,   115,   116,   117,
       0,     0,   118,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,   106,
       0,     0,   107,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   568,     0,     0,   109,     0,
       0,     0,   171,     0,   111,     0,     0,     0,   569,     0,
       0,     0,    65,   112,     0,     0,    67,   113,     0,     0,
       0,     0,   114,   115,   116,   117,     0,     0,   118,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,   358,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,   359,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,     0,   106,     0,     0,   107,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   171,     0,
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
       0,   106,     0,     0,   107,     0,   108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,   171,     0,   111,     0,     0,     0,
     569,     0,     0,     0,    65,   112,     0,     0,    67,   113,
       0,     0,     0,     0,   114,   115,   116,   117,     0,     0,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,     0,   106,     0,     0,
     107,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
     171,     0,   111,     0,     0,     0,     0,     0,     0,     0,
      65,   112,     0,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,   358,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   107,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,     0,   171,     0,   111,     0,
       0,     0,     0,     0,     0,     0,    65,   112,     0,     0,
      67,   113,     0,     0,     0,     0,   114,   115,   116,   117,
       0,     0,   118,    85,    39,    86,     0,     0,     0,     0,
      90,     0,    40,    91,     0,     0,     0,     0,     0,     0,
      96,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     101,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,   171,     0,   111,     0,     0,     0,     0,     0,
       0,     0,    65,   112,     0,     0,    67,   113,     0,     0,
       0,     0,   114,   115,   116,   117,     0,     0,   118,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,    96,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,   292,   106,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   171,     0,
     111,     0,     0,     0,     0,     0,     0,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   105,     0,     0,     0,   397,     0,
       0,   106,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,   171,     0,   111,     0,     0,     0,
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
     105,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
     171,     0,   111,     0,    39,     0,     0,     0,     0,     0,
      65,   112,    40,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,    42,     0,   118,     0,   322,     0,
      44,    45,    46,   174,   175,   176,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,     0,   329,     0,     0,   177,
       0,     0,     0,   330,   178,     0,   179,     0,     0,     0,
       0,     0,     0,     0,   180,     0,     0,     0,   181,     0,
      39,     0,   182,   331,   183,     0,     0,     0,    40,   259,
       0,     0,   184,    66,   332,   333,     0,    68,   334,   335,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,  -179,     0,     0,
     178,     0,   179,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   181,    39,     0,     0,   182,     0,
     183,     0,     0,    40,     0,     0,   735,     0,   184,    66,
       0,   242,     0,    68,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   174,   175,   176,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     239,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,   178,     0,   179,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,   181,
      39,     0,     0,   182,     0,   183,     0,     0,    40,     0,
       0,   735,     0,   184,    66,   213,   242,     0,    68,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,   214,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,     0,     0,     0,
     178,     0,   179,     0,     0,     0,     0,     0,     0,     0,
     180,    39,     0,     0,   181,     0,     0,     0,   182,    40,
     183,     0,     0,     0,     0,     0,     0,     0,   184,    66,
       0,    42,     0,    68,     0,   322,     0,    44,    45,    46,
     174,   175,   176,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,     0,     0,     0,
       0,   178,     0,   179,     0,     0,     0,     0,     0,     0,
       0,   180,    39,     0,     0,   181,   323,     0,     0,   182,
      40,   183,     0,     0,     0,     0,     0,   659,     0,   184,
      66,     0,    42,     0,    68,     0,     0,     0,    44,    45,
      46,   174,   175,   176,     0,   660,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   177,    39,     0,
       0,     0,   178,     0,   179,     0,    40,     0,     0,     0,
       0,     0,   180,     0,     0,     0,   181,     0,    42,     0,
     182,     0,   183,     0,    44,    45,    46,   174,   175,   176,
     184,    66,     0,    52,    53,    68,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   351,   177,     0,     0,     0,     0,   178,     0,
     179,     0,     0,     0,     0,     0,     0,     0,   180,    39,
       0,     0,   181,     0,     0,     0,   182,    40,   183,     0,
       0,     0,     0,     0,     0,     0,   184,    66,     0,    42,
       0,    68,     0,   322,     0,    44,    45,    46,   174,   175,
     176,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   177,    39,     0,     0,     0,   178,
       0,   179,     0,    40,     0,     0,     0,     0,     0,   180,
       0,     0,     0,   181,     0,    42,     0,   182,     0,   183,
       0,    44,    45,    46,   174,   175,   176,   184,    66,     0,
      52,    53,    68,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,   178,     0,   179,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,   181,
      39,     0,     0,   182,   742,   183,     0,     0,    40,     0,
       0,     0,     0,   184,    66,     0,     0,     0,    68,     0,
      42,     0,     0,     0,   322,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,    39,     0,     0,     0,
     178,     0,   179,     0,    40,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   181,     0,    42,     0,   759,     0,
     183,     0,    44,    45,    46,   174,   175,   176,   184,    66,
       0,    52,    53,    68,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     767,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   177,    39,     0,     0,     0,   178,     0,   179,     0,
      40,     0,     0,     0,     0,     0,   180,     0,     0,     0,
     181,     0,    42,     0,   182,     0,   183,     0,    44,    45,
      46,   174,   175,   176,   184,    66,     0,    52,    53,    68,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   770,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   177,    39,     0,
       0,     0,   178,     0,   179,     0,    40,     0,     0,     0,
       0,     0,   180,     0,     0,     0,   181,     0,    42,     0,
     182,     0,   183,     0,    44,    45,    46,   174,   175,   176,
     184,    66,     0,    52,    53,    68,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   177,     0,     0,     0,     0,   178,     0,
     179,     0,     0,     0,     0,     0,     0,     0,   180,     0,
       0,     0,   181,    39,     0,     0,   182,     0,   183,     0,
       0,    40,     0,     0,     0,     0,   184,    66,     0,     0,
      41,    68,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,     0,    50,    51,    52,    53,
      39,    54,    55,    56,     0,     0,    57,     0,    40,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,    64,    40,     0,   304,     0,     0,     0,     0,     0,
       0,    65,    66,     0,    42,    67,    68,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    39,    54,    55,    56,     0,     0,    57,    64,    40,
       0,    58,    59,    60,    61,    62,    63,     0,    65,    66,
       0,    42,    67,    68,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,    64,    40,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,     0,    42,    67,    68,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,     0,
      40,     0,    58,    59,    60,    61,    62,    63,     0,    65,
     112,     0,    42,    67,   113,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,   448,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    40,   225,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    42,     0,     0,    67,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    39,    54,    55,    56,     0,     0,    57,     0,    40,
     225,    58,    59,    60,    61,    62,    63,     0,     0,     0,
      65,    42,     0,     0,    67,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,     0,    40,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,    65,   112,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,   316,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    65,
      44,    45,    46,   174,   175,   176,     0,     0,     0,    52,
      53,    39,    54,    55,    56,     0,     0,    57,     0,    40,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,    42,     0,     0,     0,     0,    65,    44,    45,    46,
     174,   175,   176,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   580,   581,     0,     0,     0,     0,     0,
       0,     0,   582,     0,     0,     0,     0,    39,     0,     0,
       0,     0,   184,     0,     0,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,     0,
     732,   581,     0,    44,    45,    46,   174,   175,   176,   582,
      39,     0,    52,    53,     0,    54,    55,    56,    40,   184,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    39,     0,     0,     0,     0,     0,     0,
       0,    40,     0,     0,     0,     0,     0,   581,     0,     0,
       0,     0,     0,    42,     0,   582,     0,     0,     0,    44,
      45,    46,   174,   175,   176,   184,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,   582,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   184,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   184
  };

  const short
  parser::yycheck_[] =
  {
      87,   213,   104,   228,   111,   209,   210,   313,    87,    29,
     104,    98,   109,   110,   203,    64,   346,   109,   180,    98,
     100,   164,   257,    98,   188,   177,   178,    41,    34,   281,
     437,     5,   279,    34,   476,     1,   287,   181,   190,     1,
      64,    87,     1,   131,    19,   686,    98,    27,   358,   274,
     686,   493,     1,   217,     1,   199,   110,     1,    77,   294,
       1,   110,   463,    27,    12,   467,   101,    77,    82,   511,
     105,   106,    90,   108,   171,   182,   183,   474,   118,    79,
     108,    80,   109,   309,   105,   173,   110,     0,    77,    21,
     177,   178,   119,    93,   181,   182,    78,   118,   133,   104,
      77,   106,    84,   190,    84,   133,   105,   131,   109,   110,
     552,   198,   199,   241,   133,   512,   134,   171,   105,   198,
      84,   208,   171,   133,   124,   125,   213,   214,   128,   129,
     119,   772,   442,   108,   213,   214,   772,   119,   774,   365,
     292,   107,   119,   163,   164,   107,   198,   171,   107,   173,
     401,   403,   118,   379,   282,   241,   118,   245,   107,   118,
     107,   213,   214,   107,   112,   566,   107,   569,   133,   118,
     171,   118,   104,    79,   180,   410,   225,   118,    80,   101,
      86,    80,   104,   270,   581,   582,    22,    93,    80,   112,
     277,   278,   166,   167,    80,    80,   282,    29,   277,   351,
     235,   225,   237,   209,   210,   292,    80,   113,   209,   210,
      79,    65,    66,   119,    80,    51,    52,   119,   124,   125,
     119,   245,   128,   129,    80,   274,   313,   119,   181,   182,
     717,   277,    81,   119,   119,   105,   104,    14,   106,   545,
     546,   467,   386,   650,   113,   119,   199,    77,   118,   250,
     119,   257,   101,   119,    77,   208,   125,   344,   345,   346,
     347,   296,   105,   119,   351,   344,   112,   316,   710,   756,
     712,   103,   519,   279,   716,   118,   112,   123,   113,   359,
     610,   127,   734,    27,   119,   737,   683,   123,   294,   295,
     378,   127,   344,   395,    79,   347,   310,   384,   108,   386,
     707,   395,   112,   507,   114,    24,   558,   404,    93,   406,
     385,   473,   404,   123,   406,   458,   413,   270,   415,   407,
     717,   413,   424,   415,   549,   278,   346,   491,   118,   564,
     112,   163,   164,   113,   105,   120,   480,   105,   105,   124,
     125,   123,   108,   128,   129,   127,   112,   118,   114,   575,
     118,   118,   496,   101,   378,   119,    79,   123,   112,   756,
     313,   127,   759,   123,   616,   113,   118,   127,   113,   595,
      93,    37,    86,    96,   119,   681,    93,    91,   590,    96,
     103,   115,   113,   407,   471,   119,   103,   474,   119,   476,
     477,   223,   345,   480,   115,   647,   726,   120,   119,   603,
     604,   124,   125,   404,   410,   406,   493,   124,   112,   496,
     113,   128,   413,   602,   415,    14,   119,   115,   424,   526,
     527,   119,   108,   510,   511,   512,    93,   115,   104,    96,
     106,   119,   104,   386,   106,   765,   103,   134,   458,   271,
     272,    71,   104,   463,   106,    87,   672,   659,    72,    73,
      74,    75,   104,    84,   106,    86,   786,   124,   545,   546,
     104,   686,   106,   295,   133,   552,   563,   473,   703,     4,
     104,   563,   106,    80,    81,    80,    81,   629,   484,   485,
      80,    81,    23,   484,   485,   113,   200,   201,   794,    27,
      79,    80,   119,   580,   581,   582,    80,    81,   119,   120,
     549,   507,   113,   590,    93,    79,   507,    96,    91,   113,
     722,   590,   113,   519,   103,   741,   113,    77,   471,    93,
     724,    77,    96,   610,   477,   118,    77,   480,    77,   103,
      15,   120,    41,    81,   686,   124,   125,   109,   590,   128,
     129,   109,   629,   496,   770,   771,   566,   772,   119,   113,
     124,   125,   113,   133,   128,   129,   562,   510,   564,   113,
     113,   113,   563,   105,   119,   105,   280,   281,    80,   119,
     109,   658,   659,   660,    91,   119,   113,   113,   790,   658,
     659,   660,   734,   113,    80,   737,   113,   119,    85,    25,
     677,    85,   545,   546,   681,    27,   683,   603,   604,   686,
      81,   134,   603,   604,   133,   702,   658,   659,   660,   696,
     702,   109,   632,    77,    77,    77,   120,   696,   113,   120,
     772,    77,   774,   710,    27,   712,   458,   580,    80,   716,
     717,   463,    80,   735,    81,   722,   119,   119,   740,   726,
     727,   728,   120,   722,   696,   105,   740,   734,   727,   728,
     737,   119,   759,    81,   112,   101,   101,    84,   664,   113,
     747,   120,   119,   664,    78,   120,   380,   109,   747,   756,
     722,   109,   759,    77,    28,   727,   728,   697,   765,   120,
     120,    27,     9,   397,   113,   772,   105,   774,   402,   403,
     404,    79,   779,    81,   118,   747,    84,   703,   113,   786,
     779,   702,    37,   790,    27,    93,    80,   794,    96,    84,
      80,   790,   113,    11,    19,   103,    53,    86,   724,    91,
      86,   123,   119,   724,   677,   105,   113,   779,   681,   113,
     562,     4,   120,    84,   566,    84,   124,   125,   790,    12,
     128,   129,   105,   163,   122,   786,   713,   728,   664,   277,
     485,    24,   466,   467,   715,   724,   604,    30,    31,    32,
      33,    34,    35,    15,   717,   677,    39,    40,   546,    42,
      43,    44,   779,   210,    47,   626,   199,   270,    51,    52,
      53,    54,    55,    56,    79,   499,    79,   501,   512,   503,
     691,    86,   776,   684,   734,   473,    91,   631,    93,   697,
      93,   634,    34,    96,   284,   107,   759,   647,   646,   650,
     103,   201,   501,   235,   395,   562,   740,   394,    -1,   549,
     245,    -1,    -1,    -1,    -1,   120,    -1,   120,    -1,   124,
     125,   124,   125,   128,   129,   128,   129,    -1,    -1,    -1,
      -1,   794,    -1,   557,   558,    -1,    -1,    -1,    -1,   122,
     123,    -1,    -1,    -1,   127,   569,    -1,     3,     4,     5,
       6,     7,     8,    -1,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    19,    20,    -1,    22,    -1,    24,    -1,
      26,    -1,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,   616,   617,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    79,    -1,    -1,    63,    -1,    -1,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    93,    -1,
      76,    -1,   646,   647,   648,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,   124,
     125,    -1,   108,   128,   129,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,    -1,    -1,    -1,   142,   143,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   720,    -1,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    24,    -1,    26,    -1,    28,   751,    30,    31,
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
     132,   133,   134,   135,    -1,    -1,   138,     3,     4,     5,
     142,   143,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,
      26,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,   142,   143,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
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
      -1,    -1,   132,   133,   134,   135,    -1,     1,   138,     3,
       4,     5,   142,   143,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,   112,    -1,
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
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    94,    -1,    96,    -1,
      -1,    -1,    -1,    -1,   102,   103,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,   112,   113,   114,    -1,    -1,    -1,
      -1,   119,   120,    -1,   122,   123,   124,   125,   126,   127,
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
      -1,   103,    -1,    -1,    -1,    -1,   108,   109,    -1,    -1,
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
      -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,   120,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,   118,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    46,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
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
     118,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,
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
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
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
      -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    86,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,
      -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,
     138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,     4,    -1,    -1,    -1,    -1,    -1,
     122,   123,    12,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    24,    -1,   138,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,
      -1,    -1,    -1,    93,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
       4,    -1,   112,   113,   114,    -1,    -1,    -1,    12,   119,
      -1,    -1,   122,   123,   124,   125,    -1,   127,   128,   129,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    -1,    -1,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,     4,    -1,    -1,   112,    -1,
     114,    -1,    -1,    12,    -1,    -1,   120,    -1,   122,   123,
      -1,   125,    -1,   127,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
       4,    -1,    -1,   112,    -1,   114,    -1,    -1,    12,    -1,
      -1,   120,    -1,   122,   123,    19,   125,    -1,   127,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,     4,    -1,    -1,   108,    -1,    -1,    -1,   112,    12,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
      -1,    24,    -1,   127,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,     4,    -1,    -1,   108,   109,    -1,    -1,   112,
      12,   114,    -1,    -1,    -1,    -1,    -1,    19,    -1,   122,
     123,    -1,    24,    -1,   127,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,     4,    -1,
      -1,    -1,    94,    -1,    96,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,    24,    -1,
     112,    -1,   114,    -1,    30,    31,    32,    33,    34,    35,
     122,   123,    -1,    39,    40,   127,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,     4,
      -1,    -1,   108,    -1,    -1,    -1,   112,    12,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    24,
      -1,   127,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,     4,    -1,    -1,    -1,    94,
      -1,    96,    -1,    12,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,    24,    -1,   112,    -1,   114,
      -1,    30,    31,    32,    33,    34,    35,   122,   123,    -1,
      39,    40,   127,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
       4,    -1,    -1,   112,   113,   114,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   122,   123,    -1,    -1,    -1,   127,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,     4,    -1,    -1,    -1,
      94,    -1,    96,    -1,    12,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,    24,    -1,   112,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,     4,    -1,
      -1,    -1,    94,    -1,    96,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,    24,    -1,
     112,    -1,   114,    -1,    30,    31,    32,    33,    34,    35,
     122,   123,    -1,    39,    40,   127,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,     4,    -1,    -1,   112,    -1,   114,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
      21,   127,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    12,    -1,    78,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    24,   126,   127,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,   112,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,   122,   123,
      -1,    24,   126,   127,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    24,   126,   127,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,   122,
     123,    -1,    24,   126,   127,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    78,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    24,    -1,    -1,   126,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
     112,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
     122,    24,    -1,    -1,   126,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,   122,   123,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,   122,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,   122,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,   122,    -1,    -1,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,
     103,   104,    -1,    30,    31,    32,    33,    34,    35,   112,
       4,    -1,    39,    40,    -1,    42,    43,    44,    12,   122,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,    -1,    -1,    24,    -1,   112,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,   122,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   145,   146,   147,   150,   123,   127,   347,
     151,   162,     0,   151,    65,    66,   148,   105,   118,   152,
     163,   164,     1,   107,   346,   108,   133,   214,   214,   112,
     153,    14,   165,   173,   174,   133,   215,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   112,   122,   123,   126,   127,   154,
     155,   156,   160,   313,   316,   317,   330,   332,   333,   339,
      27,    24,   166,   118,   161,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    63,    76,    82,    89,    92,    94,   108,
     112,   114,   123,   127,   132,   133,   134,   135,   138,   142,
     143,   171,   175,   176,   177,   178,   180,   195,   219,   259,
     264,   268,   269,   271,   272,   273,   274,   302,   303,   306,
     307,   329,   330,   333,   341,   342,   345,   109,   119,   347,
      79,    93,    96,   103,   124,   125,   128,   129,   336,   337,
     338,   340,   113,   119,   112,   157,   104,   106,   149,   347,
     118,   112,   267,   268,    33,    34,    35,    89,    94,    96,
     104,   108,   112,   114,   122,   200,   224,   226,   228,   229,
     231,   235,   311,   312,   316,   326,   328,   339,    37,   112,
     104,   106,   294,   267,    72,    73,    74,    75,   181,   104,
     106,   211,   212,    19,    37,   179,   226,   228,   312,    14,
     294,   272,   108,   265,   266,   112,   330,   108,   112,   304,
     305,   307,   342,   272,   292,   293,   272,   271,   272,    79,
     109,   120,   125,   129,   267,   268,   276,   278,   279,   309,
     323,   325,   335,   336,   338,   343,   344,   102,   113,   119,
     275,   276,   277,   336,   337,   338,   343,   348,   115,   348,
      19,   265,   265,   134,   170,   161,    71,   201,    80,   119,
      81,    84,   120,   261,   262,   263,   309,   322,   324,   334,
     336,   337,    88,   272,   101,   104,    87,   133,   113,   113,
     113,   113,   113,   156,    78,   158,   159,   160,   151,   151,
       4,   167,    23,    80,   235,   235,   112,   219,   252,   253,
     254,   333,    28,   109,   221,   222,   224,   226,    79,    86,
      93,   113,   124,   125,   128,   129,   221,   239,   318,   319,
     348,   348,    27,   205,    91,    86,   120,   233,   314,   318,
     327,    88,   232,   235,   226,   221,   238,   239,    20,    46,
     267,   291,   295,   296,   297,   295,   118,   270,    77,    77,
      77,    77,   217,   222,   236,   210,   259,   260,   268,   210,
      15,   190,   226,   226,    80,   119,    81,    41,    90,   134,
     330,    77,   133,   344,    80,   119,   216,    86,   292,   332,
     341,   322,    78,    84,   119,   109,   119,   268,   331,   333,
     101,   113,   113,   119,   113,   119,   113,   113,   113,   119,
     115,   236,   330,   330,   120,   172,   308,   320,   321,   337,
     344,   133,   200,   218,   222,   223,   329,   267,   281,   282,
     297,   332,    27,   213,   263,   269,   235,   331,    78,   298,
     299,   300,   330,   331,   333,   272,   113,   113,   119,   105,
     346,   347,    12,   112,   168,   169,   104,   106,   283,   217,
     337,    80,   105,   119,   240,   109,    80,    91,   113,   113,
     119,   113,   113,   115,   104,   106,   204,   226,   222,   316,
     328,   228,   235,    80,   197,   113,   119,   212,   294,    85,
     105,   118,   346,    25,    27,   209,   105,   118,   346,   267,
      81,    80,    81,   198,   222,   243,   112,   312,   221,   133,
     134,   109,    77,    77,   113,   108,   112,   114,   310,   311,
     304,    77,   267,   120,   120,   267,   280,   297,   267,   276,
     276,   331,   276,   276,    77,    80,    80,   333,   342,   119,
      77,   133,    80,    81,   196,   247,   213,    81,   119,   120,
     212,   105,   119,    81,   101,   160,   112,   154,   105,   118,
     267,   284,   285,   286,   290,   284,   346,   113,   222,   254,
     103,   104,   112,   241,   242,   326,   243,   222,   221,     7,
      26,   191,   202,   203,   260,   203,   120,   120,   243,   221,
     267,   296,   267,   104,   106,   208,   260,   222,   243,   241,
      84,   184,   329,   109,   113,   115,   119,    78,   217,   220,
     220,   120,   120,   320,    77,   243,    28,   248,   249,   250,
      27,   244,     9,   255,   256,   257,   267,   297,   299,   276,
     301,   331,   154,   113,   284,   105,   118,    84,    86,   287,
     288,   289,   346,   222,   326,   326,   113,    37,   192,    19,
      37,   190,   226,   105,   118,   346,   270,    26,   194,   206,
     207,   260,   207,   185,   328,    27,   187,    80,   297,   267,
      77,   119,    77,   240,    84,   225,   230,   234,   235,   251,
     104,   106,   255,    22,    51,    52,   112,   182,   258,   315,
     316,   257,    81,   101,   113,   286,   281,   267,   213,   289,
      80,   105,    80,   226,   190,   226,    80,    81,   199,   202,
      11,    19,   193,   105,   118,   346,    86,   104,   106,   188,
     218,   217,   103,   249,    91,   120,   234,   308,   245,   246,
     270,   245,   113,   226,   227,   237,   258,    53,   183,   276,
     331,    86,   213,   243,   243,   197,    81,   199,   243,   112,
     242,   326,   267,   190,   206,   186,   328,    78,   189,   190,
      78,   189,   230,   251,   230,   105,   118,   305,   346,   119,
     113,   226,   267,   105,   113,   326,    84,   328,   105,   105,
     118,   346,   346,   246,    80,   237,   185,   190,   217
  };

  const short
  parser::yyr1_[] =
  {
       0,   144,   145,   146,   146,   147,   148,   148,   148,   149,
     149,   150,   150,   151,   152,   152,   152,   153,   153,   154,
     155,   155,   156,   156,   157,   157,   157,   158,   158,   159,
     159,   160,   160,   161,   161,   162,   162,   163,   164,   164,
     165,   166,   166,   167,   167,   168,   168,   169,   169,   170,
     170,   171,   171,   171,   172,   172,   173,   174,   174,   175,
     175,   175,   175,   175,   175,   175,   175,   176,   177,   177,
     177,   177,   177,   178,   179,   179,   180,   180,   181,   181,
     181,   181,   181,   182,   182,   182,   183,   184,   184,   185,
     186,   186,   187,   187,   188,   188,   188,   188,   189,   189,
     189,   189,   190,   191,   191,   191,   191,   191,   192,   192,
     193,   193,   194,   195,   195,   196,   196,   197,   197,   198,
     198,   198,   199,   199,   199,   200,   200,   201,   201,   201,
     202,   202,   203,   203,   203,   203,   204,   204,   205,   205,
     206,   206,   207,   207,   207,   207,   208,   208,   209,   209,
     210,   210,   210,   210,   211,   211,   212,   213,   213,   214,
     214,   215,   215,   215,   216,   216,   217,   218,   219,   219,
     220,   220,   221,   221,   222,   222,   222,   223,   224,   225,
     226,   226,   227,   228,   229,   229,   230,   230,   231,   231,
     231,   232,   233,   233,   234,   235,   235,   235,   235,   235,
     235,   235,   235,   235,   235,   236,   237,   237,   238,   238,
     239,   239,   240,   240,   241,   241,   241,   242,   242,   243,
     244,   244,   244,   245,   245,   246,   247,   248,   248,   249,
     249,   250,   250,   251,   251,   252,   252,   253,   253,   254,
     255,   255,   256,   256,   257,   257,   257,   258,   258,   258,
     259,   259,   260,   261,   261,   262,   262,   263,   264,   264,
     264,   264,   264,   264,   264,   264,   264,   265,   265,   266,
     266,   267,   267,   268,   268,   269,   269,   270,   270,   271,
     271,   271,   271,   272,   272,   272,   272,   272,   272,   272,
     272,   272,   272,   273,   273,   273,   274,   274,   274,   274,
     274,   274,   274,   274,   275,   275,   276,   276,   276,   277,
     277,   278,   278,   278,   278,   278,   278,   278,   279,   279,
     280,   280,   281,   282,   282,   283,   283,   283,   283,   284,
     284,   285,   285,   285,   286,   287,   287,   288,   288,   289,
     290,   291,   292,   293,   293,   294,   294,   295,   295,   295,
     295,   296,   296,   297,   297,   297,   298,   298,   299,   299,
     299,   300,   300,   300,   300,   301,   301,   302,   302,   303,
     303,   304,   304,   304,   305,   305,   306,   306,   306,   306,
     307,   307,   308,   308,   309,   309,   310,   310,   310,   311,
     311,   311,   311,   311,   312,   312,   313,   313,   313,   313,
     314,   314,   315,   316,   316,   317,   318,   318,   318,   319,
     319,   319,   319,   320,   320,   321,   321,   322,   322,   323,
     323,   324,   324,   325,   325,   326,   327,   328,   328,   328,
     328,   328,   329,   329,   330,   330,   330,   331,   332,   332,
     333,   333,   333,   333,   333,   333,   333,   333,   334,   334,
     335,   335,   336,   337,   337,   338,   338,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   340,   340,   341,   341,   342,
     343,   343,   344,   344,   345,   345,   345,   345,   345,   346,
     346,   347,   347,   348,   348
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
       6,     6,     4,     4,     3,     1,     4,     3,     2,     2,
       2,     2,     0,     1,     1,     1,     2,     0,     2,     3,
       2,     1,     0,     2,     3,     3,     3,     3,     3,     2,
       1,     0,     3,     4,     3,     4,     2,     3,     0,     1,
       0,     1,     3,     1,     1,     0,     2,     0,     2,     0,
       2,     2,     0,     2,     4,     3,     1,     4,     3,     0,
       1,     1,     3,     2,     1,     0,     3,     3,     2,     0,
       1,     1,     3,     2,     1,     0,     3,     3,     2,     0,
       3,     2,     1,     0,     3,     3,     1,     2,     0,     1,
       3,     3,     1,     0,     0,     2,     1,     1,     3,     1,
       1,     3,     1,     3,     4,     3,     1,     1,     1,     1,
       1,     3,     1,     1,     1,     3,     1,     2,     1,     2,
       3,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       3,     2,     5,     3,     3,     1,     1,     3,     1,     0,
       1,     3,     2,     0,     1,     3,     5,     1,     5,     1,
       4,     4,     0,     3,     1,     4,     2,     3,     1,     4,
       2,     3,     0,     1,     3,     0,     1,     3,     1,     3,
       0,     1,     2,     1,     2,     3,     3,     1,     2,     3,
       1,     2,     1,     3,     2,     2,     1,     4,     3,     3,
       4,     4,     3,     4,     6,     6,     4,     0,     1,     3,
       4,     3,     1,     1,     3,     2,     1,     1,     0,     2,
       3,     2,     1,     3,     2,     2,     4,     4,     8,     4,
       2,     2,     1,     4,     3,     1,     1,     1,     1,     3,
       3,     3,     3,     1,     3,     2,     1,     2,     2,     3,
       3,     1,     1,     2,     4,     3,     5,     3,     3,     3,
       3,     1,     1,     3,     1,     3,     3,     2,     2,     1,
       2,     3,     2,     1,     2,     3,     2,     2,     1,     4,
       1,     1,     1,     2,     1,     3,     3,     3,     2,     1,
       0,     1,     2,     3,     1,     2,     1,     0,     3,     1,
       1,     3,     1,     5,     3,     3,     1,     1,     1,     1,
       3,     1,     3,     1,     3,     1,     2,     3,     2,     3,
       1,     2,     1,     3,     1,     3,     1,     2,     2,     1,
       3,     3,     3,     2,     1,     3,     1,     3,     3,     3,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     1,
       1,     1,     3,     1,     3,     1,     3,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1
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
  "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"", "$accept", "unit",
  "module", "missing_module_keyword", "maybemodwarning", "body", "body2",
  "top", "top1", "maybeexports", "exportlist", "exportlist1", "export",
  "export_subspec", "qcnames", "qcnames1", "qcname", "semis1", "semis",
  "importdecls", "importdecls_semi", "importdecl", "optqualified",
  "maybeas", "maybeimpspec", "impspec", "prec", "infix", "ops", "topdecls",
  "topdecls_semi", "topdecl", "cl_decl", "ty_decl", "standalone_kind_sig",
  "sks_vars", "inst_decl", "overlap_pragma", "deriv_strategy_no_via",
  "deriv_strategy_via", "opt_injective_info", "injectivity_cond",
  "inj_varids", "where_type_family", "ty_fam_inst_eqn_list",
  "ty_fam_inst_eqns", "ty_fam_inst_eqn", "at_decl_cls", "opt_family",
  "opt_instance", "at_decl_inst", "data_or_newtype", "opt_kind_sig",
  "opt_datafam_kind_sig", "opt_tyfam_kind_sig", "opt_at_kind_inj_sig",
  "tycl_hdr", "capi_ctype", "decl_cls", "decls_cls", "decllist_cls",
  "where_cls", "decl_inst", "decls_inst", "decllist_inst", "where_inst",
  "decls", "decllist", "binds", "wherebinds", "strings", "stringlist",
  "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars", "sigtypes1",
  "ktype", "ctype", "ctypedoc", "context", "context_no_ops", "type",
  "typedoc", "btype", "infixtype", "btype_no_ops", "ftype", "tyarg",
  "tyop", "atype_docs", "atype", "inst_type", "deriv_types",
  "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr",
  "tv_bndr_no_braces", "kind", "gadt_constrlist", "gadt_constrs",
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
       0,   511,   511,   528,   529,   531,   535,   536,   537,   539,
     540,   542,   543,   546,   548,   549,   550,   558,   559,   561,
     563,   564,   566,   567,   569,   570,   571,   573,   574,   576,
     577,   579,   580,   584,   585,   587,   588,   590,   592,   593,
     595,   608,   609,   611,   612,   614,   615,   619,   620,   625,
     626,   628,   629,   630,   632,   633,   637,   639,   640,   642,
     643,   644,   645,   648,   649,   655,   657,   660,   663,   664,
     666,   667,   668,   670,   672,   673,   676,   677,   681,   682,
     683,   684,   685,   687,   688,   689,   691,   702,   703,   705,
     707,   708,   712,   713,   715,   716,   717,   718,   720,   721,
     722,   723,   725,   728,   730,   732,   734,   735,   737,   737,
     739,   739,   743,   745,   746,   750,   751,   753,   754,   756,
     757,   758,   760,   761,   762,   766,   767,   769,   770,   771,
     813,   814,   816,   817,   818,   819,   821,   822,   824,   825,
     827,   828,   830,   831,   832,   833,   835,   836,   838,   839,
     842,   843,   844,   845,   847,   848,   850,   852,   853,   861,
     862,   864,   865,   866,   879,   880,   889,   891,   893,   894,
     896,   897,   906,   907,   909,   910,   912,   914,   923,   925,
     927,   928,   930,   933,   935,   936,   938,   939,   941,   943,
     944,   946,   948,   949,   956,   963,   964,   965,   966,   967,
     968,   969,   970,   976,   977,   980,   982,   983,   985,   986,
     988,   989,   996,   997,   999,  1000,  1001,  1004,  1005,  1023,
    1029,  1030,  1031,  1033,  1034,  1036,  1038,  1040,  1041,  1043,
    1044,  1046,  1047,  1049,  1050,  1056,  1057,  1059,  1060,  1062,
    1064,  1065,  1067,  1068,  1070,  1071,  1072,  1074,  1075,  1076,
    1081,  1083,  1085,  1089,  1090,  1092,  1093,  1097,  1107,  1108,
    1110,  1111,  1112,  1113,  1114,  1115,  1116,  1119,  1120,  1122,
    1123,  1128,  1129,  1133,  1134,  1136,  1137,  1139,  1140,  1145,
    1146,  1147,  1148,  1151,  1152,  1153,  1154,  1155,  1157,  1159,
    1160,  1161,  1163,  1166,  1167,  1168,  1171,  1172,  1173,  1174,
    1175,  1176,  1181,  1182,  1185,  1186,  1191,  1192,  1193,  1198,
    1199,  1217,  1218,  1219,  1220,  1221,  1222,  1223,  1225,  1226,
    1239,  1241,  1251,  1253,  1254,  1257,  1258,  1259,  1260,  1262,
    1263,  1265,  1266,  1267,  1269,  1271,  1272,  1274,  1275,  1284,
    1286,  1288,  1290,  1292,  1293,  1296,  1297,  1299,  1300,  1301,
    1302,  1307,  1308,  1310,  1311,  1312,  1317,  1318,  1320,  1321,
    1322,  1324,  1325,  1326,  1327,  1330,  1331,  1363,  1364,  1366,
    1367,  1369,  1370,  1371,  1373,  1374,  1376,  1377,  1378,  1379,
    1381,  1382,  1384,  1385,  1387,  1388,  1391,  1392,  1393,  1395,
    1396,  1397,  1398,  1399,  1401,  1402,  1404,  1405,  1406,  1407,
    1410,  1411,  1413,  1415,  1416,  1420,  1422,  1423,  1424,  1426,
    1427,  1428,  1429,  1434,  1435,  1437,  1438,  1440,  1441,  1444,
    1445,  1450,  1451,  1453,  1454,  1458,  1460,  1462,  1463,  1464,
    1465,  1466,  1469,  1470,  1472,  1473,  1474,  1476,  1478,  1479,
    1481,  1482,  1483,  1484,  1485,  1486,  1487,  1488,  1490,  1491,
    1493,  1494,  1496,  1498,  1499,  1501,  1502,  1504,  1505,  1506,
    1507,  1508,  1509,  1510,  1511,  1512,  1513,  1514,  1515,  1516,
    1517,  1518,  1519,  1520,  1521,  1523,  1524,  1528,  1529,  1531,
    1533,  1534,  1536,  1537,  1541,  1542,  1543,  1544,  1545,  1550,
    1553,  1557,  1558,  1560,  1561
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
#line 7110 "parser.cc"

#line 1570 "parser.y"


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
std::tuple<Located<string>, vector<Hs::LType>>
check_type_or_class_header(const Hs::LType& type)
{
    auto [type_head, type_args] = Hs::decompose_type_apps(type);

    // FIXME -- add location!
    auto tc = unloc(type_head).to<Hs::TypeCon>();

    // Convert to error message!
    if (not tc)
        throw myexception()<<"Malformed type or class header '"<<type<<"'";

    auto name = tc->name;

    return {{type_head.loc,name}, type_args};
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
    auto [name, type_args] = check_type_or_class_header(lhs_type);
    return {name, check_all_type_vars(type_args), rhs_type};
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
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, k, constrs};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k, const std::optional<Hs::GADTConstructorsDecl>& constrs)
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

Hs::InstanceDecl make_instance_decl(const std::optional<std::string>& oprag, const Hs::LType& ltype_orig, const optional<Located<Hs::Decls>>& decls)
{
    // GHC stores the instance as a polytype?
    // This would seem to allow (instance forall a.Eq a => forall a.Eq [a] x y ....)
    auto ltype = ltype_orig;
    auto& [loc, type] = ltype;
    
    if (type.is_a<Hs::ForallType>())
        throw myexception()<<"instance declaration '"<<unloc(ltype)<<"' is malformed";
    Hs::Context context;
    if (auto ct = type.to<Hs::ConstrainedType>())
    {
        context = ct->context;
        ltype = ct->type;
    }

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
                throw myexception()<<"In declaration of instance "<<unloc(ltype_orig).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {oprag, context, ltype, type_inst_decls, method_decls};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::LType& header, const optional<Located<Hs::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);

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
                throw myexception()<<"In declaration of class "<<name<<", I don't recognize declaration:\n   "<<decl.print();
        }

    return {context, name, check_all_type_vars(type_args), fixity_decls, fam_decls, default_type_inst_decls, sig_decls, default_method_decls};
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

Hs::ConstructorDecl make_constructor(const vector<Hs::LTypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::LType& typeish)
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
            return {forall, c, con, *fd};
        }
    }

    // 4. Otherwise make a normal constructor.
    return {forall, c, con, args};
}

