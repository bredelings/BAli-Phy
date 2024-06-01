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
#line 57 "parser.y"

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
#line 512 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2506 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 529 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2512 "parser.cc"
    break;

  case 4: // module: body2
#line 530 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2518 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 532 "parser.y"
                                                                 {drv.push_module_context();}
#line 2524 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 540 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2530 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 541 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2536 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 543 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2542 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 544 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2548 "parser.cc"
    break;

  case 13: // top: semis top1
#line 547 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2554 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 549 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2560 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 550 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2566 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 551 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2572 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 559 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2578 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 560 "parser.y"
                                      {}
#line 2584 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 562 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2590 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 564 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2596 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 565 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2602 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 567 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2608 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 568 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2614 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 570 "parser.y"
                                      {}
#line 2620 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 571 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2626 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 572 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2632 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 574 "parser.y"
                   {}
#line 2638 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 575 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2644 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 577 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2650 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 578 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2656 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 580 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2662 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 581 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2668 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 591 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2674 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 593 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2680 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 594 "parser.y"
                         { }
#line 2686 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 596 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2694 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 609 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2700 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 610 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2706 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 612 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2712 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 613 "parser.y"
                               { }
#line 2718 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 615 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2724 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 616 "parser.y"
                               { }
#line 2730 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 620 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2736 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 621 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2742 "parser.cc"
    break;

  case 49: // prec: %empty
#line 626 "parser.y"
                   { }
#line 2748 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 627 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2754 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 629 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2760 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 630 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2766 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 631 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2772 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 633 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2778 "parser.cc"
    break;

  case 55: // ops: op
#line 634 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2784 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 638 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2790 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 640 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2796 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 641 "parser.y"
                                            { }
#line 2802 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 643 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2808 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 644 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2814 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 645 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2820 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 646 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2826 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 649 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2832 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 650 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2838 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 656 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2844 "parser.cc"
    break;

  case 66: // topdecl: infixexp
#line 658 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2850 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 661 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2856 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 664 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2862 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 665 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2868 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 667 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 2874 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 668 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2880 "parser.cc"
    break;

  case 72: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 669 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 2886 "parser.cc"
    break;

  case 73: // standalone_kind_sig: "type" sks_vars "::" kind
#line 671 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 2892 "parser.cc"
    break;

  case 74: // sks_vars: sks_vars "," oqtycon
#line 673 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2898 "parser.cc"
    break;

  case 75: // sks_vars: oqtycon
#line 674 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2904 "parser.cc"
    break;

  case 76: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 677 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2910 "parser.cc"
    break;

  case 77: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 678 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 2916 "parser.cc"
    break;

  case 78: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 680 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 2926 "parser.cc"
    break;

  case 79: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 686 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 2936 "parser.cc"
    break;

  case 80: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 692 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 2942 "parser.cc"
    break;

  case 81: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 693 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 2948 "parser.cc"
    break;

  case 82: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 694 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 2954 "parser.cc"
    break;

  case 83: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 695 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 2960 "parser.cc"
    break;

  case 84: // overlap_pragma: %empty
#line 696 "parser.y"
                                               {}
#line 2966 "parser.cc"
    break;

  case 94: // where_type_family: %empty
#line 723 "parser.y"
                                                           {}
#line 2972 "parser.cc"
    break;

  case 95: // where_type_family: "where" ty_fam_inst_eqn_list
#line 724 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2978 "parser.cc"
    break;

  case 96: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 726 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2984 "parser.cc"
    break;

  case 97: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 727 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2990 "parser.cc"
    break;

  case 98: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 728 "parser.y"
                                                           {}
#line 2996 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 729 "parser.y"
                                                           {}
#line 3002 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 731 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3008 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 732 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3014 "parser.cc"
    break;

  case 102: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 733 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3020 "parser.cc"
    break;

  case 103: // ty_fam_inst_eqns: %empty
#line 734 "parser.y"
                                                           {}
#line 3026 "parser.cc"
    break;

  case 104: // ty_fam_inst_eqn: type "=" ctype
#line 736 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3032 "parser.cc"
    break;

  case 105: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 739 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3038 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 741 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3044 "parser.cc"
    break;

  case 107: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 743 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3050 "parser.cc"
    break;

  case 108: // at_decl_cls: "type" ty_fam_inst_eqn
#line 745 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3056 "parser.cc"
    break;

  case 109: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 746 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3062 "parser.cc"
    break;

  case 114: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 754 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3068 "parser.cc"
    break;

  case 115: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 757 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3078 "parser.cc"
    break;

  case 116: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 764 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3088 "parser.cc"
    break;

  case 117: // data_or_newtype: "data"
#line 770 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3094 "parser.cc"
    break;

  case 118: // data_or_newtype: "newtype"
#line 771 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3100 "parser.cc"
    break;

  case 119: // opt_kind_sig: %empty
#line 775 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3106 "parser.cc"
    break;

  case 120: // opt_kind_sig: "::" kind
#line 776 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3112 "parser.cc"
    break;

  case 121: // opt_datafam_kind_sig: %empty
#line 778 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3118 "parser.cc"
    break;

  case 122: // opt_datafam_kind_sig: "::" kind
#line 779 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3124 "parser.cc"
    break;

  case 123: // opt_tyfam_kind_sig: %empty
#line 781 "parser.y"
                                      {}
#line 3130 "parser.cc"
    break;

  case 124: // opt_tyfam_kind_sig: "::" kind
#line 782 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3136 "parser.cc"
    break;

  case 125: // opt_tyfam_kind_sig: "=" tv_bndr
#line 783 "parser.y"
                                      {}
#line 3142 "parser.cc"
    break;

  case 126: // opt_at_kind_inj_sig: %empty
#line 785 "parser.y"
                                      {}
#line 3148 "parser.cc"
    break;

  case 127: // opt_at_kind_inj_sig: "::" kind
#line 786 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3154 "parser.cc"
    break;

  case 128: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 787 "parser.y"
                                                                  {}
#line 3160 "parser.cc"
    break;

  case 129: // tycl_hdr: context "=>" type
#line 790 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3166 "parser.cc"
    break;

  case 130: // tycl_hdr: type
#line 791 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3172 "parser.cc"
    break;

  case 131: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 794 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3178 "parser.cc"
    break;

  case 132: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 795 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3184 "parser.cc"
    break;

  case 133: // datafam_inst_hdr: context "=>" type
#line 796 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3190 "parser.cc"
    break;

  case 134: // datafam_inst_hdr: type
#line 797 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3196 "parser.cc"
    break;

  case 138: // decl_cls: at_decl_cls
#line 843 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3202 "parser.cc"
    break;

  case 139: // decl_cls: decl
#line 844 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3208 "parser.cc"
    break;

  case 140: // decls_cls: decls_cls ";" decl_cls
#line 846 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3214 "parser.cc"
    break;

  case 141: // decls_cls: decls_cls ";"
#line 847 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3220 "parser.cc"
    break;

  case 142: // decls_cls: decl_cls
#line 848 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3226 "parser.cc"
    break;

  case 143: // decls_cls: %empty
#line 849 "parser.y"
                                           {}
#line 3232 "parser.cc"
    break;

  case 144: // decllist_cls: "{" decls_cls "}"
#line 851 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3238 "parser.cc"
    break;

  case 145: // decllist_cls: "vocurly" decls_cls close
#line 852 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3244 "parser.cc"
    break;

  case 146: // where_cls: "where" decllist_cls
#line 854 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3250 "parser.cc"
    break;

  case 147: // where_cls: %empty
#line 855 "parser.y"
                                           {}
#line 3256 "parser.cc"
    break;

  case 148: // decl_inst: at_decl_inst
#line 857 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3262 "parser.cc"
    break;

  case 149: // decl_inst: decl
#line 858 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3268 "parser.cc"
    break;

  case 150: // decls_inst: decls_inst ";" decl_inst
#line 860 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3274 "parser.cc"
    break;

  case 151: // decls_inst: decls_inst ";"
#line 861 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3280 "parser.cc"
    break;

  case 152: // decls_inst: decl_inst
#line 862 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3286 "parser.cc"
    break;

  case 153: // decls_inst: %empty
#line 863 "parser.y"
                                           {}
#line 3292 "parser.cc"
    break;

  case 154: // decllist_inst: "{" decls_inst "}"
#line 865 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3298 "parser.cc"
    break;

  case 155: // decllist_inst: "vocurly" decls_inst close
#line 866 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3304 "parser.cc"
    break;

  case 156: // where_inst: "where" decllist_inst
#line 868 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3310 "parser.cc"
    break;

  case 157: // where_inst: %empty
#line 869 "parser.y"
                                           {}
#line 3316 "parser.cc"
    break;

  case 158: // decls: decls ";" decl
#line 872 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3322 "parser.cc"
    break;

  case 159: // decls: decls ";"
#line 873 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3328 "parser.cc"
    break;

  case 160: // decls: decl
#line 874 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3334 "parser.cc"
    break;

  case 161: // decls: %empty
#line 875 "parser.y"
                        {}
#line 3340 "parser.cc"
    break;

  case 162: // decllist: "{" decls "}"
#line 877 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3346 "parser.cc"
    break;

  case 163: // decllist: "vocurly" decls close
#line 878 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3352 "parser.cc"
    break;

  case 164: // binds: decllist
#line 880 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3358 "parser.cc"
    break;

  case 165: // wherebinds: "where" binds
#line 882 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3364 "parser.cc"
    break;

  case 166: // wherebinds: %empty
#line 883 "parser.y"
                                 {}
#line 3370 "parser.cc"
    break;

  case 172: // opt_tyconsig: %empty
#line 909 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3376 "parser.cc"
    break;

  case 173: // opt_tyconsig: "::" gtycon
#line 910 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3382 "parser.cc"
    break;

  case 174: // sigtype: ctype
#line 919 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3388 "parser.cc"
    break;

  case 175: // sigtypedoc: ctypedoc
#line 921 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3394 "parser.cc"
    break;

  case 176: // sig_vars: sig_vars "," var
#line 923 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3400 "parser.cc"
    break;

  case 177: // sig_vars: var
#line 924 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3406 "parser.cc"
    break;

  case 178: // sigtypes1: sigtype
#line 926 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3412 "parser.cc"
    break;

  case 179: // sigtypes1: sigtypes1 "," sigtype
#line 927 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3418 "parser.cc"
    break;

  case 180: // ktype: ctype
#line 936 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3424 "parser.cc"
    break;

  case 181: // ktype: ctype "::" kind
#line 937 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3430 "parser.cc"
    break;

  case 182: // ctype: "forall" tv_bndrs "." ctype
#line 939 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3436 "parser.cc"
    break;

  case 183: // ctype: context "=>" ctype
#line 940 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3442 "parser.cc"
    break;

  case 184: // ctype: type
#line 942 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3448 "parser.cc"
    break;

  case 185: // ctypedoc: ctype
#line 944 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3454 "parser.cc"
    break;

  case 186: // context: btype
#line 953 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3460 "parser.cc"
    break;

  case 187: // context_no_ops: btype_no_ops
#line 955 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3466 "parser.cc"
    break;

  case 188: // type: btype
#line 957 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3472 "parser.cc"
    break;

  case 189: // type: btype "->" ctype
#line 958 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3478 "parser.cc"
    break;

  case 190: // typedoc: type
#line 960 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3484 "parser.cc"
    break;

  case 191: // btype: infixtype
#line 963 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3490 "parser.cc"
    break;

  case 192: // infixtype: ftype
#line 965 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3496 "parser.cc"
    break;

  case 193: // infixtype: btype tyop btype
#line 966 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3502 "parser.cc"
    break;

  case 194: // btype_no_ops: atype_docs
#line 968 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3508 "parser.cc"
    break;

  case 195: // btype_no_ops: btype_no_ops atype_docs
#line 969 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3514 "parser.cc"
    break;

  case 196: // ftype: atype
#line 971 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3520 "parser.cc"
    break;

  case 197: // ftype: ftype tyarg
#line 973 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3526 "parser.cc"
    break;

  case 198: // ftype: ftype "@" atype
#line 974 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3532 "parser.cc"
    break;

  case 199: // tyarg: atype
#line 976 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3538 "parser.cc"
    break;

  case 200: // tyop: qtyconop
#line 978 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3544 "parser.cc"
    break;

  case 201: // tyop: tyvarop
#line 979 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3550 "parser.cc"
    break;

  case 202: // atype_docs: atype
#line 986 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3556 "parser.cc"
    break;

  case 203: // atype: ntgtycon
#line 993 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3562 "parser.cc"
    break;

  case 204: // atype: tyvar
#line 994 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3568 "parser.cc"
    break;

  case 205: // atype: "*"
#line 995 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3574 "parser.cc"
    break;

  case 206: // atype: PREFIX_BANG atype
#line 996 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3580 "parser.cc"
    break;

  case 207: // atype: PREFIX_TILDE atype
#line 997 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3586 "parser.cc"
    break;

  case 208: // atype: "{" fielddecls "}"
#line 998 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3592 "parser.cc"
    break;

  case 209: // atype: "(" ")"
#line 999 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3598 "parser.cc"
    break;

  case 210: // atype: "(" comma_types1 "," ktype ")"
#line 1000 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3604 "parser.cc"
    break;

  case 211: // atype: "[" ktype "]"
#line 1006 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3610 "parser.cc"
    break;

  case 212: // atype: "(" ktype ")"
#line 1007 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3616 "parser.cc"
    break;

  case 213: // inst_type: sigtype
#line 1010 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3622 "parser.cc"
    break;

  case 216: // comma_types0: comma_types1
#line 1015 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3628 "parser.cc"
    break;

  case 217: // comma_types0: %empty
#line 1016 "parser.y"
                                       { /* default construction OK */ }
#line 3634 "parser.cc"
    break;

  case 218: // comma_types1: ktype
#line 1018 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3640 "parser.cc"
    break;

  case 219: // comma_types1: comma_types1 "," ktype
#line 1019 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3646 "parser.cc"
    break;

  case 220: // tv_bndrs: tv_bndrs tv_bndr
#line 1026 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3652 "parser.cc"
    break;

  case 221: // tv_bndrs: %empty
#line 1027 "parser.y"
                               { /* default construction OK */}
#line 3658 "parser.cc"
    break;

  case 222: // tv_bndr: tv_bndr_no_braces
#line 1029 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3664 "parser.cc"
    break;

  case 223: // tv_bndr: "{" tyvar "}"
#line 1030 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3670 "parser.cc"
    break;

  case 224: // tv_bndr: "{" tyvar "::" kind "}"
#line 1031 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3676 "parser.cc"
    break;

  case 225: // tv_bndr_no_braces: tyvar
#line 1034 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3682 "parser.cc"
    break;

  case 226: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1035 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3688 "parser.cc"
    break;

  case 227: // kind: ctype
#line 1053 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3694 "parser.cc"
    break;

  case 228: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1059 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3700 "parser.cc"
    break;

  case 229: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1060 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3706 "parser.cc"
    break;

  case 230: // gadt_constrlist: %empty
#line 1061 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3712 "parser.cc"
    break;

  case 231: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1063 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3718 "parser.cc"
    break;

  case 232: // gadt_constrs: gadt_constr
#line 1064 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3724 "parser.cc"
    break;

  case 233: // gadt_constr: optSemi con_list "::" sigtype
#line 1066 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3730 "parser.cc"
    break;

  case 234: // constrs: "=" constrs1
#line 1068 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3736 "parser.cc"
    break;

  case 235: // constrs1: constrs1 "|" constr
#line 1070 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3742 "parser.cc"
    break;

  case 236: // constrs1: constr
#line 1071 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3748 "parser.cc"
    break;

  case 237: // constr: forall context_no_ops "=>" constr_stuff
#line 1073 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3754 "parser.cc"
    break;

  case 238: // constr: forall constr_stuff
#line 1074 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3760 "parser.cc"
    break;

  case 239: // forall: "forall" tv_bndrs "."
#line 1076 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3766 "parser.cc"
    break;

  case 240: // forall: %empty
#line 1077 "parser.y"
                                {}
#line 3772 "parser.cc"
    break;

  case 241: // constr_stuff: btype_no_ops
#line 1079 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3778 "parser.cc"
    break;

  case 242: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1080 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3788 "parser.cc"
    break;

  case 243: // fielddecls: %empty
#line 1086 "parser.y"
                                {}
#line 3794 "parser.cc"
    break;

  case 244: // fielddecls: fielddecls1
#line 1087 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3800 "parser.cc"
    break;

  case 245: // fielddecls1: fielddecls1 "," fielddecl
#line 1089 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3806 "parser.cc"
    break;

  case 246: // fielddecls1: fielddecl
#line 1090 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3812 "parser.cc"
    break;

  case 247: // fielddecl: sig_vars "::" ctype
#line 1092 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3818 "parser.cc"
    break;

  case 258: // decl_no_th: sigdecl
#line 1111 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3824 "parser.cc"
    break;

  case 259: // decl_no_th: infixexp rhs
#line 1113 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3830 "parser.cc"
    break;

  case 260: // decl: decl_no_th
#line 1115 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3836 "parser.cc"
    break;

  case 261: // rhs: "=" exp wherebinds
#line 1119 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3842 "parser.cc"
    break;

  case 262: // rhs: gdrhs wherebinds
#line 1120 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3848 "parser.cc"
    break;

  case 263: // gdrhs: gdrhs gdrh
#line 1122 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3854 "parser.cc"
    break;

  case 264: // gdrhs: gdrh
#line 1123 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3860 "parser.cc"
    break;

  case 265: // gdrh: "|" guardquals "=" exp
#line 1127 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3866 "parser.cc"
    break;

  case 266: // sigdecl: sig_vars "::" sigtypedoc
#line 1137 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3872 "parser.cc"
    break;

  case 267: // sigdecl: infix prec ops
#line 1138 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3878 "parser.cc"
    break;

  case 268: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1140 "parser.y"
                                                    {}
#line 3884 "parser.cc"
    break;

  case 269: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1141 "parser.y"
                                            {}
#line 3890 "parser.cc"
    break;

  case 270: // sigdecl: "{-# SCC" qvar "#-}"
#line 1142 "parser.y"
                              {}
#line 3896 "parser.cc"
    break;

  case 271: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1143 "parser.y"
                                     {}
#line 3902 "parser.cc"
    break;

  case 272: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1144 "parser.y"
                                                               {}
#line 3908 "parser.cc"
    break;

  case 273: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1145 "parser.y"
                                                                      {}
#line 3914 "parser.cc"
    break;

  case 274: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1146 "parser.y"
                                                     {}
#line 3920 "parser.cc"
    break;

  case 279: // exp: infixexp "::" sigtype
#line 1158 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 3926 "parser.cc"
    break;

  case 280: // exp: infixexp
#line 1159 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3932 "parser.cc"
    break;

  case 281: // infixexp: exp10
#line 1163 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3938 "parser.cc"
    break;

  case 282: // infixexp: infixexp qop exp10
#line 1164 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3944 "parser.cc"
    break;

  case 283: // exp10: PREFIX_MINUS fexp
#line 1166 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3950 "parser.cc"
    break;

  case 284: // exp10: fexp
#line 1167 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3956 "parser.cc"
    break;

  case 287: // fexp: fexp aexp
#line 1175 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3962 "parser.cc"
    break;

  case 288: // fexp: fexp "@" atype
#line 1176 "parser.y"
                                 {}
#line 3968 "parser.cc"
    break;

  case 289: // fexp: "static" aexp
#line 1177 "parser.y"
                                 {}
#line 3974 "parser.cc"
    break;

  case 290: // fexp: aexp
#line 1178 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3980 "parser.cc"
    break;

  case 291: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1181 "parser.y"
                                            {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern(Hs::Var(yystack_[2].value.as < std::string > ()),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3986 "parser.cc"
    break;

  case 292: // aexp: PREFIX_TILDE aexp
#line 1182 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3992 "parser.cc"
    break;

  case 293: // aexp: PREFIX_BANG aexp
#line 1183 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3998 "parser.cc"
    break;

  case 294: // aexp: "\\" apats1 "->" exp
#line 1184 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4004 "parser.cc"
    break;

  case 295: // aexp: "let" binds "in" exp
#line 1185 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4010 "parser.cc"
    break;

  case 296: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1187 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4016 "parser.cc"
    break;

  case 297: // aexp: "case" exp "of" altslist
#line 1189 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4022 "parser.cc"
    break;

  case 298: // aexp: "do" stmtlist
#line 1190 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4028 "parser.cc"
    break;

  case 299: // aexp: "mdo" stmtlist
#line 1191 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4034 "parser.cc"
    break;

  case 300: // aexp: aexp1
#line 1193 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4040 "parser.cc"
    break;

  case 301: // aexp1: aexp1 "{" fbinds "}"
#line 1196 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4046 "parser.cc"
    break;

  case 302: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1197 "parser.y"
                                     { }
#line 4052 "parser.cc"
    break;

  case 303: // aexp1: aexp2
#line 1198 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4058 "parser.cc"
    break;

  case 304: // aexp2: qvar
#line 1201 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4064 "parser.cc"
    break;

  case 305: // aexp2: qcon
#line 1202 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4070 "parser.cc"
    break;

  case 306: // aexp2: literal
#line 1203 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4076 "parser.cc"
    break;

  case 307: // aexp2: "(" texp ")"
#line 1204 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4082 "parser.cc"
    break;

  case 308: // aexp2: "(" tup_exprs ")"
#line 1205 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4088 "parser.cc"
    break;

  case 309: // aexp2: "(" projection ")"
#line 1206 "parser.y"
                              {}
#line 4094 "parser.cc"
    break;

  case 310: // aexp2: "[" list "]"
#line 1211 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4100 "parser.cc"
    break;

  case 311: // aexp2: "_"
#line 1212 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4106 "parser.cc"
    break;

  case 314: // texp: exp
#line 1221 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4112 "parser.cc"
    break;

  case 315: // texp: infixexp qop
#line 1222 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4118 "parser.cc"
    break;

  case 316: // texp: qopm infixexp
#line 1223 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4124 "parser.cc"
    break;

  case 317: // tup_exprs: tup_exprs "," texp
#line 1228 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4130 "parser.cc"
    break;

  case 318: // tup_exprs: texp "," texp
#line 1229 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4136 "parser.cc"
    break;

  case 319: // list: texp
#line 1247 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4142 "parser.cc"
    break;

  case 320: // list: lexps
#line 1248 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4148 "parser.cc"
    break;

  case 321: // list: texp ".."
#line 1249 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4154 "parser.cc"
    break;

  case 322: // list: texp "," exp ".."
#line 1250 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4160 "parser.cc"
    break;

  case 323: // list: texp ".." exp
#line 1251 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4166 "parser.cc"
    break;

  case 324: // list: texp "," exp ".." exp
#line 1252 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4172 "parser.cc"
    break;

  case 325: // list: texp "|" squals
#line 1253 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4178 "parser.cc"
    break;

  case 326: // lexps: lexps "," texp
#line 1255 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4184 "parser.cc"
    break;

  case 327: // lexps: texp "," texp
#line 1256 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4190 "parser.cc"
    break;

  case 328: // squals: squals "," qual
#line 1269 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4196 "parser.cc"
    break;

  case 329: // squals: qual
#line 1271 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4202 "parser.cc"
    break;

  case 330: // guardquals: guardquals1
#line 1281 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4208 "parser.cc"
    break;

  case 331: // guardquals1: guardquals1 "," qual
#line 1283 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4214 "parser.cc"
    break;

  case 332: // guardquals1: qual
#line 1284 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4220 "parser.cc"
    break;

  case 333: // altslist: "{" alts "}"
#line 1287 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4226 "parser.cc"
    break;

  case 334: // altslist: "vocurly" alts close
#line 1288 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4232 "parser.cc"
    break;

  case 335: // altslist: "{" "}"
#line 1289 "parser.y"
                                 {}
#line 4238 "parser.cc"
    break;

  case 336: // altslist: "vocurly" close
#line 1290 "parser.y"
                                 {}
#line 4244 "parser.cc"
    break;

  case 337: // alts: alts1
#line 1292 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4250 "parser.cc"
    break;

  case 338: // alts: ";" alts
#line 1293 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4256 "parser.cc"
    break;

  case 339: // alts1: alts1 ";" alt
#line 1295 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4262 "parser.cc"
    break;

  case 340: // alts1: alts1 ";"
#line 1296 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4268 "parser.cc"
    break;

  case 341: // alts1: alt
#line 1297 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4274 "parser.cc"
    break;

  case 342: // alt: pat alt_rhs
#line 1299 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4280 "parser.cc"
    break;

  case 343: // alt_rhs: "->" exp wherebinds
#line 1301 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4286 "parser.cc"
    break;

  case 344: // alt_rhs: gdpats wherebinds
#line 1302 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4292 "parser.cc"
    break;

  case 345: // gdpats: gdpats gdpat
#line 1304 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4298 "parser.cc"
    break;

  case 346: // gdpats: gdpat
#line 1305 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4304 "parser.cc"
    break;

  case 347: // gdpat: "|" guardquals "->" exp
#line 1314 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4310 "parser.cc"
    break;

  case 348: // pat: exp
#line 1316 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4316 "parser.cc"
    break;

  case 349: // bindpat: exp
#line 1318 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4322 "parser.cc"
    break;

  case 350: // apat: aexp
#line 1320 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4328 "parser.cc"
    break;

  case 351: // apats1: apats1 apat
#line 1322 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4334 "parser.cc"
    break;

  case 352: // apats1: apat
#line 1323 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4340 "parser.cc"
    break;

  case 353: // stmtlist: "{" stmts "}"
#line 1326 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4346 "parser.cc"
    break;

  case 354: // stmtlist: "vocurly" stmts close
#line 1327 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4352 "parser.cc"
    break;

  case 355: // stmts: stmts ";" stmt
#line 1329 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4358 "parser.cc"
    break;

  case 356: // stmts: stmts ";"
#line 1330 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4364 "parser.cc"
    break;

  case 357: // stmts: stmt
#line 1331 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4370 "parser.cc"
    break;

  case 358: // stmts: %empty
#line 1332 "parser.y"
                       {}
#line 4376 "parser.cc"
    break;

  case 359: // stmt: qual
#line 1337 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4382 "parser.cc"
    break;

  case 360: // stmt: "rec" stmtlist
#line 1338 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4388 "parser.cc"
    break;

  case 361: // qual: bindpat "<-" exp
#line 1340 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4394 "parser.cc"
    break;

  case 362: // qual: exp
#line 1341 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4400 "parser.cc"
    break;

  case 363: // qual: "let" binds
#line 1342 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4406 "parser.cc"
    break;

  case 364: // fbinds: fbinds1
#line 1347 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4412 "parser.cc"
    break;

  case 365: // fbinds: %empty
#line 1348 "parser.y"
                        {}
#line 4418 "parser.cc"
    break;

  case 366: // fbinds1: fbind "," fbinds1
#line 1350 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4424 "parser.cc"
    break;

  case 367: // fbinds1: fbind
#line 1351 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4430 "parser.cc"
    break;

  case 368: // fbinds1: ".."
#line 1352 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4436 "parser.cc"
    break;

  case 369: // fbind: qvar "=" texp
#line 1354 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4442 "parser.cc"
    break;

  case 370: // fbind: qvar
#line 1355 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4448 "parser.cc"
    break;

  case 371: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1356 "parser.y"
                                                      {}
#line 4454 "parser.cc"
    break;

  case 372: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1357 "parser.y"
                                                      {}
#line 4460 "parser.cc"
    break;

  case 375: // qcon: gen_qcon
#line 1393 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4466 "parser.cc"
    break;

  case 376: // qcon: sysdcon
#line 1394 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4472 "parser.cc"
    break;

  case 377: // gen_qcon: qconid
#line 1396 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4478 "parser.cc"
    break;

  case 378: // gen_qcon: "(" qconsym ")"
#line 1397 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4484 "parser.cc"
    break;

  case 379: // con: conid
#line 1399 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4490 "parser.cc"
    break;

  case 380: // con: "(" consym ")"
#line 1400 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4496 "parser.cc"
    break;

  case 381: // con: sysdcon
#line 1401 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4502 "parser.cc"
    break;

  case 382: // con_list: con_list "," con
#line 1403 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4508 "parser.cc"
    break;

  case 383: // con_list: con
#line 1404 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4514 "parser.cc"
    break;

  case 384: // sysdcon_no_list: "(" ")"
#line 1406 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4520 "parser.cc"
    break;

  case 385: // sysdcon_no_list: "(" commas ")"
#line 1407 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4526 "parser.cc"
    break;

  case 386: // sysdcon_no_list: "(#" "#)"
#line 1408 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4532 "parser.cc"
    break;

  case 387: // sysdcon_no_list: "(#" commas "#)"
#line 1409 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4538 "parser.cc"
    break;

  case 388: // sysdcon: sysdcon_no_list
#line 1411 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4544 "parser.cc"
    break;

  case 389: // sysdcon: "[" "]"
#line 1412 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4550 "parser.cc"
    break;

  case 390: // conop: consym
#line 1414 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4556 "parser.cc"
    break;

  case 391: // conop: "`" conid "`"
#line 1415 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4562 "parser.cc"
    break;

  case 392: // qconop: qconsym
#line 1417 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4568 "parser.cc"
    break;

  case 393: // qconop: "`" qconid "`"
#line 1418 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4574 "parser.cc"
    break;

  case 394: // gtycon: ntgtycon
#line 1421 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4580 "parser.cc"
    break;

  case 395: // gtycon: "(" ")"
#line 1422 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4586 "parser.cc"
    break;

  case 396: // gtycon: "(#" "#)"
#line 1423 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4592 "parser.cc"
    break;

  case 397: // ntgtycon: oqtycon
#line 1425 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4598 "parser.cc"
    break;

  case 398: // ntgtycon: "(" commas ")"
#line 1426 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4604 "parser.cc"
    break;

  case 399: // ntgtycon: "(#" commas "#)"
#line 1427 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4610 "parser.cc"
    break;

  case 400: // ntgtycon: "(" "->" ")"
#line 1428 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4616 "parser.cc"
    break;

  case 401: // ntgtycon: "[" "]"
#line 1429 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4622 "parser.cc"
    break;

  case 402: // oqtycon: qtycon
#line 1431 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4628 "parser.cc"
    break;

  case 403: // oqtycon: "(" qtyconsym ")"
#line 1432 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4634 "parser.cc"
    break;

  case 404: // oqtycon_no_varcon: qtycon
#line 1434 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4640 "parser.cc"
    break;

  case 405: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1435 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4646 "parser.cc"
    break;

  case 406: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1436 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4652 "parser.cc"
    break;

  case 407: // oqtycon_no_varcon: "(" ":" ")"
#line 1437 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4658 "parser.cc"
    break;

  case 408: // qtyconop: qtyconsym
#line 1440 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4664 "parser.cc"
    break;

  case 409: // qtyconop: "`" qtycon "`"
#line 1441 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4670 "parser.cc"
    break;

  case 410: // qtycondoc: qtycon
#line 1443 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4676 "parser.cc"
    break;

  case 411: // qtycon: "QCONID"
#line 1445 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4682 "parser.cc"
    break;

  case 412: // qtycon: tycon
#line 1446 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4688 "parser.cc"
    break;

  case 413: // tycon: "CONID"
#line 1450 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4694 "parser.cc"
    break;

  case 414: // qtyconsym: "QCONSYM"
#line 1452 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4700 "parser.cc"
    break;

  case 415: // qtyconsym: "QVARSYM"
#line 1453 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4706 "parser.cc"
    break;

  case 416: // qtyconsym: tyconsym
#line 1454 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4712 "parser.cc"
    break;

  case 417: // tyconsym: "CONSYM"
#line 1456 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4718 "parser.cc"
    break;

  case 418: // tyconsym: "VARSYM"
#line 1457 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4724 "parser.cc"
    break;

  case 419: // tyconsym: ":"
#line 1458 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4730 "parser.cc"
    break;

  case 420: // tyconsym: "-"
#line 1459 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4736 "parser.cc"
    break;

  case 421: // op: varop
#line 1464 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4742 "parser.cc"
    break;

  case 422: // op: conop
#line 1465 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4748 "parser.cc"
    break;

  case 423: // varop: varsym
#line 1467 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4754 "parser.cc"
    break;

  case 424: // varop: "`" varid "`"
#line 1468 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4760 "parser.cc"
    break;

  case 425: // qop: qvarop
#line 1470 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4766 "parser.cc"
    break;

  case 426: // qop: qconop
#line 1471 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4772 "parser.cc"
    break;

  case 427: // qopm: qvaropm
#line 1474 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4778 "parser.cc"
    break;

  case 428: // qopm: qconop
#line 1475 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4784 "parser.cc"
    break;

  case 429: // qvarop: qvarsym
#line 1480 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4790 "parser.cc"
    break;

  case 430: // qvarop: "`" qvarid "`"
#line 1481 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4796 "parser.cc"
    break;

  case 431: // qvaropm: qvarsym_no_minus
#line 1483 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4802 "parser.cc"
    break;

  case 432: // qvaropm: "`" qvarid "`"
#line 1484 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4808 "parser.cc"
    break;

  case 433: // tyvar: tyvarid
#line 1488 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4814 "parser.cc"
    break;

  case 434: // tyvarop: "`" tyvarid "`"
#line 1490 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4820 "parser.cc"
    break;

  case 435: // tyvarid: "VARID"
#line 1492 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4826 "parser.cc"
    break;

  case 436: // tyvarid: special_id
#line 1493 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4832 "parser.cc"
    break;

  case 437: // tyvarid: "unsafe"
#line 1494 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4838 "parser.cc"
    break;

  case 438: // tyvarid: "safe"
#line 1495 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4844 "parser.cc"
    break;

  case 439: // tyvarid: "interruptible"
#line 1496 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4850 "parser.cc"
    break;

  case 440: // var: varid
#line 1499 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4856 "parser.cc"
    break;

  case 441: // var: "(" varsym ")"
#line 1500 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4862 "parser.cc"
    break;

  case 442: // qvar: qvarid
#line 1502 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4868 "parser.cc"
    break;

  case 443: // qvar: "(" varsym ")"
#line 1503 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4874 "parser.cc"
    break;

  case 444: // qvar: "(" qvarsym1 ")"
#line 1504 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4880 "parser.cc"
    break;

  case 445: // field: varid
#line 1506 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4886 "parser.cc"
    break;

  case 446: // qvarid: varid
#line 1508 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4892 "parser.cc"
    break;

  case 447: // qvarid: "QVARID"
#line 1509 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4898 "parser.cc"
    break;

  case 448: // varid: "VARID"
#line 1511 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4904 "parser.cc"
    break;

  case 449: // varid: special_id
#line 1512 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4910 "parser.cc"
    break;

  case 450: // varid: "unsafe"
#line 1513 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4916 "parser.cc"
    break;

  case 451: // varid: "safe"
#line 1514 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4922 "parser.cc"
    break;

  case 452: // varid: "interruptible"
#line 1515 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4928 "parser.cc"
    break;

  case 453: // varid: "forall"
#line 1516 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4934 "parser.cc"
    break;

  case 454: // varid: "family"
#line 1517 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4940 "parser.cc"
    break;

  case 455: // varid: "role"
#line 1518 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4946 "parser.cc"
    break;

  case 456: // qvarsym: varsym
#line 1520 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4952 "parser.cc"
    break;

  case 457: // qvarsym: qvarsym1
#line 1521 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4958 "parser.cc"
    break;

  case 458: // qvarsym_no_minus: varsym_no_minus
#line 1523 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4964 "parser.cc"
    break;

  case 459: // qvarsym_no_minus: qvarsym1
#line 1524 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4970 "parser.cc"
    break;

  case 460: // qvarsym1: "QVARSYM"
#line 1526 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4976 "parser.cc"
    break;

  case 461: // varsym: varsym_no_minus
#line 1528 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4982 "parser.cc"
    break;

  case 462: // varsym: "-"
#line 1529 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4988 "parser.cc"
    break;

  case 463: // varsym_no_minus: "VARSYM"
#line 1531 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4994 "parser.cc"
    break;

  case 464: // varsym_no_minus: special_sym
#line 1532 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5000 "parser.cc"
    break;

  case 465: // special_id: "as"
#line 1534 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5006 "parser.cc"
    break;

  case 466: // special_id: "qualified"
#line 1535 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5012 "parser.cc"
    break;

  case 467: // special_id: "hiding"
#line 1536 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5018 "parser.cc"
    break;

  case 468: // special_id: "export"
#line 1537 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5024 "parser.cc"
    break;

  case 469: // special_id: "label"
#line 1538 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5030 "parser.cc"
    break;

  case 470: // special_id: "dynamic"
#line 1539 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5036 "parser.cc"
    break;

  case 471: // special_id: "stdcall"
#line 1540 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5042 "parser.cc"
    break;

  case 472: // special_id: "ccall"
#line 1541 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5048 "parser.cc"
    break;

  case 473: // special_id: "capi"
#line 1542 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5054 "parser.cc"
    break;

  case 474: // special_id: "prim"
#line 1543 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5060 "parser.cc"
    break;

  case 475: // special_id: "javascript"
#line 1544 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5066 "parser.cc"
    break;

  case 476: // special_id: "group"
#line 1545 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5072 "parser.cc"
    break;

  case 477: // special_id: "stock"
#line 1546 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5078 "parser.cc"
    break;

  case 478: // special_id: "anyclass"
#line 1547 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5084 "parser.cc"
    break;

  case 479: // special_id: "via"
#line 1548 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5090 "parser.cc"
    break;

  case 480: // special_id: "unit"
#line 1549 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5096 "parser.cc"
    break;

  case 481: // special_id: "dependency"
#line 1550 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5102 "parser.cc"
    break;

  case 482: // special_id: "signature"
#line 1551 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5108 "parser.cc"
    break;

  case 483: // special_sym: "."
#line 1553 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5114 "parser.cc"
    break;

  case 484: // special_sym: "*"
#line 1554 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5120 "parser.cc"
    break;

  case 485: // qconid: conid
#line 1558 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5126 "parser.cc"
    break;

  case 486: // qconid: "QCONID"
#line 1559 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5132 "parser.cc"
    break;

  case 487: // conid: "CONID"
#line 1561 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5138 "parser.cc"
    break;

  case 488: // qconsym: consym
#line 1563 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5144 "parser.cc"
    break;

  case 489: // qconsym: "QCONSYM"
#line 1564 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5150 "parser.cc"
    break;

  case 490: // consym: "CONSYM"
#line 1566 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5156 "parser.cc"
    break;

  case 491: // consym: ":"
#line 1567 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5162 "parser.cc"
    break;

  case 492: // literal: "CHAR"
#line 1571 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5168 "parser.cc"
    break;

  case 493: // literal: "STRING"
#line 1572 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5174 "parser.cc"
    break;

  case 494: // literal: "INTEGER"
#line 1573 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5180 "parser.cc"
    break;

  case 495: // literal: "RATIONAL"
#line 1574 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5186 "parser.cc"
    break;

  case 496: // literal: "PRIMINTEGER"
#line 1575 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5192 "parser.cc"
    break;

  case 498: // close: error
#line 1583 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5198 "parser.cc"
    break;

  case 499: // modid: "CONID"
#line 1587 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5204 "parser.cc"
    break;

  case 500: // modid: "QCONID"
#line 1588 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5210 "parser.cc"
    break;

  case 501: // commas: commas ","
#line 1590 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5216 "parser.cc"
    break;

  case 502: // commas: ","
#line 1591 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5222 "parser.cc"
    break;


#line 5226 "parser.cc"

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

  const short parser::yytable_ninf_ = -462;

  const short
  parser::yypact_[] =
  {
      71,   254,  -665,    97,  -665,  -665,  -665,  -665,  -665,   397,
     -34,    16,  -665,    52,   -32,   -32,    -5,  -665,  -665,  -665,
    -665,   137,  -665,  -665,  -665,    27,  -665,    89,   108,  4514,
     173,   169,    98,  -665,   956,  -665,   253,  -665,  -665,  -665,
    -665,   254,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,   311,  -665,  -665,  -665,  -665,   118,
     102,  -665,   131,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
     181,  -665,   254,  -665,   168,  -665,  2463,  4409,   265,   202,
     229,  2463,  -665,  -665,  -665,   369,   312,  -665,   788,   328,
     229,  3143,   218,  4753,   270,  3143,  3143,  2735,  3143,  1783,
    1647,   268,  -665,  -665,  -665,  -665,  -665,  -665,  -665,    64,
     218,   231,    98,  -665,  -665,  -665,  -665,    67,   -19,  -665,
    -665,   638,  -665,  2871,  -665,   299,  -665,  -665,  -665,  -665,
    -665,  -665,   298,     5,  -665,  -665,  -665,  -665,   277,  -665,
     281,  -665,  -665,  -665,  -665,   309,  -665,   316,   321,   344,
    -665,  -665,  -665,  4514,  4551,  -665,  -665,  -665,  -665,   433,
    -665,  1647,   436,   399,  -665,  -665,  -665,  4409,  4409,  -665,
    4852,  3571,  3255,   377,  -665,   441,   417,  -665,   766,  -665,
    3758,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  4409,  3859,
    2191,  2191,  -665,   392,   427,   434,   435,   437,  3859,  1375,
    1375,  -665,   501,  4409,  4409,    13,   439,  1220,    75,   476,
    -665,  -665,    -9,  4753,  -665,   152,   -20,   409,   233,  -665,
      88,  -665,  -665,  -665,  -665,  3007,  -665,  2871,  -665,  -665,
    -665,  4652,  -665,  -665,  -665,   399,   127,   422,   402,  -665,
    2463,  -665,  -665,  -665,  -665,  -665,  -665,  4975,  -665,  -665,
     166,   125,   192,   321,   420,   421,   426,   225,  -665,   282,
    3859,  4753,  4753,  -665,   144,   168,   451,   393,  4409,  3859,
    4852,  2463,  2599,  4652,  -665,    32,  -665,  -665,  2463,  -665,
    -665,  -665,  -665,  4409,  -665,  4975,  4716,  3143,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,   431,   438,   416,  -665,   442,
      52,   254,    30,   324,  3859,  -665,  -665,   126,    99,   443,
     430,  -665,  -665,  -665,  -665,   449,   479,   472,  -665,  -665,
     455,  -665,  -665,  -665,  -665,  -665,  -665,   456,   452,   457,
    -665,   261,   287,   342,  -665,  4409,  3859,   893,  4409,  -665,
    -665,  -665,  4409,  -665,  -665,   498,  -665,   462,   460,   312,
     229,   499,   504,   110,  -665,  -665,    45,  -665,   555,  -665,
    -665,  -665,  -665,  -665,  -665,   565,   112,  -665,  -665,   638,
      49,  2463,  -665,   512,   384,  3859,   161,  3859,   463,   461,
     488,   521,  -665,   523,   492,   243,   270,   524,  2463,  -665,
     490,   491,  2463,  2463,  2599,  1919,  -665,  1919,  1080,  -665,
    -665,  4975,  -665,  -665,  1919,  -665,  1919,   122,  -665,  -665,
    -665,  -665,   540,   541,   542,  4815,   506,  -665,  -665,  -665,
    -665,  -665,  3960,   -11,   386,  -665,  -665,  -665,  -665,   601,
     548,   513,  -665,   514,   312,  -665,  -665,  -665,  -665,  -665,
    -665,   530,  -665,   517,   557,   544,   547,  -665,  -665,  -665,
    4615,  -665,  -665,  -665,   529,  4514,  -665,  -665,  2055,  1511,
    -665,  -665,   536,  3859,  -665,  4852,  5012,  -665,  3859,  3859,
    -665,  -665,  3859,  -665,  -665,  -665,  1239,  1239,  -665,  -665,
    -665,   531,   533,   412,  -665,  3859,  -665,  -665,  3859,   501,
    -665,  2463,  -665,  2191,  -665,  2463,   343,  -665,  -665,  1375,
    -665,  -665,  3859,  3859,  5125,   559,  -665,  -665,   495,  -665,
    -665,  4852,   545,  -665,  -665,  -665,  -665,   546,   938,   294,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,   552,  -665,
     572,  -665,  -665,  -665,  -665,  -665,  -665,  3859,  3859,   554,
     556,   144,  -665,   386,   573,  -665,  -665,   598,  3859,   656,
     658,   683,  -665,  2463,  2599,  -665,  -665,  -665,  4716,  1919,
    4975,  -665,  4514,   580,  -665,  2327,  -665,   590,   578,  -665,
     368,    52,  -665,  -665,  -665,  -665,  3859,  5211,  5211,  -665,
    -665,  -665,  -665,  -665,   584,   661,  3672,  -665,  -665,   165,
    -665,    66,  -665,  -665,  -665,  -665,  -665,  -665,   392,  1098,
    1098,  -665,  -665,  -665,  -665,  -665,  5211,   673,   622,  -665,
    -665,  -665,  2599,  2463,  -665,   -13,    10,  -665,  -665,  -665,
    4889,   658,   683,  4409,  -665,  -665,  -665,   626,  -665,  4409,
     349,   683,   220,  -665,   683,  -665,  -665,  -665,  -665,  -665,
     -12,  -665,   600,  -665,  -665,  -665,  2463,  2599,  2463,  -665,
      36,  -665,  -665,  -665,    18,   635,  -665,  -665,  4409,  4409,
    4409,  -665,   407,  -665,  1239,  -665,   707,  -665,   702,  -665,
     702,  -665,   190,  -665,    79,  -665,   639,   352,  -665,  3859,
    -665,  -665,  -665,  3859,  -665,  4409,  4409,   683,  -665,  -665,
    5049,   656,   636,  3361,  -665,  -665,  -665,   392,   392,  -665,
    -665,  -665,  -665,  4046,   252,   676,  -665,  -665,  -665,  1919,
    4975,  -665,  -665,   640,   601,  -665,  -665,  3859,  -665,  3859,
     498,  -665,   413,  3859,  4151,  -665,  -665,  2463,  -665,  4409,
     451,  -665,  1098,  -665,  5211,  4237,  4323,  -665,  -665,  -665,
    -665,   644,   412,  -665,  -665,  -665,  4409,   607,  -665,  4409,
     191,  -665,   270,    81,  -665,  -665,   619,   627,  -665,  4409,
    -665,  -665,  -665,  2463,  -665,   634,   630,  -665,  5158,  -665,
    -665,  3255,   660,   662,  -665,  -665,  3960,  -665,  5211,  -665,
     642,   236,  -665,    52,    83,  4409,  3466,  -665,  4409,  -665,
     392,   130,  -665,  4409,  -665,  -665,  -665,  -665,  -665,   635,
    5211,   386,  -665,  -665,  -665,  4409,  -665,  -665,  -665,  -665,
    3859,  -665,  -665,   658,   683,  -665,  -665,   683,  -665,  -665
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   499,   500,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   498,   497,    12,   171,   167,     0,     0,     0,
       0,    42,    37,    15,    14,   170,     0,     6,     7,   465,
     467,     0,   466,   453,   468,   469,   470,   451,   452,   450,
     454,   455,   471,   472,   473,   474,   475,   476,   477,   478,
     479,   480,   482,   481,     0,   448,   413,   447,   411,     0,
      19,    21,    24,    32,   404,   412,    31,   442,   446,   449,
       0,    41,     0,    34,    38,   311,     0,     0,   117,     0,
       0,     0,    51,    52,    53,    84,     0,   118,     0,     0,
       0,     0,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   487,   486,   492,   493,   494,   495,   496,   275,
     275,    49,    56,    59,    60,    61,    62,   137,     0,    65,
     258,    66,   281,   284,   290,   300,   303,   305,   375,   388,
     376,   177,   304,   446,   377,   485,   306,   168,     0,    23,
       0,   462,   484,   483,   463,     0,   460,     0,     0,     0,
     461,   464,    17,     0,    27,    22,    36,    36,     3,    44,
      33,     0,     0,   280,   438,   439,   437,     0,     0,   205,
     243,     0,     0,     0,   435,   147,     0,   130,   188,   191,
     192,   196,   203,   397,   402,   204,   433,   436,     0,   217,
     358,   358,   298,   286,     0,     0,     0,     0,     0,   161,
     161,   164,     0,     0,     0,     0,     0,   188,   397,     0,
     299,   289,     0,     0,   276,     0,     0,     0,     0,   383,
     172,   381,   379,   350,   352,     0,   292,   283,   293,   491,
     389,     0,   490,   489,   314,   280,   319,     0,   320,   428,
       0,   427,   431,   459,   458,   392,   488,     0,   384,   502,
       0,     0,     0,   459,     0,   458,   392,     0,   386,     0,
       0,     0,     0,    50,     0,    57,   137,     0,     0,     0,
       0,     0,     0,     0,   259,   166,   264,   426,     0,   425,
     429,   457,   456,     0,   287,     0,   365,     0,   169,   407,
     406,   405,   444,   443,    20,     0,     0,    28,    30,     0,
       0,     0,    46,     0,     0,   207,   206,     0,     0,     0,
     244,   246,   440,   221,   401,     0,   180,     0,   184,   419,
       0,   420,   209,   418,   417,   415,   414,   218,     0,     0,
     416,     0,     0,     0,    67,     0,     0,     0,     0,   200,
     408,   201,     0,   197,   199,   121,   218,     0,   216,     0,
       0,   362,     0,     0,   357,   359,     0,   285,     0,    81,
      80,    82,    83,   213,   174,   157,     0,   260,   160,     0,
       0,     0,    77,     0,   123,     0,     0,     0,     0,     0,
       0,     0,   270,     0,     0,     0,     0,     0,     0,   351,
       0,     0,   315,   321,     0,     0,   310,     0,   316,   313,
     445,     0,   309,   307,     0,   308,     0,   443,   378,   385,
     501,   387,     0,     0,     0,     0,   267,   422,    55,   421,
     423,   390,     0,     0,   119,   266,   185,   175,   176,   166,
       0,   330,   332,     0,     0,   262,   263,   282,   288,   302,
     368,     0,   364,   367,   370,     0,   446,   291,    26,    25,
       0,     9,    10,    43,     0,     0,    40,    45,     0,     0,
     297,   279,     0,     0,   208,     0,     0,   211,     0,     0,
     400,   212,     0,   403,   398,   399,   143,   143,   146,   129,
     189,     0,     0,   193,   198,     0,    72,    63,     0,   363,
     360,     0,   353,   356,   354,     0,     0,    76,   162,   159,
     163,   295,     0,     0,     0,    89,   227,    73,     0,    74,
      68,     0,     0,   277,   269,   271,   380,     0,     0,     0,
     173,   394,   382,   268,   294,   432,   393,   323,   325,   329,
     314,   327,   326,   312,   318,   317,   274,     0,     0,     0,
       0,     0,   221,   119,     0,   134,   136,     0,     0,   240,
     230,   248,   261,     0,     0,   430,   165,   301,     0,     0,
       0,    29,     0,     0,   335,     0,   348,     0,   337,   341,
       0,     0,   336,   441,   247,   245,     0,     0,     0,   220,
     222,   225,   181,   183,   219,   110,     0,   138,   142,     0,
     139,     0,   409,   434,   122,   219,   361,   355,   286,   153,
     153,   156,   158,   104,   124,   125,     0,    94,     0,   278,
     395,   396,     0,   322,   178,     0,     0,   424,   391,    54,
       0,   230,   248,     0,   135,   120,   221,   234,   236,     0,
       0,   248,     0,    69,   249,   251,   265,   331,   366,   369,
     372,   374,     0,    47,   338,   333,   340,     0,     0,   342,
     166,   346,   334,   182,     0,     0,   210,   111,     0,     0,
       0,   108,   126,   144,   141,   145,     0,   117,   112,   148,
     112,   152,     0,   149,     0,    90,     0,     0,    71,     0,
     328,   324,   272,     0,   273,     0,     0,   248,    78,   133,
       0,   240,     0,   241,   194,   202,   238,   286,   286,    70,
      87,    85,    86,     0,     0,   252,   255,   410,   250,     0,
       0,    48,   339,     0,   166,   344,   345,     0,   223,     0,
     121,   109,   126,     0,     0,   106,   140,     0,   113,     0,
     137,   154,   151,   155,     0,   103,   103,    95,    64,   179,
     132,     0,   186,    79,   239,   235,     0,     0,   195,     0,
       0,   232,     0,     0,   256,   190,   214,     0,   253,     0,
     254,   371,   373,     0,   343,     0,     0,   105,     0,   107,
     127,     0,     0,   204,   296,   114,     0,   150,    91,    93,
       0,     0,   102,     0,     0,     0,   241,   237,   242,   228,
     286,     0,   229,     0,   257,    88,   347,   224,   226,   204,
       0,   119,    92,    98,    96,   101,    99,    97,   131,   231,
       0,   215,   128,   230,   248,   100,   233,   248,   115,   116
  };

  const short
  parser::yypgoto_[] =
  {
    -665,  -665,  -665,  -665,  -665,  -665,  -665,    47,  -665,  -665,
    -422,  -665,   582,  -665,  -665,  -665,  -131,   629,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,   -62,  -665,  -665,  -665,     3,  -197,  -665,  -665,    72,
    -665,   721,  -517,    29,  -665,    28,   483,   -22,  -270,    91,
     283,  -665,  -665,    26,   159,  -665,  -665,   561,  -665,  -299,
    -394,   757,  -665,  -665,  -295,    84,  -158,   226,  -161,   391,
    -665,   -82,  -665,   -83,  -665,    11,  -665,  -344,  -665,  -665,
    -665,  -652,  -165,   505,   -26,  -665,   577,  -497,   267,  -664,
    -360,  -590,    74,   -21,  -514,  -665,    82,  -665,    33,  -665,
    -665,   310,  -592,  -665,   140,    77,   759,  -186,  -665,  -665,
     509,  -665,   387,  -665,   285,    68,  -244,  -177,   688,    56,
    -665,  -665,  -665,   -80,  -665,  -665,  -665,  -665,   139,  -665,
    -665,  -421,  -665,   141,  -665,  -665,   138,  -665,  -665,   564,
    -665,   -68,   602,   301,  -261,  -665,   234,  -665,  -665,  -665,
    -665,   405,    43,  -665,   -95,  -631,   -69,  -665,   414,   -67,
    -665,  -665,  -665,   -27,  -665,  -187,  -665,   257,  -665,   566,
    -665,  -665,  -665,  -418,  -665,  -312,  -246,   -15,  -230,  -173,
      -6,  -665,  -665,     9,   -54,   -92,   -87,  -665,  -142,   -97,
     -63,  -220,  -665,  -233,    -4,  -108
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   168,     6,    10,    19,    30,
      69,    70,    71,   165,   306,   307,    72,    84,    11,    20,
      21,    32,    82,   312,   466,   467,   274,   121,   426,    33,
      34,   122,   123,   124,   125,   215,   126,   208,   714,   770,
     617,   685,   788,   688,   747,   791,   792,   597,   668,   739,
     679,   680,   560,   496,   515,   735,   185,   553,   278,   598,
     599,   488,   344,   681,   682,   611,   507,   376,   211,   212,
     445,    27,    36,   397,   373,   435,   128,   625,   337,   516,
     437,   327,   702,   328,   766,   188,   189,   703,   190,   353,
     348,   704,   191,   375,   767,   357,   338,   476,   589,   590,
     517,   641,   760,   761,   561,   637,   638,   639,   706,   319,
     320,   321,   643,   644,   645,   715,   377,   600,   284,   285,
     286,   130,   223,   224,   244,   173,   132,   762,   133,   134,
     135,   136,   260,   261,   262,   247,   248,   538,   440,   441,
     470,   577,   578,   579,   659,   660,   661,   580,   362,   234,
     235,   202,   363,   364,   365,   451,   452,   453,   650,   137,
     138,   229,   230,   139,   140,   427,   249,   530,   192,   193,
      73,   349,   716,   194,    75,   339,   340,   428,   429,   288,
     250,   289,   251,   195,   351,   196,   141,   142,   455,    77,
      78,   290,   252,   253,   292,   160,    79,   161,   144,   145,
     255,   256,   146,    24,     9,   267
  };

  const short
  parser::yytable_[] =
  {
     197,   350,    74,   269,   187,   186,   432,   232,   394,   231,
     159,   197,   315,   316,    76,   216,   382,   254,   265,   471,
     325,   442,   318,   378,   378,   354,   368,   409,   143,   246,
     350,   218,   220,   308,   438,   492,   631,   149,   356,   632,
     698,   697,   464,   573,   447,   562,    22,   266,   581,   709,
      22,   758,    13,    22,   431,   630,   264,   392,   591,   444,
     499,   279,   287,   444,   692,   449,   556,    22,   400,   719,
     782,    17,   759,   158,   341,   342,    25,   462,   169,   265,
      22,   389,    22,   270,    22,  -440,   276,   694,   226,   720,
     197,   197,     1,   385,   197,   197,   591,    12,   727,   401,
     280,    26,   131,   197,   287,   753,   693,    29,   266,   217,
     443,   197,   197,   393,   782,   355,   282,   159,   592,   263,
     657,   197,   557,   728,  -440,   390,   197,   197,   448,   693,
     383,   384,   386,   504,    18,   604,    74,    74,   277,   700,
     291,   401,   465,   539,   758,   566,   758,   510,    76,    76,
     652,    31,    23,   614,   654,   -75,    23,   221,   447,    23,
      35,   233,   236,   503,   238,   759,    37,   509,   395,   664,
     665,   159,   222,    23,   322,     2,   287,   245,   245,   473,
     263,   543,   291,   197,   674,    38,    23,   494,    23,   294,
      23,   197,   197,    81,   -75,   187,   186,   742,   635,   800,
      80,   815,  -441,   143,   143,   403,   197,   396,   391,   217,
     820,   404,   591,   309,   310,   502,    83,   508,   280,   151,
     430,   163,   152,   239,   217,   217,   520,   197,   503,   153,
     509,   162,   828,   827,   158,   829,   582,   151,   413,   245,
     152,  -441,   710,   164,   414,   151,   405,   153,   152,   396,
     154,   410,   624,   624,   291,   153,   423,   424,   197,   197,
     197,   197,   489,   472,   425,   197,   725,   411,   154,   242,
     673,   711,   712,   518,   322,   618,   154,   379,   379,   412,
     156,   454,   591,   674,    66,   166,   170,   167,    68,   410,
     456,   233,   500,   294,   823,   741,   799,   824,   197,   232,
     197,   231,   198,   647,   686,   415,   350,   463,   742,   800,
     287,   416,   239,   254,   199,   254,   783,   318,   408,   519,
     491,   594,   254,   612,   254,   541,   222,   542,   550,   571,
     774,   431,   713,   200,   544,   201,   545,   605,   419,   287,
     651,   814,   219,    66,   420,   197,   258,    68,   662,   555,
     554,   527,   259,   457,   815,   528,   217,   529,   242,   493,
     591,   690,   147,   809,   713,   273,    66,   775,   675,   776,
      68,   172,   148,   780,   484,    66,   203,     7,   227,    68,
     420,     8,   228,   268,   111,   297,   197,   259,   291,   197,
     150,   197,   197,   112,   299,   197,   442,   421,   749,   671,
     295,   420,   485,   296,   151,   410,   420,   152,   197,   621,
     298,   197,   796,   259,   153,   798,   209,   291,   210,   549,
     341,   342,   300,   683,   683,   197,   197,   197,   468,   301,
     469,   676,   789,    74,   302,   154,   155,   311,    74,   156,
     157,   204,   205,   206,   207,    76,   486,   609,   487,   610,
      76,   743,   657,   707,   658,   708,   745,   303,   746,   313,
     197,   197,    14,    15,   513,   514,   558,   559,   343,   322,
     786,   197,   731,   245,   705,   245,   812,   254,   239,   314,
     143,   143,   245,   431,   245,   361,   361,   733,   734,   649,
     772,   329,   151,   733,   778,   152,   259,   430,   686,   197,
     197,   197,   153,   143,   369,   331,   271,   272,   345,   197,
     367,   370,   371,   672,   372,   322,   381,   388,   240,   283,
     387,   407,   277,   154,   242,   826,   433,   156,   243,   197,
     802,   406,   347,   417,  -461,   460,   333,   334,   705,   418,
     335,   336,   785,   197,   458,    74,   197,   461,   474,   475,
     699,   459,   197,   454,   379,   379,   683,    76,   477,   478,
     816,   817,   456,   479,   410,   350,   439,   361,   480,   481,
     483,   482,   326,   326,   329,   497,   431,   379,   495,   498,
     505,   197,   197,   197,  -349,   730,   383,   732,   331,   501,
     326,   705,   506,   512,   705,   522,   521,   523,   524,   374,
     525,   533,   197,   143,   143,   526,   197,   217,   197,   197,
     535,   536,   750,   197,   751,   717,   197,   546,   825,   333,
     334,   547,   548,   335,   336,   551,   197,   254,   444,   563,
     765,   705,   564,   705,   565,   567,   568,   245,   569,   771,
     197,   572,   197,   616,   217,   570,   197,   197,  -445,   583,
     623,   602,   197,   603,   619,   324,   383,   197,   197,   197,
     550,   374,   383,   383,   633,   232,   511,   231,   143,   197,
     436,   622,   197,   341,   627,   634,   628,   379,   379,   217,
     217,   217,   197,   534,   636,   640,   805,   717,   537,   361,
     540,   197,   642,   653,   197,   655,   656,   666,   667,   197,
     687,   197,   689,   555,   554,   374,   217,   752,   197,   197,
     701,   197,   818,   721,   410,   729,   197,   239,   737,   281,
     765,   738,   282,   197,   217,   744,   773,   756,   197,   769,
     112,   151,   383,   197,   152,   795,   143,   490,   803,   807,
     804,   153,   379,   808,   810,   304,  -225,   813,   822,   794,
     217,   275,   740,   576,   576,   127,   217,   217,   283,   777,
     779,   434,   154,   242,   811,   736,   156,   243,   787,   684,
     601,   380,    28,   748,   626,   422,   358,   821,   326,   819,
     217,   615,   763,   755,   718,   585,   606,   245,   361,   797,
     608,   768,    39,   129,   446,   237,   723,   722,   726,   399,
      40,   532,   648,   366,   607,   801,   217,   213,   629,   531,
     379,   402,    42,     0,   217,     0,     0,     0,    44,    45,
      46,   174,   175,   176,     0,   214,   217,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,   329,     0,     0,   646,   361,
       0,     0,   346,     0,     0,     0,     0,  -186,     0,   331,
     576,     0,     0,     0,   584,     0,     0,     0,     0,     0,
     593,     0,     0,   326,     0,     0,     0,   177,     0,     0,
       0,     0,   178,     0,   179,     0,   347,     0,     0,   326,
     333,   334,   180,     0,   335,   336,   181,    39,     0,     0,
     182,     0,   183,   613,     0,    40,     0,   361,   691,     0,
     184,    66,     0,     0,     0,    68,     0,    42,     0,     0,
       0,     0,     0,    44,    45,    46,   174,   175,   176,     0,
       0,     0,    52,    53,     0,    54,    55,    56,   374,   374,
      57,   576,   361,   724,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
      39,    86,    87,    88,    89,     0,    90,     0,    40,    91,
       0,     0,    92,    93,    94,    95,    96,   663,    97,     0,
      42,     0,    98,     0,    43,    99,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   101,    58,    59,    60,
      61,    62,    63,   102,     0,   184,    66,   329,     0,   103,
      68,     0,   784,     0,   330,     0,     0,     0,     0,     0,
       0,   331,   104,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,     0,   106,     0,     0,   107,     0,
     108,   620,     0,     0,     0,     0,     0,   259,   806,     0,
       0,     0,   333,   334,   109,     0,   335,   336,   110,     0,
     111,     0,     0,     0,     0,     0,     0,     0,    65,   112,
     436,     0,    67,   113,   374,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,     0,     0,     0,   119,   120,
       0,    85,    39,    86,     0,   677,     0,     0,    90,     0,
      40,    91,     0,     0,    92,    93,    94,     0,    96,     0,
      97,     0,    42,     0,   678,   613,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,   102,     0,     0,     0,   239,
       0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   326,   151,   104,     0,   152,     0,     0,     0,
     105,     0,     0,   153,     0,     0,     0,   106,     0,     0,
     107,     0,   108,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,   154,   242,   109,     0,   156,   243,
     110,   374,   111,     0,     0,     0,     0,     0,     0,     0,
      65,   112,     0,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,     0,     0,   118,     0,     0,     0,
     119,   120,    85,    39,    86,     0,   595,     0,     0,    90,
       0,    40,    91,     0,     0,    92,    93,    94,     0,    96,
       0,     0,     0,    42,     0,   596,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,   102,     0,     0,   329,
       0,     0,   103,     0,     0,     0,   346,     0,     0,     0,
       0,     0,     0,   331,     0,   104,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,   106,     0,
       0,   107,     0,   108,     0,     0,     0,     0,     0,     0,
     347,     0,     0,     0,   333,   334,     0,   109,   335,   336,
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
       0,     0,     0,     0,   106,     0,     0,   107,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,   110,     0,   111,
       0,     0,     0,     0,     0,     0,     0,    65,   112,     0,
       0,    67,   113,     0,     0,     0,     0,   114,   115,   116,
     117,     0,    22,   118,    85,    39,    86,   119,   120,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,    96,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,     0,
     106,     0,     0,   107,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    23,   109,
       0,     0,     0,   171,     0,   111,     0,     0,     0,   575,
       0,     0,     0,    65,   112,     0,     0,    67,   113,     0,
       0,     0,     0,   114,   115,   116,   117,     0,     0,   118,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,     0,   105,
       0,     0,     0,     0,     0,     0,   106,     0,     0,   107,
     151,   108,     0,   152,     0,     0,     0,     0,     0,   257,
     153,     0,     0,     0,     0,   109,     0,     0,     0,   171,
     258,   111,     0,     0,     0,     0,   259,   241,     0,    65,
     112,   154,   242,    67,   113,   156,   243,     0,     0,   114,
     115,   116,   117,     0,     0,   118,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   239,     0,     0,   105,     0,     0,     0,     0,
       0,     0,   106,     0,     0,   107,     0,   108,     0,   152,
       0,     0,     0,     0,     0,     0,   153,     0,     0,     0,
       0,   109,   240,     0,     0,   171,     0,   111,     0,     0,
       0,     0,     0,   241,     0,    65,   112,   154,   242,    67,
     113,   156,   243,     0,     0,   114,   115,   116,   117,     0,
       0,   118,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,    96,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   239,     0,
       0,   105,     0,     0,     0,     0,     0,     0,   106,     0,
       0,   107,     0,   108,     0,   152,     0,     0,     0,     0,
       0,     0,   153,     0,     0,     0,     0,   109,     0,     0,
       0,   171,     0,   111,     0,     0,     0,     0,     0,   241,
       0,    65,   112,   154,   242,    67,   113,   156,   243,     0,
       0,   114,   115,   116,   117,     0,     0,   118,    85,    39,
      86,     0,     0,     0,     0,    90,     0,    40,    91,     0,
       0,     0,     0,     0,     0,    96,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   100,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   101,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,     0,   106,     0,     0,   107,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     574,     0,     0,   109,     0,     0,     0,   171,     0,   111,
       0,     0,     0,   575,     0,     0,     0,    65,   112,     0,
       0,    67,   113,     0,     0,     0,     0,   114,   115,   116,
     117,     0,     0,   118,    85,    39,    86,     0,     0,     0,
       0,    90,     0,    40,    91,     0,     0,     0,     0,     0,
       0,   359,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   100,    50,    51,
      52,    53,     0,    54,    55,    56,     0,   360,    57,     0,
       0,   101,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,     0,
     106,     0,     0,   107,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,   171,     0,   111,     0,     0,     0,     0,
       0,     0,     0,    65,   112,     0,     0,    67,   113,     0,
       0,     0,     0,   114,   115,   116,   117,     0,     0,   118,
      85,    39,    86,     0,     0,     0,     0,    90,     0,    40,
      91,     0,     0,     0,     0,     0,     0,    96,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   100,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   101,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   105,
       0,     0,     0,     0,     0,     0,   106,     0,     0,   107,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   171,
       0,   111,     0,     0,     0,   575,     0,     0,     0,    65,
     112,     0,     0,    67,   113,     0,     0,     0,     0,   114,
     115,   116,   117,     0,     0,   118,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   101,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,     0,   106,     0,     0,   107,     0,   108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,   171,     0,   111,     0,     0,
       0,     0,     0,     0,     0,    65,   112,     0,     0,    67,
     113,     0,     0,     0,     0,   114,   115,   116,   117,     0,
       0,   118,    85,    39,    86,     0,     0,     0,     0,    90,
       0,    40,    91,     0,     0,     0,     0,     0,     0,   359,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   100,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   101,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,   106,     0,
       0,   107,     0,   108,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,   171,     0,   111,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,   106,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,   171,     0,   111,
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
       0,     0,     0,   105,     0,     0,     0,     0,     0,   293,
     106,     0,     0,     0,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,   171,     0,   111,     0,     0,     0,     0,
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
       0,     0,     0,   398,     0,     0,   106,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   171,
       0,   111,     0,     0,     0,     0,     0,     0,     0,    65,
     112,     0,     0,    67,   113,     0,     0,     0,     0,   114,
     115,   116,   117,     0,     0,   118,    85,    39,    86,     0,
       0,     0,     0,    90,     0,    40,    91,     0,     0,     0,
       0,     0,     0,    96,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   100,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,   171,     0,   111,     0,    39,
       0,     0,     0,     0,     0,    65,   112,    40,     0,    67,
     113,     0,     0,     0,     0,   114,   115,   116,   117,    42,
       0,   118,     0,   323,     0,    44,    45,    46,   174,   175,
     176,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   329,     0,     0,     0,     0,     0,
       0,   330,     0,     0,   177,     0,     0,     0,   331,   178,
       0,   179,     0,     0,     0,     0,     0,     0,     0,   180,
       0,     0,     0,   181,     0,    39,     0,   182,   332,   183,
       0,     0,     0,    40,   259,     0,     0,   184,    66,   333,
     334,     0,    68,   335,   336,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   174,   175,   176,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     239,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,  -187,     0,     0,   178,     0,   179,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,   181,
      39,     0,     0,   182,     0,   183,     0,     0,    40,     0,
       0,   757,     0,   184,    66,     0,   242,     0,    68,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   239,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,     0,     0,     0,
     178,     0,   179,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   181,    39,     0,     0,   182,     0,
     183,     0,     0,    40,     0,     0,   757,     0,   184,    66,
       0,   242,     0,    68,     0,    42,     0,     0,     0,   323,
       0,    44,    45,    46,   174,   175,   176,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,   178,     0,   179,     0,     0,
       0,     0,     0,     0,     0,   180,    39,     0,     0,   181,
     324,     0,     0,   182,    40,   183,     0,     0,     0,     0,
       0,   669,     0,   184,    66,     0,    42,     0,    68,     0,
       0,     0,    44,    45,    46,   174,   175,   176,     0,   670,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   177,    39,     0,     0,     0,   178,     0,   179,     0,
      40,     0,     0,     0,     0,     0,   180,     0,     0,     0,
     181,     0,    42,     0,   182,     0,   183,     0,    44,    45,
      46,   174,   175,   176,   184,    66,     0,    52,    53,    68,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   352,   177,     0,     0,
       0,     0,   178,     0,   179,     0,     0,     0,     0,     0,
       0,     0,   180,    39,     0,     0,   181,     0,     0,     0,
     182,    40,   183,     0,     0,     0,     0,     0,     0,     0,
     184,    66,     0,    42,     0,    68,     0,   323,     0,    44,
      45,    46,   174,   175,   176,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   177,     0,
       0,     0,     0,   178,     0,   179,     0,     0,     0,     0,
       0,     0,     0,   180,    39,     0,     0,   181,     0,     0,
       0,   182,    40,   183,     0,     0,     0,     0,     0,     0,
       0,   184,    66,     0,    42,     0,    68,     0,   552,     0,
      44,    45,    46,   174,   175,   176,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   177,
      39,     0,     0,     0,   178,     0,   179,     0,    40,     0,
       0,     0,     0,     0,   180,     0,     0,     0,   181,     0,
      42,     0,   182,     0,   183,     0,    44,    45,    46,   174,
     175,   176,   184,    66,     0,    52,    53,    68,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,     0,     0,     0,
     178,     0,   179,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   181,    39,     0,     0,   182,   764,
     183,     0,     0,    40,     0,     0,     0,     0,   184,    66,
       0,     0,     0,    68,     0,    42,     0,     0,     0,   323,
       0,    44,    45,    46,   174,   175,   176,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,    39,     0,     0,     0,   178,     0,   179,     0,    40,
       0,     0,     0,     0,     0,   180,     0,     0,     0,   181,
       0,    42,     0,   781,     0,   183,     0,    44,    45,    46,
     174,   175,   176,   184,    66,     0,    52,    53,    68,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   790,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,    39,     0,     0,
       0,   178,     0,   179,     0,    40,     0,     0,     0,     0,
       0,   180,     0,     0,     0,   181,     0,    42,     0,   182,
       0,   183,     0,    44,    45,    46,   174,   175,   176,   184,
      66,     0,    52,    53,    68,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   793,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   177,    39,     0,     0,     0,   178,     0,   179,
       0,    40,     0,     0,     0,     0,     0,   180,     0,     0,
       0,   181,     0,    42,     0,   182,     0,   183,     0,    44,
      45,    46,   174,   175,   176,   184,    66,     0,    52,    53,
      68,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   177,     0,
       0,     0,     0,   178,     0,   179,     0,     0,     0,     0,
       0,     0,     0,   180,     0,     0,     0,   181,    39,     0,
       0,   182,     0,   183,     0,     0,    40,     0,     0,     0,
       0,   184,    66,     0,     0,    41,    68,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,    39,    54,    55,    56,     0,
       0,    57,     0,    40,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,    64,    40,     0,   305,
       0,     0,     0,     0,     0,     0,    65,    66,     0,    42,
      67,    68,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    39,    54,    55,    56,
       0,     0,    57,    64,    40,     0,    58,    59,    60,    61,
      62,    63,     0,    65,    66,     0,    42,    67,    68,     0,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,    64,    40,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,     0,
      42,    67,    68,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,    39,    54,    55,
      56,     0,     0,    57,     0,    40,     0,    58,    59,    60,
      61,    62,    63,     0,    65,   112,     0,    42,    67,   113,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,   450,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,     0,    40,   225,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    42,
       0,     0,    67,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    39,    54,    55,    56,
       0,     0,    57,     0,    40,   225,    58,    59,    60,    61,
      62,    63,     0,     0,     0,    65,    42,     0,     0,    67,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,    39,    54,    55,    56,     0,     0,    57,
       0,    40,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,    42,     0,     0,     0,     0,     0,    44,
      45,    46,   174,   175,   176,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,    65,   112,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   317,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,     0,     0,     0,     0,    39,
     695,     0,     0,     0,     0,     0,     0,    40,     0,     0,
       0,     0,   696,   587,     0,     0,     0,     0,     0,    42,
       0,   588,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   184,    50,    51,    52,    53,    39,    54,    55,    56,
       0,     0,    57,     0,    40,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   174,   175,   176,     0,     0,
       0,    52,    53,    39,    54,    55,    56,     0,     0,    57,
       0,    40,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,    42,     0,     0,     0,     0,     0,    44,
      45,    46,   174,   175,   176,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,    65,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   586,   587,     0,     0,     0,
       0,     0,     0,     0,   588,     0,     0,     0,     0,    39,
       0,     0,     0,     0,   184,     0,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,   754,   587,     0,    44,    45,    46,   174,   175,
     176,   588,    39,     0,    52,    53,     0,    54,    55,    56,
      40,   184,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   174,   175,   176,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,     0,     0,   587,
       0,     0,     0,     0,     0,    42,     0,   588,     0,     0,
       0,    44,    45,    46,   174,   175,   176,   184,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
     588,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   184
  };

  const short
  parser::yycheck_[] =
  {
      87,   188,    29,   111,    87,    87,   276,   104,   228,   104,
      64,    98,   177,   178,    29,    98,   213,   109,   110,   314,
     181,   282,   180,   209,   210,   190,   203,   257,    34,   109,
     217,    98,   100,   164,   280,   347,   553,    41,   199,   553,
     632,   631,    12,   465,   288,   439,     1,   110,   469,   641,
       1,   703,     5,     1,   274,   552,   110,    77,   476,    27,
     359,    80,   131,    27,    77,   295,    77,     1,   241,    81,
     734,   105,   703,    64,   182,   183,   108,   310,    82,   171,
       1,    90,     1,    19,     1,    80,    19,    77,   103,   101,
     177,   178,    21,    80,   181,   182,   514,     0,    80,   241,
     119,   133,    34,   190,   173,   697,   119,   112,   171,    98,
     283,   198,   199,   133,   778,   198,    84,   171,   478,   110,
      84,   208,   133,   105,   119,   134,   213,   214,   293,   119,
     213,   214,   119,   366,   118,   495,   163,   164,    71,   636,
     131,   283,   112,   404,   796,   444,   798,   380,   163,   164,
     572,    14,   107,   513,   575,    80,   107,   101,   402,   107,
     133,   105,   106,   118,   108,   796,    77,   118,    80,   587,
     588,   225,   108,   107,   180,   104,   245,   109,   110,    80,
     171,   411,   173,   270,   118,    77,   107,   352,   107,   133,
     107,   278,   279,    24,   119,   278,   278,   118,   558,   118,
      27,   118,    80,   209,   210,    78,   293,   119,   223,   198,
      80,    84,   630,   166,   167,   105,   118,   105,   119,    93,
     274,   119,    96,    79,   213,   214,   387,   314,   118,   103,
     118,   113,   824,   823,   225,   827,   469,    93,   113,   171,
      96,   119,    22,   112,   119,    93,   119,   103,    96,   119,
     124,   257,   547,   548,   245,   103,   271,   272,   345,   346,
     347,   348,   345,   317,   120,   352,   660,   101,   124,   125,
     105,    51,    52,   112,   280,   521,   124,   209,   210,   113,
     128,   296,   700,   118,   123,   104,   118,   106,   127,   295,
     296,   235,   360,   237,   811,   105,   105,   811,   385,   396,
     387,   396,    37,   564,   616,   113,   493,   311,   118,   118,
     379,   119,    79,   405,   112,   407,   734,   475,   250,   386,
     347,   482,   414,   509,   416,   405,   108,   407,   425,   460,
     724,   551,   112,   104,   414,   106,   416,   498,   113,   408,
     570,   105,    14,   123,   119,   432,   113,   127,   581,   432,
     432,   108,   119,   297,   118,   112,   345,   114,   125,   348,
     778,   622,   109,   781,   112,   134,   123,   727,   601,   729,
     127,    86,   119,   733,   113,   123,    91,   123,   108,   127,
     119,   127,   112,   115,   114,    87,   473,   119,   379,   476,
      79,   478,   479,   123,   113,   482,   657,   115,   693,   596,
     101,   119,   115,   104,    93,   411,   119,    96,   495,   115,
     133,   498,   756,   119,   103,   759,   104,   408,   106,   425,
     528,   529,   113,   609,   610,   512,   513,   514,   104,   113,
     106,   608,   744,   460,   113,   124,   125,     4,   465,   128,
     129,    72,    73,    74,    75,   460,   104,   104,   106,   106,
     465,   684,    84,   104,    86,   106,   104,   113,   106,    23,
     547,   548,    65,    66,    80,    81,    80,    81,    27,   475,
     740,   558,   669,   405,   639,   407,   788,   569,    79,    80,
     486,   487,   414,   703,   416,   200,   201,    80,    81,   569,
     720,    79,    93,    80,    81,    96,   119,   551,   810,   586,
     587,   588,   103,   509,    77,    93,   119,   120,    91,   596,
     118,    77,    77,   596,    77,   521,    15,    41,   109,   120,
      81,   119,    71,   124,   125,   820,   133,   128,   129,   616,
     763,   109,   120,   113,   113,   119,   124,   125,   703,   113,
     128,   129,   739,   630,   113,   572,   633,   105,   105,   119,
     633,   113,   639,   568,   486,   487,   742,   572,   109,    80,
     793,   794,   568,    91,   570,   752,   281,   282,   113,   113,
     113,   119,   181,   182,    79,   113,   796,   509,    80,   119,
      25,   668,   669,   670,    85,   668,   669,   670,    93,    85,
     199,   756,    27,    81,   759,   134,   133,   109,    77,   208,
      77,    77,   689,   609,   610,   113,   693,   596,   695,   696,
     120,   120,   695,   700,   696,   642,   703,    77,   815,   124,
     125,    80,    80,   128,   129,   119,   713,   719,    27,    81,
     713,   796,   119,   798,   120,   105,   119,   569,    81,   719,
     727,   112,   729,    84,   633,   101,   733,   734,   101,   113,
      78,   120,   739,   120,   109,   109,   739,   744,   745,   746,
     757,   270,   745,   746,    91,   762,   381,   762,   674,   756,
     279,   119,   759,   781,   120,    77,   120,   609,   610,   668,
     669,   670,   769,   398,    28,    27,   769,   714,   403,   404,
     405,   778,     9,   113,   781,   105,   118,   113,    37,   786,
      27,   788,    80,   786,   786,   314,   695,   696,   795,   796,
      84,   798,   795,   113,   720,    80,   803,    79,    11,    81,
     803,    19,    84,   810,   713,    86,    86,    91,   815,    53,
     123,    93,   815,   820,    96,    91,   742,   346,   119,   105,
     113,   103,   674,   113,    84,   163,    84,   105,   810,   746,
     739,   122,   680,   468,   469,    34,   745,   746,   120,   730,
     732,   278,   124,   125,   786,   674,   128,   129,   742,   610,
     487,   210,    15,   689,   548,   270,   199,   803,   387,   800,
     769,   514,   708,   701,   644,   475,   501,   719,   503,   756,
     505,   714,     4,    34,   285,   107,   657,   656,   660,   235,
      12,   396,   568,   201,   503,   762,   795,    19,   551,   395,
     742,   245,    24,    -1,   803,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,   815,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    79,    -1,    -1,   563,   564,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    91,    -1,    93,
     575,    -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,
     479,    -1,    -1,   482,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    94,    -1,    96,    -1,   120,    -1,    -1,   498,
     124,   125,   104,    -1,   128,   129,   108,     4,    -1,    -1,
     112,    -1,   114,   512,    -1,    12,    -1,   622,   623,    -1,
     122,   123,    -1,    -1,    -1,   127,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,   547,   548,
      47,   656,   657,   658,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,    -1,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    19,    20,   586,    22,    -1,
      24,    -1,    26,    -1,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,   122,   123,    79,    -1,    63,
     127,    -1,   737,    -1,    86,    -1,    -1,    -1,    -1,    -1,
      -1,    93,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,   113,    -1,    -1,    -1,    -1,    -1,   119,   773,    -1,
      -1,    -1,   124,   125,   108,    -1,   128,   129,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
     689,    -1,   126,   127,   693,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,    -1,    -1,    -1,   142,   143,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      22,    -1,    24,    -1,    26,   734,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    79,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   781,    93,    76,    -1,    96,    -1,    -1,    -1,
      82,    -1,    -1,   103,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,    -1,    -1,    -1,   124,   125,   108,    -1,   128,   129,
     112,   820,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,    -1,    -1,    -1,
     142,   143,     3,     4,     5,    -1,     7,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    24,    -1,    26,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    79,
      -1,    -1,    63,    -1,    -1,    -1,    86,    -1,    -1,    -1,
      -1,    -1,    -1,    93,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
     120,    -1,    -1,    -1,   124,   125,    -1,   108,   128,   129,
      -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,
      -1,   132,   133,   134,   135,    -1,    -1,   138,     3,     4,
       5,   142,   143,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,
      -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,
     135,    -1,     1,   138,     3,     4,     5,   142,   143,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
      -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,   118,
      -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,
      -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,   102,
     103,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,
     113,   114,    -1,    -1,    -1,    -1,   119,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,   132,
     133,   134,   135,    -1,    -1,   138,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,    96,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,   120,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,    -1,    -1,   132,   133,   134,   135,    -1,
      -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,   120,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,    -1,
      -1,   132,   133,   134,   135,    -1,    -1,   138,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,
      -1,    -1,    -1,   118,    -1,    -1,    -1,   122,   123,    -1,
      -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,
     135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    46,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,
      -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,
      -1,   114,    -1,    -1,    -1,   118,    -1,    -1,    -1,   122,
     123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,
     133,   134,   135,    -1,    -1,   138,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,
     127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,
      -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,
      -1,   132,   133,   134,   135,    -1,    -1,   138,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,
      -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,
     135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      89,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,
      -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    86,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,
     133,   134,   135,    -1,    -1,   138,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,     4,
      -1,    -1,    -1,    -1,    -1,   122,   123,    12,    -1,   126,
     127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,    24,
      -1,   138,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    -1,    89,    -1,    -1,    -1,    93,    94,
      -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,     4,    -1,   112,   113,   114,
      -1,    -1,    -1,    12,   119,    -1,    -1,   122,   123,   124,
     125,    -1,   127,   128,   129,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    91,    -1,    -1,    94,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
       4,    -1,    -1,   112,    -1,   114,    -1,    -1,    12,    -1,
      -1,   120,    -1,   122,   123,    -1,   125,    -1,   127,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,     4,    -1,    -1,   112,    -1,
     114,    -1,    -1,    12,    -1,    -1,   120,    -1,   122,   123,
      -1,   125,    -1,   127,    -1,    24,    -1,    -1,    -1,    28,
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
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,     4,    -1,    -1,   112,   113,
     114,    -1,    -1,    12,    -1,    -1,    -1,    -1,   122,   123,
      -1,    -1,    -1,   127,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,     4,    -1,    -1,    -1,    94,    -1,    96,    -1,    12,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,
      -1,    24,    -1,   112,    -1,   114,    -1,    30,    31,    32,
      33,    34,    35,   122,   123,    -1,    39,    40,   127,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,     4,    -1,    -1,
      -1,    94,    -1,    96,    -1,    12,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,   108,    -1,    24,    -1,   112,
      -1,   114,    -1,    30,    31,    32,    33,    34,    35,   122,
     123,    -1,    39,    40,   127,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,     4,    -1,    -1,    -1,    94,    -1,    96,
      -1,    12,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,    24,    -1,   112,    -1,   114,    -1,    30,
      31,    32,    33,    34,    35,   122,   123,    -1,    39,    40,
     127,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,     4,    -1,
      -1,   112,    -1,   114,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   122,   123,    -1,    -1,    21,   127,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    12,    -1,    78,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    24,
     126,   127,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,   112,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,   122,   123,    -1,    24,   126,   127,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,   112,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,
      24,   126,   127,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,   122,   123,    -1,    24,   126,   127,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    78,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    24,
      -1,    -1,   126,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,   112,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,   122,    24,    -1,    -1,   126,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,   122,   123,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,     4,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   103,   104,    -1,    -1,    -1,    -1,    -1,    24,
      -1,   112,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,   122,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,   122,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   104,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,   122,    -1,    -1,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,   103,   104,    -1,    30,    31,    32,    33,    34,
      35,   112,     4,    -1,    39,    40,    -1,    42,    43,    44,
      12,   122,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,    -1,    -1,    24,    -1,   112,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,   122,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   145,   146,   147,   150,   123,   127,   348,
     151,   162,     0,   151,    65,    66,   148,   105,   118,   152,
     163,   164,     1,   107,   347,   108,   133,   215,   215,   112,
     153,    14,   165,   173,   174,   133,   216,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   112,   122,   123,   126,   127,   154,
     155,   156,   160,   314,   317,   318,   331,   333,   334,   340,
      27,    24,   166,   118,   161,     3,     5,     6,     7,     8,
      10,    13,    16,    17,    18,    19,    20,    22,    26,    29,
      36,    50,    57,    63,    76,    82,    89,    92,    94,   108,
     112,   114,   123,   127,   132,   133,   134,   135,   138,   142,
     143,   171,   175,   176,   177,   178,   180,   195,   220,   260,
     265,   269,   270,   272,   273,   274,   275,   303,   304,   307,
     308,   330,   331,   334,   342,   343,   346,   109,   119,   348,
      79,    93,    96,   103,   124,   125,   128,   129,   337,   338,
     339,   341,   113,   119,   112,   157,   104,   106,   149,   348,
     118,   112,   268,   269,    33,    34,    35,    89,    94,    96,
     104,   108,   112,   114,   122,   200,   225,   227,   229,   230,
     232,   236,   312,   313,   317,   327,   329,   340,    37,   112,
     104,   106,   295,   268,    72,    73,    74,    75,   181,   104,
     106,   212,   213,    19,    37,   179,   227,   229,   313,    14,
     295,   273,   108,   266,   267,   112,   331,   108,   112,   305,
     306,   308,   343,   273,   293,   294,   273,   272,   273,    79,
     109,   120,   125,   129,   268,   269,   277,   279,   280,   310,
     324,   326,   336,   337,   339,   344,   345,   102,   113,   119,
     276,   277,   278,   337,   338,   339,   344,   349,   115,   349,
      19,   266,   266,   134,   170,   161,    19,    71,   202,    80,
     119,    81,    84,   120,   262,   263,   264,   310,   323,   325,
     335,   337,   338,    88,   273,   101,   104,    87,   133,   113,
     113,   113,   113,   113,   156,    78,   158,   159,   160,   151,
     151,     4,   167,    23,    80,   236,   236,   112,   220,   253,
     254,   255,   334,    28,   109,   222,   223,   225,   227,    79,
      86,    93,   113,   124,   125,   128,   129,   222,   240,   319,
     320,   349,   349,    27,   206,    91,    86,   120,   234,   315,
     319,   328,    88,   233,   236,   227,   222,   239,   240,    20,
      46,   268,   292,   296,   297,   298,   296,   118,   271,    77,
      77,    77,    77,   218,   223,   237,   211,   260,   261,   269,
     211,    15,   190,   227,   227,    80,   119,    81,    41,    90,
     134,   331,    77,   133,   345,    80,   119,   217,    86,   293,
     333,   342,   323,    78,    84,   119,   109,   119,   269,   332,
     334,   101,   113,   113,   119,   113,   119,   113,   113,   113,
     119,   115,   237,   331,   331,   120,   172,   309,   321,   322,
     338,   345,   202,   133,   200,   219,   223,   224,   330,   268,
     282,   283,   298,   333,    27,   214,   264,   270,   236,   332,
      78,   299,   300,   301,   331,   332,   334,   273,   113,   113,
     119,   105,   347,   348,    12,   112,   168,   169,   104,   106,
     284,   218,   338,    80,   105,   119,   241,   109,    80,    91,
     113,   113,   119,   113,   113,   115,   104,   106,   205,   227,
     223,   317,   329,   229,   236,    80,   197,   113,   119,   213,
     295,    85,   105,   118,   347,    25,    27,   210,   105,   118,
     347,   268,    81,    80,    81,   198,   223,   244,   112,   313,
     222,   133,   134,   109,    77,    77,   113,   108,   112,   114,
     311,   312,   305,    77,   268,   120,   120,   268,   281,   298,
     268,   277,   277,   332,   277,   277,    77,    80,    80,   334,
     343,   119,    28,   201,   225,   227,    77,   133,    80,    81,
     196,   248,   214,    81,   119,   120,   213,   105,   119,    81,
     101,   160,   112,   154,   105,   118,   268,   285,   286,   287,
     291,   285,   347,   113,   223,   255,   103,   104,   112,   242,
     243,   327,   244,   223,   222,     7,    26,   191,   203,   204,
     261,   204,   120,   120,   244,   222,   268,   297,   268,   104,
     106,   209,   261,   223,   244,   242,    84,   184,   330,   109,
     113,   115,   119,    78,   218,   221,   221,   120,   120,   321,
     241,   196,   248,    91,    77,   244,    28,   249,   250,   251,
      27,   245,     9,   256,   257,   258,   268,   298,   300,   277,
     302,   332,   154,   113,   285,   105,   118,    84,    86,   288,
     289,   290,   347,   223,   327,   327,   113,    37,   192,    19,
      37,   190,   227,   105,   118,   347,   271,     7,    26,   194,
     195,   207,   208,   261,   208,   185,   329,    27,   187,    80,
     298,   268,    77,   119,    77,    91,   103,   245,   256,   227,
     241,    84,   226,   231,   235,   236,   252,   104,   106,   256,
      22,    51,    52,   112,   182,   259,   316,   317,   258,    81,
     101,   113,   287,   282,   268,   214,   290,    80,   105,    80,
     227,   190,   227,    80,    81,   199,   203,    11,    19,   193,
     193,   105,   118,   347,    86,   104,   106,   188,   219,   218,
     227,   225,   229,   256,   103,   250,    91,   120,   235,   309,
     246,   247,   271,   246,   113,   227,   228,   238,   259,    53,
     183,   277,   332,    86,   214,   244,   244,   197,    81,   199,
     244,   112,   243,   327,   268,   190,   202,   207,   186,   329,
      78,   189,   190,    78,   189,    91,   231,   252,   231,   105,
     118,   306,   347,   119,   113,   227,   268,   105,   113,   327,
      84,   201,   329,   105,   105,   118,   347,   347,   227,   247,
      80,   238,   185,   196,   248,   190,   218,   245,   256,   256
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
     177,   177,   177,   178,   179,   179,   180,   180,   180,   180,
     181,   181,   181,   181,   181,   182,   182,   182,   183,   184,
     184,   185,   186,   186,   187,   187,   188,   188,   188,   188,
     189,   189,   189,   189,   190,   191,   191,   191,   191,   191,
     192,   192,   193,   193,   194,   194,   194,   195,   195,   196,
     196,   197,   197,   198,   198,   198,   199,   199,   199,   200,
     200,   201,   201,   201,   201,   202,   202,   202,   203,   203,
     204,   204,   204,   204,   205,   205,   206,   206,   207,   207,
     208,   208,   208,   208,   209,   209,   210,   210,   211,   211,
     211,   211,   212,   212,   213,   214,   214,   215,   215,   216,
     216,   216,   217,   217,   218,   219,   220,   220,   221,   221,
     222,   222,   223,   223,   223,   224,   225,   226,   227,   227,
     228,   229,   230,   230,   231,   231,   232,   232,   232,   233,
     234,   234,   235,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   237,   238,   238,   239,   239,   240,   240,
     241,   241,   242,   242,   242,   243,   243,   244,   245,   245,
     245,   246,   246,   247,   248,   249,   249,   250,   250,   251,
     251,   252,   252,   253,   253,   254,   254,   255,   256,   256,
     257,   257,   258,   258,   258,   259,   259,   259,   260,   260,
     261,   262,   262,   263,   263,   264,   265,   265,   265,   265,
     265,   265,   265,   265,   265,   266,   266,   267,   267,   268,
     268,   269,   269,   270,   270,   271,   271,   272,   272,   272,
     272,   273,   273,   273,   273,   273,   273,   273,   273,   273,
     273,   274,   274,   274,   275,   275,   275,   275,   275,   275,
     275,   275,   276,   276,   277,   277,   277,   278,   278,   279,
     279,   279,   279,   279,   279,   279,   280,   280,   281,   281,
     282,   283,   283,   284,   284,   284,   284,   285,   285,   286,
     286,   286,   287,   288,   288,   289,   289,   290,   291,   292,
     293,   294,   294,   295,   295,   296,   296,   296,   296,   297,
     297,   298,   298,   298,   299,   299,   300,   300,   300,   301,
     301,   301,   301,   302,   302,   303,   303,   304,   304,   305,
     305,   305,   306,   306,   307,   307,   307,   307,   308,   308,
     309,   309,   310,   310,   311,   311,   311,   312,   312,   312,
     312,   312,   313,   313,   314,   314,   314,   314,   315,   315,
     316,   317,   317,   318,   319,   319,   319,   320,   320,   320,
     320,   321,   321,   322,   322,   323,   323,   324,   324,   325,
     325,   326,   326,   327,   328,   329,   329,   329,   329,   329,
     330,   330,   331,   331,   331,   332,   333,   333,   334,   334,
     334,   334,   334,   334,   334,   334,   335,   335,   336,   336,
     337,   338,   338,   339,   339,   340,   340,   340,   340,   340,
     340,   340,   340,   340,   340,   340,   340,   340,   340,   340,
     340,   340,   340,   341,   341,   342,   342,   343,   344,   344,
     345,   345,   346,   346,   346,   346,   346,   347,   347,   348,
     348,   349,   349
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
       6,     6,     4,     4,     3,     1,     4,     3,     6,     7,
       2,     2,     2,     2,     0,     1,     1,     1,     2,     0,
       2,     3,     2,     1,     0,     2,     3,     3,     3,     3,
       3,     2,     1,     0,     3,     4,     3,     4,     2,     3,
       0,     1,     0,     1,     3,     6,     7,     1,     1,     0,
       2,     0,     2,     0,     2,     2,     0,     2,     4,     3,
       1,     6,     4,     3,     1,     4,     3,     0,     1,     1,
       3,     2,     1,     0,     3,     3,     2,     0,     1,     1,
       3,     2,     1,     0,     3,     3,     2,     0,     3,     2,
       1,     0,     3,     3,     1,     2,     0,     1,     3,     3,
       1,     0,     0,     2,     1,     1,     3,     1,     1,     3,
       1,     3,     4,     3,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     3,     1,     2,     1,     2,     3,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     3,     2,
       5,     3,     3,     1,     1,     3,     1,     0,     1,     3,
       2,     0,     1,     3,     5,     1,     5,     1,     4,     4,
       0,     3,     1,     4,     2,     3,     1,     4,     2,     3,
       0,     1,     3,     0,     1,     3,     1,     3,     0,     1,
       2,     1,     2,     3,     3,     1,     2,     3,     1,     2,
       1,     3,     2,     2,     1,     4,     3,     3,     4,     4,
       3,     4,     6,     6,     4,     0,     1,     3,     4,     3,
       1,     1,     3,     2,     1,     1,     0,     2,     3,     2,
       1,     3,     2,     2,     4,     4,     8,     4,     2,     2,
       1,     4,     3,     1,     1,     1,     1,     3,     3,     3,
       3,     1,     3,     2,     1,     2,     2,     3,     3,     1,
       1,     2,     4,     3,     5,     3,     3,     3,     3,     1,
       1,     3,     1,     3,     3,     2,     2,     1,     2,     3,
       2,     1,     2,     3,     2,     2,     1,     4,     1,     1,
       1,     2,     1,     3,     3,     3,     2,     1,     0,     1,
       2,     3,     1,     2,     1,     0,     3,     1,     1,     3,
       1,     5,     3,     3,     1,     1,     1,     1,     3,     1,
       3,     1,     3,     1,     2,     3,     2,     3,     1,     2,
       1,     3,     1,     3,     1,     2,     2,     1,     3,     3,
       3,     2,     1,     3,     1,     3,     3,     3,     1,     3,
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
  "tycl_hdr", "datafam_inst_hdr", "capi_ctype", "decl_cls", "decls_cls",
  "decllist_cls", "where_cls", "decl_inst", "decls_inst", "decllist_inst",
  "where_inst", "decls", "decllist", "binds", "wherebinds", "strings",
  "stringlist", "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars",
  "sigtypes1", "ktype", "ctype", "ctypedoc", "context", "context_no_ops",
  "type", "typedoc", "btype", "infixtype", "btype_no_ops", "ftype",
  "tyarg", "tyop", "atype_docs", "atype", "inst_type", "deriv_types",
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
       0,   512,   512,   529,   530,   532,   536,   537,   538,   540,
     541,   543,   544,   547,   549,   550,   551,   559,   560,   562,
     564,   565,   567,   568,   570,   571,   572,   574,   575,   577,
     578,   580,   581,   585,   586,   588,   589,   591,   593,   594,
     596,   609,   610,   612,   613,   615,   616,   620,   621,   626,
     627,   629,   630,   631,   633,   634,   638,   640,   641,   643,
     644,   645,   646,   649,   650,   656,   658,   661,   664,   665,
     667,   668,   669,   671,   673,   674,   677,   678,   679,   685,
     692,   693,   694,   695,   696,   698,   699,   700,   702,   713,
     714,   716,   718,   719,   723,   724,   726,   727,   728,   729,
     731,   732,   733,   734,   736,   739,   741,   743,   745,   746,
     748,   748,   750,   750,   754,   756,   763,   770,   771,   775,
     776,   778,   779,   781,   782,   783,   785,   786,   787,   790,
     791,   794,   795,   796,   797,   799,   800,   801,   843,   844,
     846,   847,   848,   849,   851,   852,   854,   855,   857,   858,
     860,   861,   862,   863,   865,   866,   868,   869,   872,   873,
     874,   875,   877,   878,   880,   882,   883,   891,   892,   894,
     895,   896,   909,   910,   919,   921,   923,   924,   926,   927,
     936,   937,   939,   940,   942,   944,   953,   955,   957,   958,
     960,   963,   965,   966,   968,   969,   971,   973,   974,   976,
     978,   979,   986,   993,   994,   995,   996,   997,   998,   999,
    1000,  1006,  1007,  1010,  1012,  1013,  1015,  1016,  1018,  1019,
    1026,  1027,  1029,  1030,  1031,  1034,  1035,  1053,  1059,  1060,
    1061,  1063,  1064,  1066,  1068,  1070,  1071,  1073,  1074,  1076,
    1077,  1079,  1080,  1086,  1087,  1089,  1090,  1092,  1094,  1095,
    1097,  1098,  1100,  1101,  1102,  1104,  1105,  1106,  1111,  1113,
    1115,  1119,  1120,  1122,  1123,  1127,  1137,  1138,  1140,  1141,
    1142,  1143,  1144,  1145,  1146,  1149,  1150,  1152,  1153,  1158,
    1159,  1163,  1164,  1166,  1167,  1169,  1170,  1175,  1176,  1177,
    1178,  1181,  1182,  1183,  1184,  1185,  1187,  1189,  1190,  1191,
    1193,  1196,  1197,  1198,  1201,  1202,  1203,  1204,  1205,  1206,
    1211,  1212,  1215,  1216,  1221,  1222,  1223,  1228,  1229,  1247,
    1248,  1249,  1250,  1251,  1252,  1253,  1255,  1256,  1269,  1271,
    1281,  1283,  1284,  1287,  1288,  1289,  1290,  1292,  1293,  1295,
    1296,  1297,  1299,  1301,  1302,  1304,  1305,  1314,  1316,  1318,
    1320,  1322,  1323,  1326,  1327,  1329,  1330,  1331,  1332,  1337,
    1338,  1340,  1341,  1342,  1347,  1348,  1350,  1351,  1352,  1354,
    1355,  1356,  1357,  1360,  1361,  1393,  1394,  1396,  1397,  1399,
    1400,  1401,  1403,  1404,  1406,  1407,  1408,  1409,  1411,  1412,
    1414,  1415,  1417,  1418,  1421,  1422,  1423,  1425,  1426,  1427,
    1428,  1429,  1431,  1432,  1434,  1435,  1436,  1437,  1440,  1441,
    1443,  1445,  1446,  1450,  1452,  1453,  1454,  1456,  1457,  1458,
    1459,  1464,  1465,  1467,  1468,  1470,  1471,  1474,  1475,  1480,
    1481,  1483,  1484,  1488,  1490,  1492,  1493,  1494,  1495,  1496,
    1499,  1500,  1502,  1503,  1504,  1506,  1508,  1509,  1511,  1512,
    1513,  1514,  1515,  1516,  1517,  1518,  1520,  1521,  1523,  1524,
    1526,  1528,  1529,  1531,  1532,  1534,  1535,  1536,  1537,  1538,
    1539,  1540,  1541,  1542,  1543,  1544,  1545,  1546,  1547,  1548,
    1549,  1550,  1551,  1553,  1554,  1558,  1559,  1561,  1563,  1564,
    1566,  1567,  1571,  1572,  1573,  1574,  1575,  1580,  1583,  1587,
    1588,  1590,  1591
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
#line 7253 "parser.cc"

#line 1600 "parser.y"


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
    return {name, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs)};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k, const Hs::GADTConstructorsDecl& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype)
    {
        if (constrs.size() != 1 or constrs[0].con_names.size() != 1)
            throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    }

    return {name, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs)};
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

