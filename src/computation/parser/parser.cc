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

  case 20: // exportlist: %empty
#line 563 "parser.y"
                                      {}
#line 2596 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 564 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2602 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 565 "parser.y"
                                      {}
#line 2608 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 567 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2614 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 568 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2620 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 570 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2626 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 571 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2632 "parser.cc"
    break;

  case 27: // export_subspec: %empty
#line 573 "parser.y"
                                      {}
#line 2638 "parser.cc"
    break;

  case 28: // export_subspec: "(" qcnames ")"
#line 574 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2644 "parser.cc"
    break;

  case 29: // export_subspec: "(" ".." ")"
#line 575 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2650 "parser.cc"
    break;

  case 30: // qcnames: %empty
#line 577 "parser.y"
                   {}
#line 2656 "parser.cc"
    break;

  case 31: // qcnames: qcnames1
#line 578 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2662 "parser.cc"
    break;

  case 32: // qcnames1: qcnames1 "," qcname
#line 580 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2668 "parser.cc"
    break;

  case 33: // qcnames1: qcname
#line 581 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2674 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 583 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2680 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 584 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2686 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 594 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2692 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 596 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2698 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 597 "parser.y"
                         { }
#line 2704 "parser.cc"
    break;

  case 43: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 599 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2712 "parser.cc"
    break;

  case 44: // optqualified: "qualified"
#line 612 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2718 "parser.cc"
    break;

  case 45: // optqualified: %empty
#line 613 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2724 "parser.cc"
    break;

  case 46: // maybeas: "as" modid
#line 615 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2730 "parser.cc"
    break;

  case 47: // maybeas: %empty
#line 616 "parser.y"
                               { }
#line 2736 "parser.cc"
    break;

  case 48: // maybeimpspec: impspec
#line 618 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2742 "parser.cc"
    break;

  case 49: // maybeimpspec: %empty
#line 619 "parser.y"
                               { }
#line 2748 "parser.cc"
    break;

  case 50: // impspec: "(" exportlist ")"
#line 623 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2754 "parser.cc"
    break;

  case 51: // impspec: "hiding" "(" exportlist ")"
#line 624 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2760 "parser.cc"
    break;

  case 52: // prec: %empty
#line 629 "parser.y"
                   { }
#line 2766 "parser.cc"
    break;

  case 53: // prec: "INTEGER"
#line 630 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2772 "parser.cc"
    break;

  case 54: // infix: "infix"
#line 632 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2778 "parser.cc"
    break;

  case 55: // infix: "infixl"
#line 633 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2784 "parser.cc"
    break;

  case 56: // infix: "infixr"
#line 634 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2790 "parser.cc"
    break;

  case 57: // ops: ops "," op
#line 636 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2796 "parser.cc"
    break;

  case 58: // ops: op
#line 637 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2802 "parser.cc"
    break;

  case 59: // topdecls: topdecls_semi topdecl
#line 641 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2808 "parser.cc"
    break;

  case 60: // topdecls_semi: topdecls_semi topdecl semis1
#line 643 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2814 "parser.cc"
    break;

  case 61: // topdecls_semi: %empty
#line 644 "parser.y"
                                            { }
#line 2820 "parser.cc"
    break;

  case 62: // topdecl: cl_decl
#line 646 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2826 "parser.cc"
    break;

  case 63: // topdecl: ty_decl
#line 647 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2832 "parser.cc"
    break;

  case 64: // topdecl: standalone_kind_sig
#line 648 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2838 "parser.cc"
    break;

  case 65: // topdecl: inst_decl
#line 649 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2844 "parser.cc"
    break;

  case 66: // topdecl: "default" "(" comma_types0 ")"
#line 652 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2850 "parser.cc"
    break;

  case 67: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 653 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2856 "parser.cc"
    break;

  case 68: // topdecl: decl_no_th
#line 659 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2862 "parser.cc"
    break;

  case 69: // topdecl: infixexp
#line 661 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2868 "parser.cc"
    break;

  case 70: // cl_decl: "class" tycl_hdr where_cls
#line 664 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2874 "parser.cc"
    break;

  case 71: // ty_decl: "type" type "=" ktype
#line 667 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2880 "parser.cc"
    break;

  case 72: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 668 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2886 "parser.cc"
    break;

  case 73: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 670 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 2892 "parser.cc"
    break;

  case 74: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 671 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2898 "parser.cc"
    break;

  case 75: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 672 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 2904 "parser.cc"
    break;

  case 76: // standalone_kind_sig: "type" sks_vars "::" kind
#line 674 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 2910 "parser.cc"
    break;

  case 77: // sks_vars: sks_vars "," oqtycon
#line 676 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2916 "parser.cc"
    break;

  case 78: // sks_vars: oqtycon
#line 677 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2922 "parser.cc"
    break;

  case 79: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 680 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2928 "parser.cc"
    break;

  case 80: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 681 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 2934 "parser.cc"
    break;

  case 81: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 683 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 2944 "parser.cc"
    break;

  case 82: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 689 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 2954 "parser.cc"
    break;

  case 83: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 695 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 2960 "parser.cc"
    break;

  case 84: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 696 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 2966 "parser.cc"
    break;

  case 85: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 697 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 2972 "parser.cc"
    break;

  case 86: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 698 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 2978 "parser.cc"
    break;

  case 87: // overlap_pragma: %empty
#line 699 "parser.y"
                                               {}
#line 2984 "parser.cc"
    break;

  case 97: // where_type_family: %empty
#line 726 "parser.y"
                                                           {}
#line 2990 "parser.cc"
    break;

  case 98: // where_type_family: "where" ty_fam_inst_eqn_list
#line 727 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2996 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 729 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3002 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 730 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3008 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 731 "parser.y"
                                                           {}
#line 3014 "parser.cc"
    break;

  case 102: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 732 "parser.y"
                                                           {}
#line 3020 "parser.cc"
    break;

  case 103: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 734 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3026 "parser.cc"
    break;

  case 104: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 735 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3032 "parser.cc"
    break;

  case 105: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 736 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3038 "parser.cc"
    break;

  case 106: // ty_fam_inst_eqns: %empty
#line 737 "parser.y"
                                                           {}
#line 3044 "parser.cc"
    break;

  case 107: // ty_fam_inst_eqn: type "=" ctype
#line 739 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3050 "parser.cc"
    break;

  case 108: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 742 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3056 "parser.cc"
    break;

  case 109: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 744 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3062 "parser.cc"
    break;

  case 110: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 746 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3068 "parser.cc"
    break;

  case 111: // at_decl_cls: "type" ty_fam_inst_eqn
#line 748 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3074 "parser.cc"
    break;

  case 112: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 749 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3080 "parser.cc"
    break;

  case 117: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 757 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3086 "parser.cc"
    break;

  case 118: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 760 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3096 "parser.cc"
    break;

  case 119: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 767 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3106 "parser.cc"
    break;

  case 120: // data_or_newtype: "data"
#line 773 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3112 "parser.cc"
    break;

  case 121: // data_or_newtype: "newtype"
#line 774 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3118 "parser.cc"
    break;

  case 122: // opt_kind_sig: %empty
#line 778 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3124 "parser.cc"
    break;

  case 123: // opt_kind_sig: "::" kind
#line 779 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3130 "parser.cc"
    break;

  case 124: // opt_datafam_kind_sig: %empty
#line 781 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3136 "parser.cc"
    break;

  case 125: // opt_datafam_kind_sig: "::" kind
#line 782 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3142 "parser.cc"
    break;

  case 126: // opt_tyfam_kind_sig: %empty
#line 784 "parser.y"
                                      {}
#line 3148 "parser.cc"
    break;

  case 127: // opt_tyfam_kind_sig: "::" kind
#line 785 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3154 "parser.cc"
    break;

  case 128: // opt_tyfam_kind_sig: "=" tv_bndr
#line 786 "parser.y"
                                      {}
#line 3160 "parser.cc"
    break;

  case 129: // opt_at_kind_inj_sig: %empty
#line 788 "parser.y"
                                      {}
#line 3166 "parser.cc"
    break;

  case 130: // opt_at_kind_inj_sig: "::" kind
#line 789 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3172 "parser.cc"
    break;

  case 131: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 790 "parser.y"
                                                                  {}
#line 3178 "parser.cc"
    break;

  case 132: // tycl_hdr: context "=>" type
#line 793 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3184 "parser.cc"
    break;

  case 133: // tycl_hdr: type
#line 794 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3190 "parser.cc"
    break;

  case 134: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 797 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3196 "parser.cc"
    break;

  case 135: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 798 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3202 "parser.cc"
    break;

  case 136: // datafam_inst_hdr: context "=>" type
#line 799 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3208 "parser.cc"
    break;

  case 137: // datafam_inst_hdr: type
#line 800 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3214 "parser.cc"
    break;

  case 141: // decl_cls: at_decl_cls
#line 846 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3220 "parser.cc"
    break;

  case 142: // decl_cls: decl
#line 847 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3226 "parser.cc"
    break;

  case 143: // decls_cls: decls_cls ";" decl_cls
#line 849 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3232 "parser.cc"
    break;

  case 144: // decls_cls: decls_cls ";"
#line 850 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3238 "parser.cc"
    break;

  case 145: // decls_cls: decl_cls
#line 851 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3244 "parser.cc"
    break;

  case 146: // decls_cls: %empty
#line 852 "parser.y"
                                           {}
#line 3250 "parser.cc"
    break;

  case 147: // decllist_cls: "{" decls_cls "}"
#line 854 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3256 "parser.cc"
    break;

  case 148: // decllist_cls: "vocurly" decls_cls close
#line 855 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3262 "parser.cc"
    break;

  case 149: // where_cls: "where" decllist_cls
#line 857 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3268 "parser.cc"
    break;

  case 150: // where_cls: %empty
#line 858 "parser.y"
                                           {}
#line 3274 "parser.cc"
    break;

  case 151: // decl_inst: at_decl_inst
#line 860 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3280 "parser.cc"
    break;

  case 152: // decl_inst: decl
#line 861 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3286 "parser.cc"
    break;

  case 153: // decls_inst: decls_inst ";" decl_inst
#line 863 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3292 "parser.cc"
    break;

  case 154: // decls_inst: decls_inst ";"
#line 864 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3298 "parser.cc"
    break;

  case 155: // decls_inst: decl_inst
#line 865 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3304 "parser.cc"
    break;

  case 156: // decls_inst: %empty
#line 866 "parser.y"
                                           {}
#line 3310 "parser.cc"
    break;

  case 157: // decllist_inst: "{" decls_inst "}"
#line 868 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3316 "parser.cc"
    break;

  case 158: // decllist_inst: "vocurly" decls_inst close
#line 869 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3322 "parser.cc"
    break;

  case 159: // where_inst: "where" decllist_inst
#line 871 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3328 "parser.cc"
    break;

  case 160: // where_inst: %empty
#line 872 "parser.y"
                                           {}
#line 3334 "parser.cc"
    break;

  case 161: // decls: decls ";" decl
#line 875 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3340 "parser.cc"
    break;

  case 162: // decls: decls ";"
#line 876 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3346 "parser.cc"
    break;

  case 163: // decls: decl
#line 877 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3352 "parser.cc"
    break;

  case 164: // decls: %empty
#line 878 "parser.y"
                        {}
#line 3358 "parser.cc"
    break;

  case 165: // decllist: "{" decls "}"
#line 880 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3364 "parser.cc"
    break;

  case 166: // decllist: "vocurly" decls close
#line 881 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3370 "parser.cc"
    break;

  case 167: // binds: decllist
#line 883 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3376 "parser.cc"
    break;

  case 168: // wherebinds: "where" binds
#line 885 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3382 "parser.cc"
    break;

  case 169: // wherebinds: %empty
#line 886 "parser.y"
                                 {}
#line 3388 "parser.cc"
    break;

  case 175: // opt_tyconsig: %empty
#line 912 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3394 "parser.cc"
    break;

  case 176: // opt_tyconsig: "::" gtycon
#line 913 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3400 "parser.cc"
    break;

  case 177: // sigtype: ctype
#line 922 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3406 "parser.cc"
    break;

  case 178: // sigtypedoc: ctypedoc
#line 924 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3412 "parser.cc"
    break;

  case 179: // sig_vars: sig_vars "," var
#line 926 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3418 "parser.cc"
    break;

  case 180: // sig_vars: var
#line 927 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3424 "parser.cc"
    break;

  case 181: // sigtypes1: sigtype
#line 929 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3430 "parser.cc"
    break;

  case 182: // sigtypes1: sigtypes1 "," sigtype
#line 930 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3436 "parser.cc"
    break;

  case 183: // ktype: ctype
#line 939 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3442 "parser.cc"
    break;

  case 184: // ktype: ctype "::" kind
#line 940 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3448 "parser.cc"
    break;

  case 185: // ctype: "forall" tv_bndrs "." ctype
#line 942 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3454 "parser.cc"
    break;

  case 186: // ctype: context "=>" ctype
#line 943 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3460 "parser.cc"
    break;

  case 187: // ctype: type
#line 945 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3466 "parser.cc"
    break;

  case 188: // ctypedoc: ctype
#line 947 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3472 "parser.cc"
    break;

  case 189: // context: btype
#line 956 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3478 "parser.cc"
    break;

  case 190: // context_no_ops: btype_no_ops
#line 958 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3484 "parser.cc"
    break;

  case 191: // type: btype
#line 960 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3490 "parser.cc"
    break;

  case 192: // type: btype "->" ctype
#line 961 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3496 "parser.cc"
    break;

  case 193: // typedoc: type
#line 963 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3502 "parser.cc"
    break;

  case 194: // btype: infixtype
#line 966 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3508 "parser.cc"
    break;

  case 195: // infixtype: ftype
#line 968 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3514 "parser.cc"
    break;

  case 196: // infixtype: btype tyop btype
#line 969 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3520 "parser.cc"
    break;

  case 197: // btype_no_ops: atype_docs
#line 971 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3526 "parser.cc"
    break;

  case 198: // btype_no_ops: btype_no_ops atype_docs
#line 972 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3532 "parser.cc"
    break;

  case 199: // ftype: atype
#line 974 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3538 "parser.cc"
    break;

  case 200: // ftype: ftype tyarg
#line 976 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3544 "parser.cc"
    break;

  case 201: // ftype: ftype "@" atype
#line 977 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3550 "parser.cc"
    break;

  case 202: // tyarg: atype
#line 979 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3556 "parser.cc"
    break;

  case 203: // tyop: qtyconop
#line 981 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3562 "parser.cc"
    break;

  case 204: // tyop: tyvarop
#line 982 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3568 "parser.cc"
    break;

  case 205: // atype_docs: atype
#line 989 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3574 "parser.cc"
    break;

  case 206: // atype: ntgtycon
#line 996 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3580 "parser.cc"
    break;

  case 207: // atype: tyvar
#line 997 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3586 "parser.cc"
    break;

  case 208: // atype: "*"
#line 998 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3592 "parser.cc"
    break;

  case 209: // atype: PREFIX_BANG atype
#line 999 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3598 "parser.cc"
    break;

  case 210: // atype: PREFIX_TILDE atype
#line 1000 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3604 "parser.cc"
    break;

  case 211: // atype: "{" fielddecls "}"
#line 1001 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3610 "parser.cc"
    break;

  case 212: // atype: "(" ")"
#line 1002 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3616 "parser.cc"
    break;

  case 213: // atype: "(" comma_types1 "," ktype ")"
#line 1003 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3622 "parser.cc"
    break;

  case 214: // atype: "[" ktype "]"
#line 1009 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3628 "parser.cc"
    break;

  case 215: // atype: "(" ktype ")"
#line 1010 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3634 "parser.cc"
    break;

  case 216: // inst_type: sigtype
#line 1013 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3640 "parser.cc"
    break;

  case 219: // comma_types0: comma_types1
#line 1018 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3646 "parser.cc"
    break;

  case 220: // comma_types0: %empty
#line 1019 "parser.y"
                                       { /* default construction OK */ }
#line 3652 "parser.cc"
    break;

  case 221: // comma_types1: ktype
#line 1021 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3658 "parser.cc"
    break;

  case 222: // comma_types1: comma_types1 "," ktype
#line 1022 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3664 "parser.cc"
    break;

  case 223: // tv_bndrs: tv_bndrs tv_bndr
#line 1029 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3670 "parser.cc"
    break;

  case 224: // tv_bndrs: %empty
#line 1030 "parser.y"
                               { /* default construction OK */}
#line 3676 "parser.cc"
    break;

  case 225: // tv_bndr: tv_bndr_no_braces
#line 1032 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3682 "parser.cc"
    break;

  case 226: // tv_bndr: "{" tyvar "}"
#line 1033 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3688 "parser.cc"
    break;

  case 227: // tv_bndr: "{" tyvar "::" kind "}"
#line 1034 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3694 "parser.cc"
    break;

  case 228: // tv_bndr_no_braces: tyvar
#line 1037 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3700 "parser.cc"
    break;

  case 229: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1038 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3706 "parser.cc"
    break;

  case 230: // kind: ctype
#line 1056 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3712 "parser.cc"
    break;

  case 231: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1062 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3718 "parser.cc"
    break;

  case 232: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1063 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3724 "parser.cc"
    break;

  case 233: // gadt_constrlist: %empty
#line 1064 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3730 "parser.cc"
    break;

  case 234: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1066 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3736 "parser.cc"
    break;

  case 235: // gadt_constrs: gadt_constr
#line 1067 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3742 "parser.cc"
    break;

  case 236: // gadt_constr: optSemi con_list "::" sigtype
#line 1069 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3748 "parser.cc"
    break;

  case 237: // constrs: "=" constrs1
#line 1071 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3754 "parser.cc"
    break;

  case 238: // constrs1: constrs1 "|" constr
#line 1073 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3760 "parser.cc"
    break;

  case 239: // constrs1: constr
#line 1074 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3766 "parser.cc"
    break;

  case 240: // constr: forall context_no_ops "=>" constr_stuff
#line 1076 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3772 "parser.cc"
    break;

  case 241: // constr: forall constr_stuff
#line 1077 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3778 "parser.cc"
    break;

  case 242: // forall: "forall" tv_bndrs "."
#line 1079 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3784 "parser.cc"
    break;

  case 243: // forall: %empty
#line 1080 "parser.y"
                                {}
#line 3790 "parser.cc"
    break;

  case 244: // constr_stuff: btype_no_ops
#line 1082 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3796 "parser.cc"
    break;

  case 245: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1083 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3806 "parser.cc"
    break;

  case 246: // fielddecls: %empty
#line 1089 "parser.y"
                                {}
#line 3812 "parser.cc"
    break;

  case 247: // fielddecls: fielddecls1
#line 1090 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3818 "parser.cc"
    break;

  case 248: // fielddecls1: fielddecls1 "," fielddecl
#line 1092 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3824 "parser.cc"
    break;

  case 249: // fielddecls1: fielddecl
#line 1093 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3830 "parser.cc"
    break;

  case 250: // fielddecl: sig_vars "::" ctype
#line 1095 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3836 "parser.cc"
    break;

  case 261: // decl_no_th: sigdecl
#line 1114 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3842 "parser.cc"
    break;

  case 262: // decl_no_th: infixexp rhs
#line 1116 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3848 "parser.cc"
    break;

  case 263: // decl: decl_no_th
#line 1118 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3854 "parser.cc"
    break;

  case 264: // rhs: "=" exp wherebinds
#line 1122 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3860 "parser.cc"
    break;

  case 265: // rhs: gdrhs wherebinds
#line 1123 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3866 "parser.cc"
    break;

  case 266: // gdrhs: gdrhs gdrh
#line 1125 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3872 "parser.cc"
    break;

  case 267: // gdrhs: gdrh
#line 1126 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3878 "parser.cc"
    break;

  case 268: // gdrh: "|" guardquals "=" exp
#line 1130 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3884 "parser.cc"
    break;

  case 269: // sigdecl: sig_vars "::" sigtypedoc
#line 1140 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3890 "parser.cc"
    break;

  case 270: // sigdecl: infix prec ops
#line 1141 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3896 "parser.cc"
    break;

  case 271: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1143 "parser.y"
                                                    {}
#line 3902 "parser.cc"
    break;

  case 272: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1144 "parser.y"
                                            {}
#line 3908 "parser.cc"
    break;

  case 273: // sigdecl: "{-# SCC" qvar "#-}"
#line 1145 "parser.y"
                              {}
#line 3914 "parser.cc"
    break;

  case 274: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1146 "parser.y"
                                     {}
#line 3920 "parser.cc"
    break;

  case 275: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1147 "parser.y"
                                                               {}
#line 3926 "parser.cc"
    break;

  case 276: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1148 "parser.y"
                                                                      {}
#line 3932 "parser.cc"
    break;

  case 277: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1149 "parser.y"
                                                     {}
#line 3938 "parser.cc"
    break;

  case 282: // exp: infixexp "::" sigtype
#line 1161 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 3944 "parser.cc"
    break;

  case 283: // exp: infixexp
#line 1162 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3950 "parser.cc"
    break;

  case 284: // infixexp: exp10
#line 1166 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3956 "parser.cc"
    break;

  case 285: // infixexp: infixexp qop exp10
#line 1167 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3962 "parser.cc"
    break;

  case 286: // exp10: PREFIX_MINUS fexp
#line 1169 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3968 "parser.cc"
    break;

  case 287: // exp10: fexp
#line 1170 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3974 "parser.cc"
    break;

  case 290: // fexp: fexp aexp
#line 1178 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3980 "parser.cc"
    break;

  case 291: // fexp: fexp "@" atype
#line 1179 "parser.y"
                                 {}
#line 3986 "parser.cc"
    break;

  case 292: // fexp: "static" aexp
#line 1180 "parser.y"
                                 {}
#line 3992 "parser.cc"
    break;

  case 293: // fexp: aexp
#line 1181 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3998 "parser.cc"
    break;

  case 294: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1184 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4004 "parser.cc"
    break;

  case 295: // aexp: PREFIX_TILDE aexp
#line 1185 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4010 "parser.cc"
    break;

  case 296: // aexp: PREFIX_BANG aexp
#line 1186 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4016 "parser.cc"
    break;

  case 297: // aexp: "\\" apats1 "->" exp
#line 1187 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4022 "parser.cc"
    break;

  case 298: // aexp: "let" binds "in" exp
#line 1188 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4028 "parser.cc"
    break;

  case 299: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1190 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4034 "parser.cc"
    break;

  case 300: // aexp: "case" exp "of" altslist
#line 1192 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4040 "parser.cc"
    break;

  case 301: // aexp: "do" stmtlist
#line 1193 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4046 "parser.cc"
    break;

  case 302: // aexp: "mdo" stmtlist
#line 1194 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4052 "parser.cc"
    break;

  case 303: // aexp: aexp1
#line 1196 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4058 "parser.cc"
    break;

  case 304: // aexp1: aexp1 "{" fbinds "}"
#line 1199 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4064 "parser.cc"
    break;

  case 305: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1200 "parser.y"
                                     { }
#line 4070 "parser.cc"
    break;

  case 306: // aexp1: aexp2
#line 1201 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4076 "parser.cc"
    break;

  case 307: // aexp2: qvar
#line 1204 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4082 "parser.cc"
    break;

  case 308: // aexp2: qcon
#line 1205 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4088 "parser.cc"
    break;

  case 309: // aexp2: literal
#line 1206 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4094 "parser.cc"
    break;

  case 310: // aexp2: "(" texp ")"
#line 1207 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4100 "parser.cc"
    break;

  case 311: // aexp2: "(" tup_exprs ")"
#line 1208 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4106 "parser.cc"
    break;

  case 312: // aexp2: "(" projection ")"
#line 1209 "parser.y"
                              {}
#line 4112 "parser.cc"
    break;

  case 313: // aexp2: "[" list "]"
#line 1214 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4118 "parser.cc"
    break;

  case 314: // aexp2: "_"
#line 1215 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4124 "parser.cc"
    break;

  case 317: // texp: exp
#line 1224 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4130 "parser.cc"
    break;

  case 318: // texp: infixexp qop
#line 1225 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4136 "parser.cc"
    break;

  case 319: // texp: qopm infixexp
#line 1226 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4142 "parser.cc"
    break;

  case 320: // tup_exprs: tup_exprs "," texp
#line 1231 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4148 "parser.cc"
    break;

  case 321: // tup_exprs: texp "," texp
#line 1232 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4154 "parser.cc"
    break;

  case 322: // list: texp
#line 1250 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4160 "parser.cc"
    break;

  case 323: // list: lexps
#line 1251 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4166 "parser.cc"
    break;

  case 324: // list: texp ".."
#line 1252 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4172 "parser.cc"
    break;

  case 325: // list: texp "," exp ".."
#line 1253 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4178 "parser.cc"
    break;

  case 326: // list: texp ".." exp
#line 1254 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4184 "parser.cc"
    break;

  case 327: // list: texp "," exp ".." exp
#line 1255 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4190 "parser.cc"
    break;

  case 328: // list: texp "|" squals
#line 1256 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4196 "parser.cc"
    break;

  case 329: // lexps: lexps "," texp
#line 1258 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4202 "parser.cc"
    break;

  case 330: // lexps: texp "," texp
#line 1259 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4208 "parser.cc"
    break;

  case 331: // squals: squals "," qual
#line 1272 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4214 "parser.cc"
    break;

  case 332: // squals: qual
#line 1274 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4220 "parser.cc"
    break;

  case 333: // guardquals: guardquals1
#line 1284 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4226 "parser.cc"
    break;

  case 334: // guardquals1: guardquals1 "," qual
#line 1286 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4232 "parser.cc"
    break;

  case 335: // guardquals1: qual
#line 1287 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4238 "parser.cc"
    break;

  case 336: // altslist: "{" alts "}"
#line 1290 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4244 "parser.cc"
    break;

  case 337: // altslist: "vocurly" alts close
#line 1291 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4250 "parser.cc"
    break;

  case 338: // altslist: "{" "}"
#line 1292 "parser.y"
                                 {}
#line 4256 "parser.cc"
    break;

  case 339: // altslist: "vocurly" close
#line 1293 "parser.y"
                                 {}
#line 4262 "parser.cc"
    break;

  case 340: // alts: alts1
#line 1295 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4268 "parser.cc"
    break;

  case 341: // alts: ";" alts
#line 1296 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4274 "parser.cc"
    break;

  case 342: // alts1: alts1 ";" alt
#line 1298 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4280 "parser.cc"
    break;

  case 343: // alts1: alts1 ";"
#line 1299 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4286 "parser.cc"
    break;

  case 344: // alts1: alt
#line 1300 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4292 "parser.cc"
    break;

  case 345: // alt: pat alt_rhs
#line 1302 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4298 "parser.cc"
    break;

  case 346: // alt_rhs: "->" exp wherebinds
#line 1304 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4304 "parser.cc"
    break;

  case 347: // alt_rhs: gdpats wherebinds
#line 1305 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4310 "parser.cc"
    break;

  case 348: // gdpats: gdpats gdpat
#line 1307 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4316 "parser.cc"
    break;

  case 349: // gdpats: gdpat
#line 1308 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4322 "parser.cc"
    break;

  case 350: // gdpat: "|" guardquals "->" exp
#line 1317 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4328 "parser.cc"
    break;

  case 351: // pat: exp
#line 1319 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4334 "parser.cc"
    break;

  case 352: // bindpat: exp
#line 1321 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4340 "parser.cc"
    break;

  case 353: // apat: aexp
#line 1323 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4346 "parser.cc"
    break;

  case 354: // apats1: apats1 apat
#line 1325 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4352 "parser.cc"
    break;

  case 355: // apats1: apat
#line 1326 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4358 "parser.cc"
    break;

  case 356: // stmtlist: "{" stmts "}"
#line 1329 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4364 "parser.cc"
    break;

  case 357: // stmtlist: "vocurly" stmts close
#line 1330 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4370 "parser.cc"
    break;

  case 358: // stmts: stmts ";" stmt
#line 1332 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4376 "parser.cc"
    break;

  case 359: // stmts: stmts ";"
#line 1333 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4382 "parser.cc"
    break;

  case 360: // stmts: stmt
#line 1334 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4388 "parser.cc"
    break;

  case 361: // stmts: %empty
#line 1335 "parser.y"
                       {}
#line 4394 "parser.cc"
    break;

  case 362: // stmt: qual
#line 1340 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4400 "parser.cc"
    break;

  case 363: // stmt: "rec" stmtlist
#line 1341 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4406 "parser.cc"
    break;

  case 364: // qual: bindpat "<-" exp
#line 1343 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4412 "parser.cc"
    break;

  case 365: // qual: exp
#line 1344 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4418 "parser.cc"
    break;

  case 366: // qual: "let" binds
#line 1345 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4424 "parser.cc"
    break;

  case 367: // fbinds: fbinds1
#line 1350 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4430 "parser.cc"
    break;

  case 368: // fbinds: %empty
#line 1351 "parser.y"
                        {}
#line 4436 "parser.cc"
    break;

  case 369: // fbinds1: fbind "," fbinds1
#line 1353 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4442 "parser.cc"
    break;

  case 370: // fbinds1: fbind
#line 1354 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4448 "parser.cc"
    break;

  case 371: // fbinds1: ".."
#line 1355 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4454 "parser.cc"
    break;

  case 372: // fbind: qvar "=" texp
#line 1357 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4460 "parser.cc"
    break;

  case 373: // fbind: qvar
#line 1358 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4466 "parser.cc"
    break;

  case 374: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1359 "parser.y"
                                                      {}
#line 4472 "parser.cc"
    break;

  case 375: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1360 "parser.y"
                                                      {}
#line 4478 "parser.cc"
    break;

  case 378: // qcon: gen_qcon
#line 1396 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4484 "parser.cc"
    break;

  case 379: // qcon: sysdcon
#line 1397 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4490 "parser.cc"
    break;

  case 380: // gen_qcon: qconid
#line 1399 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4496 "parser.cc"
    break;

  case 381: // gen_qcon: "(" qconsym ")"
#line 1400 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4502 "parser.cc"
    break;

  case 382: // con: conid
#line 1402 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4508 "parser.cc"
    break;

  case 383: // con: "(" consym ")"
#line 1403 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4514 "parser.cc"
    break;

  case 384: // con: sysdcon
#line 1404 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4520 "parser.cc"
    break;

  case 385: // con_list: con_list "," con
#line 1406 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4526 "parser.cc"
    break;

  case 386: // con_list: con
#line 1407 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4532 "parser.cc"
    break;

  case 387: // sysdcon_no_list: "(" ")"
#line 1409 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4538 "parser.cc"
    break;

  case 388: // sysdcon_no_list: "(" commas ")"
#line 1410 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4544 "parser.cc"
    break;

  case 389: // sysdcon_no_list: "(#" "#)"
#line 1411 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4550 "parser.cc"
    break;

  case 390: // sysdcon_no_list: "(#" commas "#)"
#line 1412 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4556 "parser.cc"
    break;

  case 391: // sysdcon: sysdcon_no_list
#line 1414 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4562 "parser.cc"
    break;

  case 392: // sysdcon: "[" "]"
#line 1415 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4568 "parser.cc"
    break;

  case 393: // conop: consym
#line 1417 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4574 "parser.cc"
    break;

  case 394: // conop: "`" conid "`"
#line 1418 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4580 "parser.cc"
    break;

  case 395: // qconop: qconsym
#line 1420 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4586 "parser.cc"
    break;

  case 396: // qconop: "`" qconid "`"
#line 1421 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4592 "parser.cc"
    break;

  case 397: // gtycon: ntgtycon
#line 1424 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4598 "parser.cc"
    break;

  case 398: // gtycon: "(" ")"
#line 1425 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4604 "parser.cc"
    break;

  case 399: // gtycon: "(#" "#)"
#line 1426 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4610 "parser.cc"
    break;

  case 400: // ntgtycon: oqtycon
#line 1428 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4616 "parser.cc"
    break;

  case 401: // ntgtycon: "(" commas ")"
#line 1429 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4622 "parser.cc"
    break;

  case 402: // ntgtycon: "(#" commas "#)"
#line 1430 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4628 "parser.cc"
    break;

  case 403: // ntgtycon: "(" "->" ")"
#line 1431 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4634 "parser.cc"
    break;

  case 404: // ntgtycon: "[" "]"
#line 1432 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4640 "parser.cc"
    break;

  case 405: // oqtycon: qtycon
#line 1434 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4646 "parser.cc"
    break;

  case 406: // oqtycon: "(" qtyconsym ")"
#line 1435 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4652 "parser.cc"
    break;

  case 407: // oqtycon_no_varcon: qtycon
#line 1437 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4658 "parser.cc"
    break;

  case 408: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1438 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4664 "parser.cc"
    break;

  case 409: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1439 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4670 "parser.cc"
    break;

  case 410: // oqtycon_no_varcon: "(" ":" ")"
#line 1440 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4676 "parser.cc"
    break;

  case 411: // qtyconop: qtyconsym
#line 1443 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4682 "parser.cc"
    break;

  case 412: // qtyconop: "`" qtycon "`"
#line 1444 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4688 "parser.cc"
    break;

  case 413: // qtycondoc: qtycon
#line 1446 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4694 "parser.cc"
    break;

  case 414: // qtycon: "QCONID"
#line 1448 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4700 "parser.cc"
    break;

  case 415: // qtycon: tycon
#line 1449 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4706 "parser.cc"
    break;

  case 416: // tycon: "CONID"
#line 1453 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4712 "parser.cc"
    break;

  case 417: // qtyconsym: "QCONSYM"
#line 1455 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4718 "parser.cc"
    break;

  case 418: // qtyconsym: "QVARSYM"
#line 1456 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4724 "parser.cc"
    break;

  case 419: // qtyconsym: tyconsym
#line 1457 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4730 "parser.cc"
    break;

  case 420: // tyconsym: "CONSYM"
#line 1459 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4736 "parser.cc"
    break;

  case 421: // tyconsym: "VARSYM"
#line 1460 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4742 "parser.cc"
    break;

  case 422: // tyconsym: ":"
#line 1461 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4748 "parser.cc"
    break;

  case 423: // tyconsym: "-"
#line 1462 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4754 "parser.cc"
    break;

  case 424: // op: varop
#line 1467 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4760 "parser.cc"
    break;

  case 425: // op: conop
#line 1468 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4766 "parser.cc"
    break;

  case 426: // varop: varsym
#line 1470 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4772 "parser.cc"
    break;

  case 427: // varop: "`" varid "`"
#line 1471 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4778 "parser.cc"
    break;

  case 428: // qop: qvarop
#line 1473 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4784 "parser.cc"
    break;

  case 429: // qop: qconop
#line 1474 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4790 "parser.cc"
    break;

  case 430: // qopm: qvaropm
#line 1477 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4796 "parser.cc"
    break;

  case 431: // qopm: qconop
#line 1478 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4802 "parser.cc"
    break;

  case 432: // qvarop: qvarsym
#line 1483 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4808 "parser.cc"
    break;

  case 433: // qvarop: "`" qvarid "`"
#line 1484 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4814 "parser.cc"
    break;

  case 434: // qvaropm: qvarsym_no_minus
#line 1486 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4820 "parser.cc"
    break;

  case 435: // qvaropm: "`" qvarid "`"
#line 1487 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4826 "parser.cc"
    break;

  case 436: // tyvar: tyvarid
#line 1491 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4832 "parser.cc"
    break;

  case 437: // tyvarop: "`" tyvarid "`"
#line 1493 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4838 "parser.cc"
    break;

  case 438: // tyvarid: "VARID"
#line 1495 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4844 "parser.cc"
    break;

  case 439: // tyvarid: special_id
#line 1496 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4850 "parser.cc"
    break;

  case 440: // tyvarid: "unsafe"
#line 1497 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4856 "parser.cc"
    break;

  case 441: // tyvarid: "safe"
#line 1498 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4862 "parser.cc"
    break;

  case 442: // tyvarid: "interruptible"
#line 1499 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4868 "parser.cc"
    break;

  case 443: // var: varid
#line 1502 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4874 "parser.cc"
    break;

  case 444: // var: "(" varsym ")"
#line 1503 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4880 "parser.cc"
    break;

  case 445: // qvar: qvarid
#line 1505 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4886 "parser.cc"
    break;

  case 446: // qvar: "(" varsym ")"
#line 1506 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4892 "parser.cc"
    break;

  case 447: // qvar: "(" qvarsym1 ")"
#line 1507 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4898 "parser.cc"
    break;

  case 448: // field: varid
#line 1509 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4904 "parser.cc"
    break;

  case 449: // qvarid: varid
#line 1511 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4910 "parser.cc"
    break;

  case 450: // qvarid: "QVARID"
#line 1512 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4916 "parser.cc"
    break;

  case 451: // varid: "VARID"
#line 1514 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4922 "parser.cc"
    break;

  case 452: // varid: special_id
#line 1515 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4928 "parser.cc"
    break;

  case 453: // varid: "unsafe"
#line 1516 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4934 "parser.cc"
    break;

  case 454: // varid: "safe"
#line 1517 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4940 "parser.cc"
    break;

  case 455: // varid: "interruptible"
#line 1518 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4946 "parser.cc"
    break;

  case 456: // varid: "forall"
#line 1519 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4952 "parser.cc"
    break;

  case 457: // varid: "family"
#line 1520 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4958 "parser.cc"
    break;

  case 458: // varid: "role"
#line 1521 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4964 "parser.cc"
    break;

  case 459: // qvarsym: varsym
#line 1523 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4970 "parser.cc"
    break;

  case 460: // qvarsym: qvarsym1
#line 1524 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4976 "parser.cc"
    break;

  case 461: // qvarsym_no_minus: varsym_no_minus
#line 1526 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4982 "parser.cc"
    break;

  case 462: // qvarsym_no_minus: qvarsym1
#line 1527 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4988 "parser.cc"
    break;

  case 463: // qvarsym1: "QVARSYM"
#line 1529 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4994 "parser.cc"
    break;

  case 464: // varsym: varsym_no_minus
#line 1531 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5000 "parser.cc"
    break;

  case 465: // varsym: "-"
#line 1532 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5006 "parser.cc"
    break;

  case 466: // varsym_no_minus: "VARSYM"
#line 1534 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5012 "parser.cc"
    break;

  case 467: // varsym_no_minus: special_sym
#line 1535 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5018 "parser.cc"
    break;

  case 468: // special_id: "as"
#line 1537 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5024 "parser.cc"
    break;

  case 469: // special_id: "qualified"
#line 1538 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5030 "parser.cc"
    break;

  case 470: // special_id: "hiding"
#line 1539 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5036 "parser.cc"
    break;

  case 471: // special_id: "export"
#line 1540 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5042 "parser.cc"
    break;

  case 472: // special_id: "label"
#line 1541 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5048 "parser.cc"
    break;

  case 473: // special_id: "dynamic"
#line 1542 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5054 "parser.cc"
    break;

  case 474: // special_id: "stdcall"
#line 1543 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5060 "parser.cc"
    break;

  case 475: // special_id: "ccall"
#line 1544 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5066 "parser.cc"
    break;

  case 476: // special_id: "capi"
#line 1545 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5072 "parser.cc"
    break;

  case 477: // special_id: "prim"
#line 1546 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5078 "parser.cc"
    break;

  case 478: // special_id: "javascript"
#line 1547 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5084 "parser.cc"
    break;

  case 479: // special_id: "group"
#line 1548 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5090 "parser.cc"
    break;

  case 480: // special_id: "stock"
#line 1549 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5096 "parser.cc"
    break;

  case 481: // special_id: "anyclass"
#line 1550 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5102 "parser.cc"
    break;

  case 482: // special_id: "via"
#line 1551 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5108 "parser.cc"
    break;

  case 483: // special_id: "unit"
#line 1552 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5114 "parser.cc"
    break;

  case 484: // special_id: "dependency"
#line 1553 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5120 "parser.cc"
    break;

  case 485: // special_id: "signature"
#line 1554 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5126 "parser.cc"
    break;

  case 486: // special_sym: "."
#line 1556 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5132 "parser.cc"
    break;

  case 487: // special_sym: "*"
#line 1557 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5138 "parser.cc"
    break;

  case 488: // qconid: conid
#line 1561 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5144 "parser.cc"
    break;

  case 489: // qconid: "QCONID"
#line 1562 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5150 "parser.cc"
    break;

  case 490: // conid: "CONID"
#line 1564 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5156 "parser.cc"
    break;

  case 491: // qconsym: consym
#line 1566 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5162 "parser.cc"
    break;

  case 492: // qconsym: "QCONSYM"
#line 1567 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5168 "parser.cc"
    break;

  case 493: // consym: "CONSYM"
#line 1569 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5174 "parser.cc"
    break;

  case 494: // consym: ":"
#line 1570 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5180 "parser.cc"
    break;

  case 495: // literal: "CHAR"
#line 1574 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5186 "parser.cc"
    break;

  case 496: // literal: "STRING"
#line 1575 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5192 "parser.cc"
    break;

  case 497: // literal: "INTEGER"
#line 1576 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5198 "parser.cc"
    break;

  case 498: // literal: "RATIONAL"
#line 1577 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5204 "parser.cc"
    break;

  case 499: // literal: "PRIMINTEGER"
#line 1578 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5210 "parser.cc"
    break;

  case 501: // close: error
#line 1586 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5216 "parser.cc"
    break;

  case 502: // modid: "CONID"
#line 1590 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5222 "parser.cc"
    break;

  case 503: // modid: "QCONID"
#line 1591 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5228 "parser.cc"
    break;

  case 504: // commas: commas ","
#line 1593 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5234 "parser.cc"
    break;

  case 505: // commas: ","
#line 1594 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5240 "parser.cc"
    break;


#line 5244 "parser.cc"

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


  const short parser::yypact_ninf_ = -650;

  const short parser::yytable_ninf_ = -465;

  const short
  parser::yypact_[] =
  {
      44,    -6,  -650,    94,  -650,  -650,  -650,  -650,  -650,   121,
     101,    95,  -650,    45,   -19,   -19,   129,  -650,  -650,  -650,
    -650,   218,  -650,  -650,  -650,   122,  -650,   220,   231,  1416,
     261,   317,   234,  -650,   863,  -650,   211,  -650,  -650,  -650,
    -650,    -6,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
    -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
    -650,  -650,  -650,  -650,   643,  -650,  -650,  -650,  -650,  -650,
     268,   -32,  -650,   273,  -650,  -650,  -650,  -650,  -650,  -650,
    -650,    -4,  -650,    -6,  -650,   271,  -650,  2431,  4478,   360,
     303,   347,  2431,  -650,  -650,  -650,   447,   351,  -650,  3539,
     440,   347,  3111,   329,  4822,   272,  3111,  3111,  2703,  3111,
    1751,  1615,   154,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
      61,   329,   332,   234,  -650,  -650,  -650,  -650,    54,   -13,
    -650,  -650,   412,  -650,  2839,  -650,   334,  -650,  -650,  -650,
    -650,  -650,  -650,   386,   100,  -650,  -650,  -650,  -650,   346,
    -650,   381,  -650,  -650,  -650,  -650,   385,  -650,   399,   404,
     415,  -650,  -650,  -650,  4583,  -650,  4620,  -650,  -650,  -650,
    -650,   529,  -650,  1615,   511,   321,  -650,  -650,  -650,  4478,
    4478,  -650,  4921,  3640,  3223,   416,  -650,   515,   452,  -650,
     649,  -650,  3827,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
    4478,  3928,  2159,  2159,  -650,   420,   477,   482,   483,   484,
    3928,  1292,  1292,  -650,   547,  4478,  4478,   110,   490,   854,
     111,   531,  -650,  -650,     3,  4822,  -650,   340,    20,   464,
     185,  -650,   119,  -650,  -650,  -650,  -650,  2975,  -650,  2839,
    -650,  -650,  -650,   801,  -650,  -650,  -650,   321,   117,   465,
     456,  -650,  2431,  -650,  -650,  -650,  -650,  -650,  -650,  5044,
    -650,  -650,   157,   275,   289,   404,   466,   468,   476,   294,
    -650,   248,  3928,  4822,  4822,  -650,   124,   271,   506,   458,
    4478,  3928,  4921,  2431,  2567,   801,  -650,    50,  -650,  -650,
    2431,  -650,  -650,  -650,  -650,  4478,  -650,  5044,  4785,  3111,
    -650,  -650,  -650,  -650,  -650,  -650,  -650,   486,   487,   492,
    -650,   491,    45,    -6,    46,   359,  3928,  -650,  -650,   193,
     120,   507,   499,  -650,  -650,  -650,  -650,   497,   540,   530,
    -650,  -650,   509,  -650,  -650,  -650,  -650,  -650,  -650,   510,
     514,   522,  -650,   306,   251,   384,  -650,  4478,  3928,  4721,
    4478,  -650,  -650,  -650,  4478,  -650,  -650,   548,  -650,   523,
     521,   351,   347,   557,   559,   -15,  -650,  -650,    32,  -650,
     621,  -650,  -650,  -650,  -650,  -650,  -650,   622,   167,  -650,
    -650,   412,    47,  2431,  -650,   569,   242,  3928,   130,  3928,
     518,   520,   543,   578,  -650,   585,   552,   344,   272,   591,
    2431,  -650,   551,   555,  2431,  2431,  2567,  1887,  -650,  1887,
     473,  -650,  -650,  5044,  -650,  -650,  1887,  -650,  1887,   131,
    -650,  -650,  -650,  -650,   595,   597,   605,  4884,   567,  -650,
    -650,  -650,  -650,  -650,  4029,    56,   330,  -650,  -650,  -650,
    -650,   666,   613,   577,  -650,   579,   351,  -650,  -650,  -650,
    -650,  -650,  -650,   592,  -650,   582,   617,   602,   610,  -650,
    -650,  -650,  4684,  -650,  -650,  -650,   594,  1416,  -650,  -650,
    2023,  1479,  -650,  -650,   607,  3928,  -650,  4921,  5081,  -650,
    3928,  3928,  -650,  -650,  3928,  -650,  -650,  -650,  1156,  1156,
    -650,  -650,  -650,   598,   606,   254,  -650,  3928,  -650,  -650,
    3928,   547,  -650,  2431,  -650,  2159,  -650,  2431,   391,  -650,
    -650,  1292,  -650,  -650,  3928,  3928,  5194,   647,  -650,  -650,
     501,  -650,  -650,  4921,   618,  -650,  -650,  -650,  -650,   623,
     637,   258,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
     619,  -650,   663,  -650,  -650,  -650,  -650,  -650,  -650,  3928,
    3928,   624,   625,   124,  -650,   330,   656,  -650,  -650,   671,
    3928,   721,   725,   744,  -650,  2431,  2567,  -650,  -650,  -650,
    4785,  1887,  5044,  -650,  1416,   646,  -650,  2295,  -650,   655,
     645,  -650,   423,    45,  -650,  -650,  -650,  -650,  3928,  5280,
    5280,  -650,  -650,  -650,  -650,  -650,   651,   733,  3741,  -650,
    -650,   175,  -650,    48,  -650,  -650,  -650,  -650,  -650,  -650,
     420,  1014,  1014,  -650,  -650,  -650,  -650,  -650,  5280,   748,
     696,  -650,  -650,  -650,  2567,  2431,  -650,     5,    38,  -650,
    -650,  -650,  4958,   725,   744,  4478,  -650,  -650,  -650,   695,
    -650,  4478,   407,   744,   232,  -650,   744,  -650,  -650,  -650,
    -650,  -650,    17,  -650,   667,  -650,  -650,  -650,  2431,  2567,
    2431,  -650,    58,  -650,  -650,  -650,   118,   702,  -650,  -650,
    4478,  4478,  4478,  -650,   349,  -650,  1156,  -650,   772,  -650,
     765,  -650,   765,  -650,   208,  -650,    49,  -650,   699,   425,
    -650,  3928,  -650,  -650,  -650,  3928,  -650,  4478,  4478,   744,
    -650,  -650,  5118,   721,   701,  3329,  -650,  -650,  -650,   420,
     420,  -650,  -650,  -650,  -650,  4115,   144,   737,  -650,  -650,
    -650,  1887,  5044,  -650,  -650,   700,   666,  -650,  -650,  3928,
    -650,  3928,   548,  -650,   379,  3928,  4220,  -650,  -650,  2431,
    -650,  4478,   506,  -650,  1014,  -650,  5280,  4306,  4392,  -650,
    -650,  -650,  -650,   703,   254,  -650,  -650,  -650,  4478,   670,
    -650,  4478,   243,  -650,   272,    53,  -650,  -650,   676,   683,
    -650,  4478,  -650,  -650,  -650,  2431,  -650,   692,   685,  -650,
    5227,  -650,  -650,  3223,   715,   716,  -650,  -650,  4029,  -650,
    5280,  -650,   697,   244,  -650,    45,    65,  4478,  3434,  -650,
    4478,  -650,   420,   132,  -650,  4478,  -650,  -650,  -650,  -650,
    -650,   702,  5280,   330,  -650,  -650,  -650,  4478,  -650,  -650,
    -650,  -650,  3928,  -650,  -650,   725,   744,  -650,  -650,   744,
    -650,  -650
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   502,   503,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    61,   501,   500,    12,   174,   170,     0,     0,    20,
       0,    45,    40,    15,    14,   173,     0,     6,     7,   468,
     470,     0,   469,   456,   471,   472,   473,   454,   455,   453,
     457,   458,   474,   475,   476,   477,   478,   479,   480,   481,
     482,   483,   485,   484,     0,   451,   416,   450,   414,    22,
       0,    19,    24,    27,    35,   407,   415,    34,   445,   449,
     452,     0,    44,     0,    37,    41,   314,     0,     0,   120,
       0,     0,     0,    54,    55,    56,    87,     0,   121,     0,
       0,     0,     0,   278,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   490,   489,   495,   496,   497,   498,   499,
     278,   278,    52,    59,    62,    63,    64,    65,   140,     0,
      68,   261,    69,   284,   287,   293,   303,   306,   308,   378,
     391,   379,   180,   307,   449,   380,   488,   309,   171,     0,
      26,     0,   465,   487,   486,   466,     0,   463,     0,     0,
       0,   464,   467,    17,     0,    21,    30,    25,    39,    39,
       3,    47,    36,     0,     0,   283,   441,   442,   440,     0,
       0,   208,   246,     0,     0,     0,   438,   150,     0,   133,
     191,   194,   195,   199,   206,   400,   405,   207,   436,   439,
       0,   220,   361,   361,   301,   289,     0,     0,     0,     0,
       0,   164,   164,   167,     0,     0,     0,     0,     0,   191,
     400,     0,   302,   292,     0,     0,   279,     0,     0,     0,
       0,   386,   175,   384,   382,   353,   355,     0,   295,   286,
     296,   494,   392,     0,   493,   492,   317,   283,   322,     0,
     323,   431,     0,   430,   434,   462,   461,   395,   491,     0,
     387,   505,     0,     0,     0,   462,     0,   461,   395,     0,
     389,     0,     0,     0,     0,    53,     0,    60,   140,     0,
       0,     0,     0,     0,     0,     0,   262,   169,   267,   429,
       0,   428,   432,   460,   459,     0,   290,     0,   368,     0,
     172,   410,   409,   408,   447,   446,    23,     0,     0,    31,
      33,     0,     0,     0,    49,     0,     0,   210,   209,     0,
       0,     0,   247,   249,   443,   224,   404,     0,   183,     0,
     187,   422,     0,   423,   212,   421,   420,   418,   417,   221,
       0,     0,   419,     0,     0,     0,    70,     0,     0,     0,
       0,   203,   411,   204,     0,   200,   202,   124,   221,     0,
     219,     0,     0,   365,     0,     0,   360,   362,     0,   288,
       0,    84,    83,    85,    86,   216,   177,   160,     0,   263,
     163,     0,     0,     0,    80,     0,   126,     0,     0,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,   354,     0,     0,   318,   324,     0,     0,   313,     0,
     319,   316,   448,     0,   312,   310,     0,   311,     0,   446,
     381,   388,   504,   390,     0,     0,     0,     0,   270,   425,
      58,   424,   426,   393,     0,     0,   122,   269,   188,   178,
     179,   169,     0,   333,   335,     0,     0,   265,   266,   285,
     291,   305,   371,     0,   367,   370,   373,     0,   449,   294,
      29,    28,     0,     9,    10,    46,     0,    20,    43,    48,
       0,     0,   300,   282,     0,     0,   211,     0,     0,   214,
       0,     0,   403,   215,     0,   406,   401,   402,   146,   146,
     149,   132,   192,     0,     0,   196,   201,     0,    75,    66,
       0,   366,   363,     0,   356,   359,   357,     0,     0,    79,
     165,   162,   166,   298,     0,     0,     0,    92,   230,    76,
       0,    77,    71,     0,     0,   280,   272,   274,   383,     0,
       0,     0,   176,   397,   385,   271,   297,   435,   396,   326,
     328,   332,   317,   330,   329,   315,   321,   320,   277,     0,
       0,     0,     0,     0,   224,   122,     0,   137,   139,     0,
       0,   243,   233,   251,   264,     0,     0,   433,   168,   304,
       0,     0,     0,    32,    20,     0,   338,     0,   351,     0,
     340,   344,     0,     0,   339,   444,   250,   248,     0,     0,
       0,   223,   225,   228,   184,   186,   222,   113,     0,   141,
     145,     0,   142,     0,   412,   437,   125,   222,   364,   358,
     289,   156,   156,   159,   161,   107,   127,   128,     0,    97,
       0,   281,   398,   399,     0,   325,   181,     0,     0,   427,
     394,    57,     0,   233,   251,     0,   138,   123,   224,   237,
     239,     0,     0,   251,     0,    72,   252,   254,   268,   334,
     369,   372,   375,   377,     0,    50,   341,   336,   343,     0,
       0,   345,   169,   349,   337,   185,     0,     0,   213,   114,
       0,     0,     0,   111,   129,   147,   144,   148,     0,   120,
     115,   151,   115,   155,     0,   152,     0,    93,     0,     0,
      74,     0,   331,   327,   275,     0,   276,     0,     0,   251,
      81,   136,     0,   243,     0,   244,   197,   205,   241,   289,
     289,    73,    90,    88,    89,     0,     0,   255,   258,   413,
     253,     0,     0,    51,   342,     0,   169,   347,   348,     0,
     226,     0,   124,   112,   129,     0,     0,   109,   143,     0,
     116,     0,   140,   157,   154,   158,     0,   106,   106,    98,
      67,   182,   135,     0,   189,    82,   242,   238,     0,     0,
     198,     0,     0,   235,     0,     0,   259,   193,   217,     0,
     256,     0,   257,   374,   376,     0,   346,     0,     0,   108,
       0,   110,   130,     0,     0,   207,   299,   117,     0,   153,
      94,    96,     0,     0,   105,     0,     0,     0,   244,   240,
     245,   231,   289,     0,   232,     0,   260,    91,   350,   227,
     229,   207,     0,   122,    95,   101,    99,   104,   102,   100,
     134,   234,     0,   218,   131,   233,   251,   103,   236,   251,
     118,   119
  };

  const short
  parser::yypgoto_[] =
  {
    -650,  -650,  -650,  -650,  -650,  -650,  -650,    40,  -650,  -650,
    -423,  -650,   639,  -650,  -650,  -650,  -147,   678,  -650,  -650,
    -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
    -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,  -650,
    -650,    -8,  -650,  -650,  -650,    60,  -193,  -650,  -650,   127,
    -650,   776,  -526,    74,  -650,    78,   534,    28,  -262,   141,
     331,  -650,  -650,    75,   206,  -650,  -650,   609,  -650,  -299,
    -394,   808,  -650,  -650,  -304,   133,  -159,   278,  -142,  -146,
    -650,   -83,  -650,   -84,  -650,    10,  -650,  -314,  -650,  -650,
    -650,  -636,  -166,   558,    37,  -650,   636,  -493,   335,  -595,
    -392,  -594,   136,    57,  -521,  -650,   155,  -650,    89,  -650,
    -650,   383,  -592,  -650,   216,   147,   830,  -180,  -650,  -650,
     587,  -650,   403,  -650,   284,    67,  -230,  -187,   757,   -23,
    -650,  -650,  -650,   -82,  -650,  -650,  -650,  -650,   213,  -650,
    -650,  -418,  -650,   219,  -650,  -650,   222,  -650,  -650,   641,
    -650,   -80,   687,   406,  -260,  -650,   316,  -650,  -650,  -650,
    -650,   524,   140,  -650,   -98,  -649,  -107,  -650,   528,   -79,
    -650,  -650,  -650,   -28,  -650,  -184,  -650,   368,  -650,   665,
    -650,  -650,  -650,  -408,  -650,  -306,  -246,   252,  -219,   -67,
      -7,  -650,  -650,   -12,   -54,  -102,   -88,  -650,   -64,  -103,
     -30,  -213,  -650,  -238,   -11,  -109
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      70,    71,    72,   167,   308,   309,    73,    85,    11,    20,
      21,    32,    83,   314,   468,   469,   276,   122,   428,    33,
      34,   123,   124,   125,   126,   217,   127,   210,   716,   772,
     619,   687,   790,   690,   749,   793,   794,   599,   670,   741,
     681,   682,   562,   498,   517,   737,   187,   555,   280,   600,
     601,   490,   346,   683,   684,   613,   509,   378,   213,   214,
     447,    27,    36,   399,   375,   437,   129,   627,   339,   518,
     439,   329,   704,   330,   768,   190,   191,   705,   192,   355,
     350,   706,   193,   377,   769,   359,   340,   478,   591,   592,
     519,   643,   762,   763,   563,   639,   640,   641,   708,   321,
     322,   323,   645,   646,   647,   717,   379,   602,   286,   287,
     288,   131,   225,   226,   246,   175,   133,   764,   134,   135,
     136,   137,   262,   263,   264,   249,   250,   540,   442,   443,
     472,   579,   580,   581,   661,   662,   663,   582,   364,   236,
     237,   204,   365,   366,   367,   453,   454,   455,   652,   138,
     139,   231,   232,   140,   141,   429,   251,   532,   194,   195,
      74,   351,   718,   196,    76,   341,   342,   430,   431,   290,
     252,   291,   253,   197,   353,   198,   142,   143,   457,    78,
      79,   292,   254,   255,   294,   161,    80,   162,   145,   146,
     257,   258,   147,    24,     9,   269
  };

  const short
  parser::yytable_[] =
  {
     199,    75,   234,   271,   189,   188,   352,   233,   256,   267,
     160,   199,   473,   317,   318,   218,   434,   396,   370,   310,
     220,   222,   384,   320,   444,   289,   356,   144,   248,   633,
     150,   380,   380,    22,   634,   352,   440,   328,   328,   699,
     411,   327,   700,   494,   575,    13,    22,   564,    22,    22,
      22,   711,   159,   583,    22,   328,   761,   266,   466,   358,
     449,   632,   501,   433,   376,     1,    22,   281,   289,   760,
     593,   267,   171,   278,   464,   343,   344,   446,   451,   223,
     272,   268,   694,   235,   238,   446,   240,   164,   594,    25,
     504,   199,   199,   391,    12,   199,   199,   394,   721,   265,
     168,   132,   169,   505,   199,   606,   282,   755,   593,   219,
     165,   296,   199,   199,    26,   696,   357,     7,   722,   160,
     293,     8,   199,   616,   695,   279,   376,   199,   199,   450,
     506,   385,   386,   558,   284,   438,    75,   392,    75,    23,
     289,   784,   659,   268,   512,   702,   541,   568,     2,   761,
     505,   654,    23,   395,    23,    23,    23,   695,   467,   656,
      23,   265,   760,   293,   760,   511,   676,   744,   637,   224,
     376,   802,    23,   160,   449,   324,   402,   247,   247,   403,
    -443,   666,   667,   817,   199,   784,    14,    15,   496,   559,
     387,   -78,   199,   199,   545,   405,   189,   188,   729,   397,
     475,   406,   492,   241,   144,   144,    17,   199,   311,   312,
     219,  -444,   822,    18,   235,   159,   296,   152,   445,  -443,
     153,   403,   432,   730,   593,   219,   219,   154,   199,   388,
     -78,   829,    31,   584,   830,   293,   407,   831,   398,   282,
     247,    29,   520,   328,   427,   626,   626,   522,   155,   244,
    -444,   398,   412,    66,   712,    35,   715,    68,   413,   199,
     199,   199,   199,   491,   241,   474,   199,    66,   727,   270,
     414,    68,   510,   261,   289,   324,   459,   620,   381,   381,
     675,    77,   502,   713,   714,   511,   152,   825,    81,   153,
     412,   458,   826,   676,   593,   234,   154,    37,   260,   199,
     233,   199,   465,   289,   261,   256,   649,   256,    38,   521,
     244,   352,   688,   743,   256,   573,   256,   155,   320,   410,
     148,   493,   515,   516,   552,   543,   744,   544,   785,   586,
     149,   614,   776,   331,   546,   595,   547,   777,   328,   778,
     433,    82,   596,   782,   715,   664,   199,   333,   801,   816,
     557,   556,    84,   653,   328,    66,   228,   219,   607,    68,
     495,   802,   817,   423,   692,   677,   487,   422,   615,   293,
     422,   174,   593,   623,   349,   811,   205,   261,   335,   336,
     229,   163,   337,   338,   230,   166,   112,   199,   415,   172,
     199,   751,   199,   199,   416,   113,   199,   200,   293,   444,
     241,   316,   417,   376,   376,   673,   412,   421,   418,   199,
     560,   561,   199,   422,   152,   201,    77,   153,    77,   486,
     551,   343,   344,   678,   154,   422,   199,   199,   199,   735,
     736,   685,   685,   152,    75,   297,   153,   224,   298,    75,
     791,   285,   665,   154,   798,   155,   244,   800,   745,   157,
     245,   202,   529,   203,   221,   211,   530,   212,   531,   735,
     780,   199,   199,   470,   155,   471,   275,    66,   157,   256,
     324,    68,   199,   299,   247,   707,   247,   393,   733,   300,
     788,   144,   144,   247,   814,   247,   363,   363,   488,   651,
     489,   241,   433,   283,   301,   611,   284,   612,   302,   432,
     199,   199,   199,   774,   144,   152,   688,   659,   153,   660,
     199,   709,   303,   710,   674,   154,   324,   304,   828,   206,
     207,   208,   209,   273,   274,   425,   426,   804,   305,   747,
     199,   748,   285,   313,   315,   261,   155,   244,   369,   707,
     157,   245,   345,   347,   199,   438,    75,   199,   787,   376,
     456,   701,   241,   199,   371,   381,   381,   818,   819,   372,
     373,   374,   383,   458,   685,   412,   152,   441,   363,   153,
     352,   389,   390,   242,   408,   409,   154,   279,   381,   419,
     331,  -464,   199,   199,   199,   433,   732,   385,   734,   420,
     615,   435,   707,   285,   333,   707,   463,   155,   244,   460,
     461,   157,   245,   199,   144,   144,   479,   199,   219,   199,
     199,   462,   476,   752,   199,   753,   719,   199,   477,   256,
     480,   481,   482,   483,   827,   335,   336,   199,   497,   337,
     338,   767,   707,   484,   707,   485,   499,   328,   247,   773,
     500,   199,  -352,   199,   503,   219,   507,   199,   199,   508,
     514,   523,   525,   199,   524,   526,   552,   385,   199,   199,
     199,   234,   527,   385,   385,   528,   233,   513,   535,   144,
     199,   537,   548,   199,   343,   538,   376,   549,   381,   381,
     219,   219,   219,   199,   536,   550,   553,   807,   719,   539,
     363,   542,   199,   446,   565,   199,   566,   569,   571,   567,
     199,   570,   199,   572,   557,   556,   574,   219,   754,   199,
     199,  -448,   199,   820,    77,   412,   331,   199,   604,    77,
     585,   767,   151,   332,   199,   219,   605,   621,   331,   199,
     333,   618,   326,   385,   199,   348,   152,   144,   624,   153,
    -189,   625,   333,   381,   629,   630,   154,   635,   636,   638,
     622,   219,   642,   644,   578,   578,   261,   219,   219,   655,
     657,   335,   336,   658,   668,   337,   338,   155,   156,   349,
     669,   157,   158,   335,   336,   689,   691,   337,   338,   703,
     723,   219,   731,   739,   740,   746,   775,   608,   247,   363,
     771,   610,   758,   113,   797,   805,   806,   809,   810,   812,
    -228,   277,   815,   306,   824,    39,   779,   219,   796,   742,
     128,   381,   781,    40,   436,   219,   813,   738,   686,   789,
     603,   382,   456,    28,   750,    42,    77,   219,   628,    43,
     424,    44,    45,    46,    47,    48,    49,   360,    50,    51,
      52,    53,   823,    54,    55,    56,   765,   799,    57,   648,
     363,   617,    58,    59,    60,    61,    62,    63,   757,   821,
     587,   578,   720,   770,   130,   239,    86,    39,    87,    88,
      89,    90,   725,    91,   448,    40,    92,   724,   401,    93,
      94,    95,    96,    97,   728,    98,   650,    42,     0,    99,
     368,    43,   100,    44,    45,    46,    47,    48,    49,   101,
      50,    51,    52,    53,   803,    54,    55,    56,   363,   693,
      57,   609,   404,   102,    58,    59,    60,    61,    62,    63,
     103,   631,   534,    65,   113,   533,   104,    67,   114,     0,
       0,     0,     0,   331,     0,     0,     0,     0,     0,   105,
     348,     0,   578,   363,   726,   106,     0,   333,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,   349,   111,     0,   112,   335,   336,
       0,     0,   337,   338,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,     0,     0,     0,     0,   120,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    86,    39,    87,
       0,   679,     0,   786,    91,     0,    40,    92,     0,     0,
      93,    94,    95,     0,    97,     0,    98,     0,    42,     0,
     680,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     101,    50,    51,    52,    53,     0,    54,    55,    56,   808,
       0,    57,     0,     0,   102,    58,    59,    60,    61,    62,
      63,   103,     0,     0,     0,     0,     0,   104,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,   107,     0,     0,   108,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,     0,     0,     0,     0,   120,   121,    86,
      39,    87,     0,   597,     0,     0,    91,     0,    40,    92,
       0,     0,    93,    94,    95,     0,    97,     0,     0,     0,
      42,     0,   598,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   101,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   102,    58,    59,    60,
      61,    62,    63,   103,     0,     0,     0,     0,     0,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   107,     0,     0,   108,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   111,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    86,    39,    87,     0,   120,
     121,     0,    91,     0,    40,    92,     0,     0,    93,    94,
      95,     0,    97,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   101,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   102,    58,    59,    60,    61,    62,    63,   103,
       0,     0,     0,     0,     0,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   105,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,   107,     0,     0,   108,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   111,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
      39,     0,     0,     0,   115,   116,   117,   118,    40,     0,
     119,     0,     0,     0,     0,   120,   121,    41,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
      22,     0,    86,    39,    87,     0,     0,     0,     0,    91,
       0,    40,    92,     0,     0,     0,     0,     0,     0,    97,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   101,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,    64,   102,
      58,    59,    60,    61,    62,    63,     0,     0,    65,    66,
       0,     0,    67,    68,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,   106,     0,     0,     0,     0,     0,     0,   107,     0,
       0,   108,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    23,   110,     0,     0,
       0,   173,     0,   112,     0,     0,     0,   577,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    86,    39,
      87,     0,     0,     0,     0,    91,     0,    40,    92,     0,
       0,     0,     0,     0,     0,    97,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   101,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   102,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   241,     0,     0,   106,     0,     0,
       0,     0,     0,     0,   107,     0,     0,   108,   152,   109,
       0,   153,     0,     0,     0,     0,     0,   259,   154,     0,
       0,     0,     0,   110,     0,     0,     0,   173,   260,   112,
       0,     0,     0,     0,   261,   243,     0,    65,   113,   155,
     244,    67,   114,   157,   245,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    86,    39,    87,     0,     0,     0,
       0,    91,     0,    40,    92,     0,     0,     0,     0,     0,
       0,    97,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   101,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   102,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     241,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,   109,     0,   153,     0,     0,
       0,     0,     0,     0,   154,     0,     0,     0,     0,   110,
     242,     0,     0,   173,     0,   112,     0,     0,     0,     0,
       0,   243,     0,    65,   113,   155,   244,    67,   114,   157,
     245,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      86,    39,    87,     0,     0,     0,     0,    91,     0,    40,
      92,     0,     0,     0,     0,     0,     0,    97,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   101,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   102,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   241,     0,     0,   106,
       0,     0,     0,     0,     0,     0,   107,     0,     0,   108,
       0,   109,     0,   153,     0,     0,     0,     0,     0,     0,
     154,     0,     0,     0,     0,   110,     0,     0,     0,   173,
       0,   112,     0,     0,     0,     0,     0,   243,     0,    65,
     113,   155,   244,    67,   114,   157,   245,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    86,    39,    87,     0,
       0,     0,     0,    91,     0,    40,    92,     0,     0,     0,
       0,     0,     0,    97,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   101,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   102,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,   108,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   576,     0,
       0,   110,     0,     0,     0,   173,     0,   112,     0,     0,
       0,   577,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    86,    39,    87,     0,     0,     0,     0,    91,
       0,    40,    92,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   101,    50,    51,    52,    53,
       0,    54,    55,    56,     0,   362,    57,     0,     0,   102,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,     0,   107,     0,
       0,   108,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   173,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    86,    39,
      87,     0,     0,     0,     0,    91,     0,    40,    92,     0,
       0,     0,     0,     0,     0,    97,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   101,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,   102,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,   107,     0,     0,   108,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   173,     0,   112,
       0,     0,     0,   577,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    86,    39,    87,     0,     0,     0,
       0,    91,     0,    40,    92,     0,     0,     0,     0,     0,
       0,    97,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   101,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   102,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   173,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      86,    39,    87,     0,     0,     0,     0,    91,     0,    40,
      92,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   101,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   102,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,   107,     0,     0,   108,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   173,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,    86,    39,    87,     0,
       0,     0,     0,    91,     0,    40,    92,     0,     0,     0,
       0,     0,     0,    97,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,   101,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,   102,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,   173,     0,   112,     0,     0,
       0,     0,     0,     0,     0,    65,   113,     0,     0,    67,
     114,     0,     0,     0,     0,   115,   116,   117,   118,     0,
       0,   119,    86,    39,    87,     0,     0,     0,     0,    91,
       0,    40,    92,     0,     0,     0,     0,     0,     0,    97,
       0,     0,     0,    42,     0,     0,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   101,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   106,     0,     0,     0,     0,     0,   295,   107,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   173,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,    86,    39,
      87,     0,     0,     0,     0,    91,     0,    40,    92,     0,
       0,     0,     0,     0,     0,    97,     0,     0,     0,    42,
       0,     0,     0,    43,     0,    44,    45,    46,    47,    48,
      49,   101,    50,    51,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,   400,     0,     0,   107,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   173,     0,   112,
       0,     0,     0,     0,     0,     0,     0,    65,   113,     0,
       0,    67,   114,     0,     0,     0,     0,   115,   116,   117,
     118,     0,     0,   119,    86,    39,    87,     0,     0,     0,
       0,    91,     0,    40,    92,     0,     0,     0,     0,     0,
       0,    97,     0,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   101,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   173,     0,   112,     0,    39,     0,     0,
       0,     0,     0,    65,   113,    40,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,    42,     0,   119,
       0,   325,     0,    44,    45,    46,   176,   177,   178,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   331,     0,     0,     0,     0,     0,     0,   332,
       0,     0,   179,     0,     0,     0,   333,   180,     0,   181,
       0,     0,     0,     0,     0,     0,     0,   182,     0,     0,
       0,   183,     0,    39,     0,   184,   334,   185,     0,     0,
       0,    40,   261,     0,     0,   186,    66,   335,   336,     0,
      68,   337,   338,    42,     0,     0,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   241,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,     0,
    -190,     0,     0,   180,     0,   181,     0,     0,     0,     0,
       0,     0,     0,   182,     0,     0,     0,   183,    39,     0,
       0,   184,     0,   185,     0,     0,    40,     0,     0,   759,
       0,   186,    66,     0,   244,     0,    68,     0,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   176,   177,   178,
       0,     0,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   241,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,   180,     0,
     181,     0,     0,     0,     0,     0,     0,     0,   182,     0,
       0,     0,   183,    39,     0,     0,   184,     0,   185,     0,
       0,    40,     0,     0,   759,     0,   186,    66,   215,   244,
       0,    68,     0,    42,     0,     0,     0,     0,     0,    44,
      45,    46,   176,   177,   178,     0,   216,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,     0,
       0,     0,     0,   180,     0,   181,     0,     0,     0,     0,
       0,     0,     0,   182,    39,     0,     0,   183,     0,     0,
       0,   184,    40,   185,     0,     0,     0,     0,     0,     0,
       0,   186,    66,     0,    42,     0,    68,     0,   325,     0,
      44,    45,    46,   176,   177,   178,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
       0,     0,     0,     0,   180,     0,   181,     0,     0,     0,
       0,     0,     0,     0,   182,    39,     0,     0,   183,   326,
       0,     0,   184,    40,   185,     0,     0,     0,     0,     0,
     671,     0,   186,    66,     0,    42,     0,    68,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,   672,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,    39,     0,     0,     0,   180,     0,   181,     0,    40,
       0,     0,     0,     0,     0,   182,     0,     0,     0,   183,
       0,    42,     0,   184,     0,   185,     0,    44,    45,    46,
     176,   177,   178,   186,    66,     0,    52,    53,    68,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   354,   179,     0,     0,     0,
       0,   180,     0,   181,     0,     0,     0,     0,     0,     0,
       0,   182,    39,     0,     0,   183,     0,     0,     0,   184,
      40,   185,     0,     0,     0,     0,     0,     0,     0,   186,
      66,     0,    42,     0,    68,     0,   325,     0,    44,    45,
      46,   176,   177,   178,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   179,     0,     0,
       0,     0,   180,     0,   181,     0,     0,     0,     0,     0,
       0,     0,   182,    39,     0,     0,   183,     0,     0,     0,
     184,    40,   185,     0,     0,     0,     0,     0,     0,     0,
     186,    66,     0,    42,     0,    68,     0,   554,     0,    44,
      45,    46,   176,   177,   178,     0,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,    39,
       0,     0,     0,   180,     0,   181,     0,    40,     0,     0,
       0,     0,     0,   182,     0,     0,     0,   183,     0,    42,
       0,   184,     0,   185,     0,    44,    45,    46,   176,   177,
     178,   186,    66,     0,    52,    53,    68,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   179,     0,     0,     0,     0,   180,
       0,   181,     0,     0,     0,     0,     0,     0,     0,   182,
       0,     0,     0,   183,    39,     0,     0,   184,   766,   185,
       0,     0,    40,     0,     0,     0,     0,   186,    66,     0,
       0,     0,    68,     0,    42,     0,     0,     0,   325,     0,
      44,    45,    46,   176,   177,   178,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
      39,     0,     0,     0,   180,     0,   181,     0,    40,     0,
       0,     0,     0,     0,   182,     0,     0,     0,   183,     0,
      42,     0,   783,     0,   185,     0,    44,    45,    46,   176,
     177,   178,   186,    66,     0,    52,    53,    68,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   792,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   179,    39,     0,     0,     0,
     180,     0,   181,     0,    40,     0,     0,     0,     0,     0,
     182,     0,     0,     0,   183,     0,    42,     0,   184,     0,
     185,     0,    44,    45,    46,   176,   177,   178,   186,    66,
       0,    52,    53,    68,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     795,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,    39,     0,     0,     0,   180,     0,   181,     0,
      40,     0,     0,     0,     0,     0,   182,     0,     0,     0,
     183,     0,    42,     0,   184,     0,   185,     0,    44,    45,
      46,   176,   177,   178,   186,    66,     0,    52,    53,    68,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   179,     0,     0,
       0,     0,   180,     0,   181,     0,     0,     0,     0,     0,
       0,     0,   182,     0,     0,     0,   183,    39,     0,     0,
     184,     0,   185,     0,     0,    40,     0,     0,     0,     0,
     186,    66,     0,     0,    41,    68,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,    39,    54,    55,    56,     0,     0,
      57,     0,    40,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,    64,    40,     0,   307,     0,
       0,     0,     0,     0,     0,    65,    66,     0,    42,    67,
      68,     0,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,    39,    54,    55,    56,     0,
       0,    57,    64,    40,     0,    58,    59,    60,    61,    62,
      63,     0,    65,    66,     0,    42,    67,    68,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,    64,    40,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,     0,    42,
      67,    68,     0,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,    39,    54,    55,    56,
       0,     0,    57,     0,    40,     0,    58,    59,    60,    61,
      62,    63,     0,   186,    66,     0,    42,     0,    68,     0,
      43,     0,    44,    45,    46,    47,    48,    49,     0,    50,
      51,    52,    53,   452,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,     0,    40,   227,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    42,     0,
       0,    67,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,    39,    54,    55,    56,     0,
       0,    57,     0,    40,   227,    58,    59,    60,    61,    62,
      63,     0,     0,     0,    65,    42,     0,     0,    67,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,     0,
      40,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   176,   177,   178,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,    65,   113,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   319,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,     0,     0,     0,     0,    39,   697,
       0,     0,     0,     0,     0,     0,    40,     0,     0,     0,
       0,   698,   589,     0,     0,     0,     0,     0,    42,     0,
     590,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     186,    50,    51,    52,    53,    39,    54,    55,    56,     0,
       0,    57,     0,    40,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,    39,    54,    55,    56,     0,     0,    57,     0,
      40,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,    42,     0,     0,     0,     0,     0,    44,    45,
      46,   176,   177,   178,     0,     0,     0,    52,    53,     0,
      54,    55,    56,     0,     0,    57,    65,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   588,   589,     0,     0,     0,     0,
       0,     0,     0,   590,     0,     0,     0,     0,    39,     0,
       0,     0,     0,   186,     0,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,   756,   589,     0,    44,    45,    46,   176,   177,   178,
     590,    39,     0,    52,    53,     0,    54,    55,    56,    40,
     186,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     176,   177,   178,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,    39,     0,     0,     0,     0,     0,
       0,     0,    40,     0,     0,     0,     0,     0,   589,     0,
       0,     0,     0,     0,    42,     0,   590,     0,     0,     0,
      44,    45,    46,   176,   177,   178,   186,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,   590,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   186,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   186
  };

  const short
  parser::yycheck_[] =
  {
      88,    29,   105,   112,    88,    88,   190,   105,   110,   111,
      64,    99,   316,   179,   180,    99,   278,   230,   205,   166,
      99,   101,   215,   182,   284,   132,   192,    34,   110,   555,
      41,   211,   212,     1,   555,   219,   282,   183,   184,   633,
     259,   183,   634,   349,   467,     5,     1,   441,     1,     1,
       1,   643,    64,   471,     1,   201,   705,   111,    12,   201,
     290,   554,   361,   276,   210,    21,     1,    80,   175,   705,
     478,   173,    83,    19,   312,   184,   185,    27,   297,   102,
      19,   111,    77,   106,   107,    27,   109,   119,   480,   108,
     105,   179,   180,    90,     0,   183,   184,    77,    81,   111,
     104,    34,   106,   118,   192,   497,   119,   699,   516,    99,
     142,   134,   200,   201,   133,    77,   200,   123,   101,   173,
     132,   127,   210,   515,   119,    71,   272,   215,   216,   295,
     368,   215,   216,    77,    84,   281,   164,   134,   166,   107,
     247,   736,    84,   173,   382,   638,   406,   446,   104,   798,
     118,   574,   107,   133,   107,   107,   107,   119,   112,   577,
     107,   173,   798,   175,   800,   118,   118,   118,   560,   108,
     316,   118,   107,   227,   404,   182,   243,   110,   111,   243,
      80,   589,   590,   118,   272,   780,    65,    66,   354,   133,
      80,    80,   280,   281,   413,    78,   280,   280,    80,    80,
      80,    84,   348,    79,   211,   212,   105,   295,   168,   169,
     200,    80,    80,   118,   237,   227,   239,    93,   285,   119,
      96,   285,   276,   105,   632,   215,   216,   103,   316,   119,
     119,   825,    14,   471,   826,   247,   119,   829,   119,   119,
     173,   112,   112,   389,   120,   549,   550,   389,   124,   125,
     119,   119,   259,   123,    22,   133,   112,   127,   101,   347,
     348,   349,   350,   347,    79,   319,   354,   123,   662,   115,
     113,   127,   105,   119,   381,   282,   299,   523,   211,   212,
     105,    29,   362,    51,    52,   118,    93,   813,    27,    96,
     297,   298,   813,   118,   702,   398,   103,    77,   113,   387,
     398,   389,   313,   410,   119,   407,   566,   409,    77,   388,
     125,   495,   618,   105,   416,   462,   418,   124,   477,   252,
     109,   349,    80,    81,   427,   407,   118,   409,   736,   475,
     119,   511,   726,    79,   416,   481,   418,   729,   484,   731,
     553,    24,   484,   735,   112,   583,   434,    93,   105,   105,
     434,   434,   118,   572,   500,   123,   104,   347,   500,   127,
     350,   118,   118,   115,   624,   603,   115,   119,   514,   381,
     119,    87,   780,   115,   120,   783,    92,   119,   124,   125,
     108,   113,   128,   129,   112,   112,   114,   475,   113,   118,
     478,   695,   480,   481,   119,   123,   484,    37,   410,   659,
      79,    80,   113,   549,   550,   598,   413,   113,   119,   497,
      80,    81,   500,   119,    93,   112,   164,    96,   166,   113,
     427,   530,   531,   610,   103,   119,   514,   515,   516,    80,
      81,   611,   612,    93,   462,   101,    96,   108,   104,   467,
     746,   120,   588,   103,   758,   124,   125,   761,   686,   128,
     129,   104,   108,   106,    14,   104,   112,   106,   114,    80,
      81,   549,   550,   104,   124,   106,   134,   123,   128,   571,
     477,   127,   560,    87,   407,   641,   409,   225,   671,   133,
     742,   488,   489,   416,   790,   418,   202,   203,   104,   571,
     106,    79,   705,    81,   113,   104,    84,   106,   113,   553,
     588,   589,   590,   722,   511,    93,   812,    84,    96,    86,
     598,   104,   113,   106,   598,   103,   523,   113,   822,    72,
      73,    74,    75,   120,   121,   273,   274,   765,   113,   104,
     618,   106,   120,     4,    23,   119,   124,   125,   118,   705,
     128,   129,    27,    91,   632,   691,   574,   635,   741,   695,
     298,   635,    79,   641,    77,   488,   489,   795,   796,    77,
      77,    77,    15,   570,   744,   572,    93,   283,   284,    96,
     754,    81,    41,   109,   109,   119,   103,    71,   511,   113,
      79,   113,   670,   671,   672,   798,   670,   671,   672,   113,
     736,   133,   758,   120,    93,   761,   105,   124,   125,   113,
     113,   128,   129,   691,   611,   612,   109,   695,   598,   697,
     698,   119,   105,   697,   702,   698,   644,   705,   119,   721,
      80,    91,   113,   113,   817,   124,   125,   715,    80,   128,
     129,   715,   798,   119,   800,   113,   113,   783,   571,   721,
     119,   729,    85,   731,    85,   635,    25,   735,   736,    27,
      81,   133,   109,   741,   134,    77,   759,   741,   746,   747,
     748,   764,    77,   747,   748,   113,   764,   383,    77,   676,
     758,   120,    77,   761,   783,   120,   822,    80,   611,   612,
     670,   671,   672,   771,   400,    80,   119,   771,   716,   405,
     406,   407,   780,    27,    81,   783,   119,   105,    81,   120,
     788,   119,   790,   101,   788,   788,   112,   697,   698,   797,
     798,   101,   800,   797,   462,   722,    79,   805,   120,   467,
     113,   805,    79,    86,   812,   715,   120,   109,    79,   817,
      93,    84,   109,   817,   822,    86,    93,   744,   119,    96,
      91,    78,    93,   676,   120,   120,   103,    91,    77,    28,
     113,   741,    27,     9,   470,   471,   119,   747,   748,   113,
     105,   124,   125,   118,   113,   128,   129,   124,   125,   120,
      37,   128,   129,   124,   125,    27,    80,   128,   129,    84,
     113,   771,    80,    11,    19,    86,    86,   503,   721,   505,
      53,   507,    91,   123,    91,   119,   113,   105,   113,    84,
      84,   123,   105,   164,   812,     4,   732,   797,   748,   682,
      34,   744,   734,    12,   280,   805,   788,   676,   612,   744,
     489,   212,   570,    15,   691,    24,   574,   817,   550,    28,
     272,    30,    31,    32,    33,    34,    35,   201,    37,    38,
      39,    40,   805,    42,    43,    44,   710,   758,    47,   565,
     566,   516,    51,    52,    53,    54,    55,    56,   703,   802,
     477,   577,   646,   716,    34,   108,     3,     4,     5,     6,
       7,     8,   659,    10,   287,    12,    13,   658,   237,    16,
      17,    18,    19,    20,   662,    22,   570,    24,    -1,    26,
     203,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   764,    42,    43,    44,   624,   625,
      47,   505,   247,    50,    51,    52,    53,    54,    55,    56,
      57,   553,   398,   122,   123,   397,    63,   126,   127,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    76,
      86,    -1,   658,   659,   660,    82,    -1,    93,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,    -1,   120,   112,    -1,   114,   124,   125,
      -1,    -1,   128,   129,    -1,   122,   123,    -1,    -1,   126,
     127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,
      -1,   138,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
      -1,     7,    -1,   739,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    22,    -1,    24,    -1,
      26,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,   775,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,    -1,    -1,    -1,    -1,   143,   144,     3,
       4,     5,    -1,     7,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      24,    -1,    26,    -1,    28,    -1,    30,    31,    32,    33,
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
     134,   135,    -1,    -1,   138,     3,     4,     5,    -1,   143,
     144,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,
       4,    -1,    -1,    -1,   132,   133,   134,   135,    12,    -1,
     138,    -1,    -1,    -1,    -1,   143,   144,    21,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,   112,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,
      -1,   112,    -1,   114,    -1,    -1,    -1,   118,    -1,    -1,
      -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,
      -1,   132,   133,   134,   135,    -1,    -1,   138,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    94,
      -1,    96,    -1,    -1,    -1,    -1,    -1,   102,   103,    -1,
      -1,    -1,    -1,   108,    -1,    -1,    -1,   112,   113,   114,
      -1,    -1,    -1,    -1,   119,   120,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,    -1,    -1,   132,   133,   134,
     135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    -1,    94,    -1,    96,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,   108,
     109,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,   120,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,    -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,    -1,    -1,   132,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,
      -1,   118,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,
     127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,
      -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    46,    47,    -1,    -1,    50,
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
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,
      -1,    -1,    -1,   118,    -1,    -1,    -1,   122,   123,    -1,
      -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,
     135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
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
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
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
      -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,
     127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,
      -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,
      -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,
      -1,   132,   133,   134,   135,    -1,    -1,   138,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    86,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,
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
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,   112,    -1,   114,    -1,     4,    -1,    -1,
      -1,    -1,    -1,   122,   123,    12,    -1,   126,   127,    -1,
      -1,    -1,    -1,   132,   133,   134,   135,    24,    -1,   138,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    86,
      -1,    -1,    89,    -1,    -1,    -1,    93,    94,    -1,    96,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,     4,    -1,   112,   113,   114,    -1,    -1,
      -1,    12,   119,    -1,    -1,   122,   123,   124,   125,    -1,
     127,   128,   129,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,     4,    -1,
      -1,   112,    -1,   114,    -1,    -1,    12,    -1,    -1,   120,
      -1,   122,   123,    -1,   125,    -1,   127,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,     4,    -1,    -1,   112,    -1,   114,    -1,
      -1,    12,    -1,    -1,   120,    -1,   122,   123,    19,   125,
      -1,   127,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    -1,    39,    40,
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
      -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,   109,
      -1,    -1,   112,    12,   114,    -1,    -1,    -1,    -1,    -1,
      19,    -1,   122,   123,    -1,    24,    -1,   127,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,
      -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,     4,    -1,    -1,   108,    -1,    -1,    -1,   112,
      12,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,    -1,    24,    -1,   127,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   108,     4,    -1,    -1,   112,   113,   114,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   122,   123,    -1,
      -1,    -1,   127,    -1,    24,    -1,    -1,    -1,    28,    -1,
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
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,     4,    -1,    -1,
     112,    -1,   114,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,    21,   127,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,    -1,    12,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    12,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    24,   126,
     127,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,   112,    12,    -1,    51,    52,    53,    54,    55,
      56,    -1,   122,   123,    -1,    24,   126,   127,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    24,
     126,   127,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,   122,   123,    -1,    24,    -1,   127,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    78,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    24,    -1,
      -1,   126,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,   112,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,   122,    24,    -1,    -1,   126,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,   122,   123,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,     4,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   103,   104,    -1,    -1,    -1,    -1,    -1,    24,    -1,
     112,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
     122,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,   122,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,   122,    -1,    -1,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,   103,   104,    -1,    30,    31,    32,    33,    34,    35,
     112,     4,    -1,    39,    40,    -1,    42,    43,    44,    12,
     122,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,    -1,    -1,    24,    -1,   112,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,   122,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   146,   147,   148,   151,   123,   127,   349,
     152,   163,     0,   152,    65,    66,   149,   105,   118,   153,
     164,   165,     1,   107,   348,   108,   133,   216,   216,   112,
     154,    14,   166,   174,   175,   133,   217,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   112,   122,   123,   126,   127,   142,
     155,   156,   157,   161,   315,   318,   319,   332,   334,   335,
     341,    27,    24,   167,   118,   162,     3,     5,     6,     7,
       8,    10,    13,    16,    17,    18,    19,    20,    22,    26,
      29,    36,    50,    57,    63,    76,    82,    89,    92,    94,
     108,   112,   114,   123,   127,   132,   133,   134,   135,   138,
     143,   144,   172,   176,   177,   178,   179,   181,   196,   221,
     261,   266,   270,   271,   273,   274,   275,   276,   304,   305,
     308,   309,   331,   332,   335,   343,   344,   347,   109,   119,
     349,    79,    93,    96,   103,   124,   125,   128,   129,   338,
     339,   340,   342,   113,   119,   142,   112,   158,   104,   106,
     150,   349,   118,   112,   269,   270,    33,    34,    35,    89,
      94,    96,   104,   108,   112,   114,   122,   201,   226,   228,
     230,   231,   233,   237,   313,   314,   318,   328,   330,   341,
      37,   112,   104,   106,   296,   269,    72,    73,    74,    75,
     182,   104,   106,   213,   214,    19,    37,   180,   228,   230,
     314,    14,   296,   274,   108,   267,   268,   112,   332,   108,
     112,   306,   307,   309,   344,   274,   294,   295,   274,   273,
     274,    79,   109,   120,   125,   129,   269,   270,   278,   280,
     281,   311,   325,   327,   337,   338,   340,   345,   346,   102,
     113,   119,   277,   278,   279,   338,   339,   340,   345,   350,
     115,   350,    19,   267,   267,   134,   171,   162,    19,    71,
     203,    80,   119,    81,    84,   120,   263,   264,   265,   311,
     324,   326,   336,   338,   339,    88,   274,   101,   104,    87,
     133,   113,   113,   113,   113,   113,   157,    78,   159,   160,
     161,   152,   152,     4,   168,    23,    80,   237,   237,   112,
     221,   254,   255,   256,   335,    28,   109,   223,   224,   226,
     228,    79,    86,    93,   113,   124,   125,   128,   129,   223,
     241,   320,   321,   350,   350,    27,   207,    91,    86,   120,
     235,   316,   320,   329,    88,   234,   237,   228,   223,   240,
     241,    20,    46,   269,   293,   297,   298,   299,   297,   118,
     272,    77,    77,    77,    77,   219,   224,   238,   212,   261,
     262,   270,   212,    15,   191,   228,   228,    80,   119,    81,
      41,    90,   134,   332,    77,   133,   346,    80,   119,   218,
      86,   294,   334,   343,   324,    78,    84,   119,   109,   119,
     270,   333,   335,   101,   113,   113,   119,   113,   119,   113,
     113,   113,   119,   115,   238,   332,   332,   120,   173,   310,
     322,   323,   339,   346,   203,   133,   201,   220,   224,   225,
     331,   269,   283,   284,   299,   334,    27,   215,   265,   271,
     237,   333,    78,   300,   301,   302,   332,   333,   335,   274,
     113,   113,   119,   105,   348,   349,    12,   112,   169,   170,
     104,   106,   285,   219,   339,    80,   105,   119,   242,   109,
      80,    91,   113,   113,   119,   113,   113,   115,   104,   106,
     206,   228,   224,   318,   330,   230,   237,    80,   198,   113,
     119,   214,   296,    85,   105,   118,   348,    25,    27,   211,
     105,   118,   348,   269,    81,    80,    81,   199,   224,   245,
     112,   314,   223,   133,   134,   109,    77,    77,   113,   108,
     112,   114,   312,   313,   306,    77,   269,   120,   120,   269,
     282,   299,   269,   278,   278,   333,   278,   278,    77,    80,
      80,   335,   344,   119,    28,   202,   226,   228,    77,   133,
      80,    81,   197,   249,   215,    81,   119,   120,   214,   105,
     119,    81,   101,   161,   112,   155,   105,   118,   269,   286,
     287,   288,   292,   286,   348,   113,   224,   256,   103,   104,
     112,   243,   244,   328,   245,   224,   223,     7,    26,   192,
     204,   205,   262,   205,   120,   120,   245,   223,   269,   298,
     269,   104,   106,   210,   262,   224,   245,   243,    84,   185,
     331,   109,   113,   115,   119,    78,   219,   222,   222,   120,
     120,   322,   242,   197,   249,    91,    77,   245,    28,   250,
     251,   252,    27,   246,     9,   257,   258,   259,   269,   299,
     301,   278,   303,   333,   155,   113,   286,   105,   118,    84,
      86,   289,   290,   291,   348,   224,   328,   328,   113,    37,
     193,    19,    37,   191,   228,   105,   118,   348,   272,     7,
      26,   195,   196,   208,   209,   262,   209,   186,   330,    27,
     188,    80,   299,   269,    77,   119,    77,    91,   103,   246,
     257,   228,   242,    84,   227,   232,   236,   237,   253,   104,
     106,   257,    22,    51,    52,   112,   183,   260,   317,   318,
     259,    81,   101,   113,   288,   283,   269,   215,   291,    80,
     105,    80,   228,   191,   228,    80,    81,   200,   204,    11,
      19,   194,   194,   105,   118,   348,    86,   104,   106,   189,
     220,   219,   228,   226,   230,   257,   103,   251,    91,   120,
     236,   310,   247,   248,   272,   247,   113,   228,   229,   239,
     260,    53,   184,   278,   333,    86,   215,   245,   245,   198,
      81,   200,   245,   112,   244,   328,   269,   191,   203,   208,
     187,   330,    78,   190,   191,    78,   190,    91,   232,   253,
     232,   105,   118,   307,   348,   119,   113,   228,   269,   105,
     113,   328,    84,   202,   330,   105,   105,   118,   348,   348,
     228,   248,    80,   239,   186,   197,   249,   191,   219,   246,
     257,   257
  };

  const short
  parser::yyr1_[] =
  {
       0,   145,   146,   147,   147,   148,   149,   149,   149,   150,
     150,   151,   151,   152,   153,   153,   153,   154,   154,   155,
     155,   155,   155,   156,   156,   157,   157,   158,   158,   158,
     159,   159,   160,   160,   161,   161,   162,   162,   163,   163,
     164,   165,   165,   166,   167,   167,   168,   168,   169,   169,
     170,   170,   171,   171,   172,   172,   172,   173,   173,   174,
     175,   175,   176,   176,   176,   176,   176,   176,   176,   176,
     177,   178,   178,   178,   178,   178,   179,   180,   180,   181,
     181,   181,   181,   182,   182,   182,   182,   182,   183,   183,
     183,   184,   185,   185,   186,   187,   187,   188,   188,   189,
     189,   189,   189,   190,   190,   190,   190,   191,   192,   192,
     192,   192,   192,   193,   193,   194,   194,   195,   195,   195,
     196,   196,   197,   197,   198,   198,   199,   199,   199,   200,
     200,   200,   201,   201,   202,   202,   202,   202,   203,   203,
     203,   204,   204,   205,   205,   205,   205,   206,   206,   207,
     207,   208,   208,   209,   209,   209,   209,   210,   210,   211,
     211,   212,   212,   212,   212,   213,   213,   214,   215,   215,
     216,   216,   217,   217,   217,   218,   218,   219,   220,   221,
     221,   222,   222,   223,   223,   224,   224,   224,   225,   226,
     227,   228,   228,   229,   230,   231,   231,   232,   232,   233,
     233,   233,   234,   235,   235,   236,   237,   237,   237,   237,
     237,   237,   237,   237,   237,   237,   238,   239,   239,   240,
     240,   241,   241,   242,   242,   243,   243,   243,   244,   244,
     245,   246,   246,   246,   247,   247,   248,   249,   250,   250,
     251,   251,   252,   252,   253,   253,   254,   254,   255,   255,
     256,   257,   257,   258,   258,   259,   259,   259,   260,   260,
     260,   261,   261,   262,   263,   263,   264,   264,   265,   266,
     266,   266,   266,   266,   266,   266,   266,   266,   267,   267,
     268,   268,   269,   269,   270,   270,   271,   271,   272,   272,
     273,   273,   273,   273,   274,   274,   274,   274,   274,   274,
     274,   274,   274,   274,   275,   275,   275,   276,   276,   276,
     276,   276,   276,   276,   276,   277,   277,   278,   278,   278,
     279,   279,   280,   280,   280,   280,   280,   280,   280,   281,
     281,   282,   282,   283,   284,   284,   285,   285,   285,   285,
     286,   286,   287,   287,   287,   288,   289,   289,   290,   290,
     291,   292,   293,   294,   295,   295,   296,   296,   297,   297,
     297,   297,   298,   298,   299,   299,   299,   300,   300,   301,
     301,   301,   302,   302,   302,   302,   303,   303,   304,   304,
     305,   305,   306,   306,   306,   307,   307,   308,   308,   308,
     308,   309,   309,   310,   310,   311,   311,   312,   312,   312,
     313,   313,   313,   313,   313,   314,   314,   315,   315,   315,
     315,   316,   316,   317,   318,   318,   319,   320,   320,   320,
     321,   321,   321,   321,   322,   322,   323,   323,   324,   324,
     325,   325,   326,   326,   327,   327,   328,   329,   330,   330,
     330,   330,   330,   331,   331,   332,   332,   332,   333,   334,
     334,   335,   335,   335,   335,   335,   335,   335,   335,   336,
     336,   337,   337,   338,   339,   339,   340,   340,   341,   341,
     341,   341,   341,   341,   341,   341,   341,   341,   341,   341,
     341,   341,   341,   341,   341,   341,   342,   342,   343,   343,
     344,   345,   345,   346,   346,   347,   347,   347,   347,   347,
     348,   348,   349,   349,   350,   350
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       0,     2,     1,     3,     1,     2,     2,     0,     3,     3,
       0,     1,     3,     1,     1,     1,     2,     1,     2,     0,
       2,     3,     0,     5,     1,     0,     2,     0,     1,     0,
       3,     4,     0,     1,     1,     1,     1,     3,     1,     2,
       3,     0,     1,     1,     1,     1,     4,     7,     1,     1,
       3,     4,     5,     6,     6,     4,     4,     3,     1,     4,
       3,     6,     7,     2,     2,     2,     2,     0,     1,     1,
       1,     2,     0,     2,     3,     2,     1,     0,     2,     3,
       3,     3,     3,     3,     2,     1,     0,     3,     4,     3,
       4,     2,     3,     0,     1,     0,     1,     3,     6,     7,
       1,     1,     0,     2,     0,     2,     0,     2,     2,     0,
       2,     4,     3,     1,     6,     4,     3,     1,     4,     3,
       0,     1,     1,     3,     2,     1,     0,     3,     3,     2,
       0,     1,     1,     3,     2,     1,     0,     3,     3,     2,
       0,     3,     2,     1,     0,     3,     3,     1,     2,     0,
       1,     3,     3,     1,     0,     0,     2,     1,     1,     3,
       1,     1,     3,     1,     3,     4,     3,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     3,     1,     2,     1,
       2,     3,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     3,     2,     5,     3,     3,     1,     1,     3,     1,
       0,     1,     3,     2,     0,     1,     3,     5,     1,     5,
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
     563,   564,   565,   567,   568,   570,   571,   573,   574,   575,
     577,   578,   580,   581,   583,   584,   588,   589,   591,   592,
     594,   596,   597,   599,   612,   613,   615,   616,   618,   619,
     623,   624,   629,   630,   632,   633,   634,   636,   637,   641,
     643,   644,   646,   647,   648,   649,   652,   653,   659,   661,
     664,   667,   668,   670,   671,   672,   674,   676,   677,   680,
     681,   682,   688,   695,   696,   697,   698,   699,   701,   702,
     703,   705,   716,   717,   719,   721,   722,   726,   727,   729,
     730,   731,   732,   734,   735,   736,   737,   739,   742,   744,
     746,   748,   749,   751,   751,   753,   753,   757,   759,   766,
     773,   774,   778,   779,   781,   782,   784,   785,   786,   788,
     789,   790,   793,   794,   797,   798,   799,   800,   802,   803,
     804,   846,   847,   849,   850,   851,   852,   854,   855,   857,
     858,   860,   861,   863,   864,   865,   866,   868,   869,   871,
     872,   875,   876,   877,   878,   880,   881,   883,   885,   886,
     894,   895,   897,   898,   899,   912,   913,   922,   924,   926,
     927,   929,   930,   939,   940,   942,   943,   945,   947,   956,
     958,   960,   961,   963,   966,   968,   969,   971,   972,   974,
     976,   977,   979,   981,   982,   989,   996,   997,   998,   999,
    1000,  1001,  1002,  1003,  1009,  1010,  1013,  1015,  1016,  1018,
    1019,  1021,  1022,  1029,  1030,  1032,  1033,  1034,  1037,  1038,
    1056,  1062,  1063,  1064,  1066,  1067,  1069,  1071,  1073,  1074,
    1076,  1077,  1079,  1080,  1082,  1083,  1089,  1090,  1092,  1093,
    1095,  1097,  1098,  1100,  1101,  1103,  1104,  1105,  1107,  1108,
    1109,  1114,  1116,  1118,  1122,  1123,  1125,  1126,  1130,  1140,
    1141,  1143,  1144,  1145,  1146,  1147,  1148,  1149,  1152,  1153,
    1155,  1156,  1161,  1162,  1166,  1167,  1169,  1170,  1172,  1173,
    1178,  1179,  1180,  1181,  1184,  1185,  1186,  1187,  1188,  1190,
    1192,  1193,  1194,  1196,  1199,  1200,  1201,  1204,  1205,  1206,
    1207,  1208,  1209,  1214,  1215,  1218,  1219,  1224,  1225,  1226,
    1231,  1232,  1250,  1251,  1252,  1253,  1254,  1255,  1256,  1258,
    1259,  1272,  1274,  1284,  1286,  1287,  1290,  1291,  1292,  1293,
    1295,  1296,  1298,  1299,  1300,  1302,  1304,  1305,  1307,  1308,
    1317,  1319,  1321,  1323,  1325,  1326,  1329,  1330,  1332,  1333,
    1334,  1335,  1340,  1341,  1343,  1344,  1345,  1350,  1351,  1353,
    1354,  1355,  1357,  1358,  1359,  1360,  1363,  1364,  1396,  1397,
    1399,  1400,  1402,  1403,  1404,  1406,  1407,  1409,  1410,  1411,
    1412,  1414,  1415,  1417,  1418,  1420,  1421,  1424,  1425,  1426,
    1428,  1429,  1430,  1431,  1432,  1434,  1435,  1437,  1438,  1439,
    1440,  1443,  1444,  1446,  1448,  1449,  1453,  1455,  1456,  1457,
    1459,  1460,  1461,  1462,  1467,  1468,  1470,  1471,  1473,  1474,
    1477,  1478,  1483,  1484,  1486,  1487,  1491,  1493,  1495,  1496,
    1497,  1498,  1499,  1502,  1503,  1505,  1506,  1507,  1509,  1511,
    1512,  1514,  1515,  1516,  1517,  1518,  1519,  1520,  1521,  1523,
    1524,  1526,  1527,  1529,  1531,  1532,  1534,  1535,  1537,  1538,
    1539,  1540,  1541,  1542,  1543,  1544,  1545,  1546,  1547,  1548,
    1549,  1550,  1551,  1552,  1553,  1554,  1556,  1557,  1561,  1562,
    1564,  1566,  1567,  1569,  1570,  1574,  1575,  1576,  1577,  1578,
    1583,  1586,  1590,  1591,  1593,  1594
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
#line 7288 "parser.cc"

#line 1603 "parser.y"


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

