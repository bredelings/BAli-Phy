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
#line 515 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2521 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 532 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2527 "parser.cc"
    break;

  case 4: // module: body2
#line 533 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2533 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 535 "parser.y"
                                                                 {drv.push_module_context();}
#line 2539 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 543 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2545 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 544 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2551 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 546 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2557 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 547 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2563 "parser.cc"
    break;

  case 13: // top: semis top1
#line 550 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2569 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 552 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2575 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 553 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2581 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 554 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2587 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 562 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2593 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 563 "parser.y"
                                      {}
#line 2599 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 565 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2605 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 566 "parser.y"
                                      {}
#line 2611 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 567 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2617 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 568 "parser.y"
                                      {}
#line 2623 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 570 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2629 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2635 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 573 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2641 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 574 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2647 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 575 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2653 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 577 "parser.y"
                                      {}
#line 2659 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 578 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2665 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 579 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2671 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 581 "parser.y"
                   {}
#line 2677 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 582 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2683 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 584 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2689 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 585 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2695 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 587 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2701 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 588 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2707 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 598 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2713 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 600 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2719 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 601 "parser.y"
                         { }
#line 2725 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 603 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2733 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 616 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2739 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 617 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2745 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 619 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2751 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 620 "parser.y"
                               { }
#line 2757 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 622 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2763 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 623 "parser.y"
                               { }
#line 2769 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 627 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2775 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 628 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2781 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 630 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2787 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 631 "parser.y"
                                      {}
#line 2793 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 632 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2799 "parser.cc"
    break;

  case 56: // importlist: ','
#line 633 "parser.y"
                                      {}
#line 2805 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 635 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2811 "parser.cc"
    break;

  case 58: // importlist1: import
#line 636 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2817 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 638 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2823 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 639 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2829 "parser.cc"
    break;

  case 61: // prec: %empty
#line 644 "parser.y"
                   { }
#line 2835 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 645 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2841 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 647 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2847 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 648 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2853 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 649 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2859 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 651 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2865 "parser.cc"
    break;

  case 67: // ops: op
#line 652 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2871 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 656 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2877 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 658 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2883 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 659 "parser.y"
                                            { }
#line 2889 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 661 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2895 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 662 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2901 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 663 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2907 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 664 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2913 "parser.cc"
    break;

  case 75: // topdecl: "default" "(" comma_types0 ")"
#line 667 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2919 "parser.cc"
    break;

  case 76: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 668 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2925 "parser.cc"
    break;

  case 77: // topdecl: decl_no_th
#line 674 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2931 "parser.cc"
    break;

  case 78: // topdecl: infixexp
#line 676 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2937 "parser.cc"
    break;

  case 79: // cl_decl: "class" tycl_hdr where_cls
#line 679 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2943 "parser.cc"
    break;

  case 80: // ty_decl: "type" type "=" ktype
#line 682 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2949 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 683 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2955 "parser.cc"
    break;

  case 82: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 685 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 2961 "parser.cc"
    break;

  case 83: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 686 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2967 "parser.cc"
    break;

  case 84: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 687 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 2973 "parser.cc"
    break;

  case 85: // standalone_kind_sig: "type" sks_vars "::" kind
#line 689 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 2979 "parser.cc"
    break;

  case 86: // sks_vars: sks_vars "," oqtycon
#line 691 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2985 "parser.cc"
    break;

  case 87: // sks_vars: oqtycon
#line 692 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2991 "parser.cc"
    break;

  case 88: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 695 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2997 "parser.cc"
    break;

  case 89: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 696 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3003 "parser.cc"
    break;

  case 90: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 698 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 3013 "parser.cc"
    break;

  case 91: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 704 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 3023 "parser.cc"
    break;

  case 92: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 710 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3029 "parser.cc"
    break;

  case 93: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 711 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3035 "parser.cc"
    break;

  case 94: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 712 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3041 "parser.cc"
    break;

  case 95: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 713 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3047 "parser.cc"
    break;

  case 96: // overlap_pragma: %empty
#line 714 "parser.y"
                                               {}
#line 3053 "parser.cc"
    break;

  case 106: // where_type_family: %empty
#line 741 "parser.y"
                                                           {}
#line 3059 "parser.cc"
    break;

  case 107: // where_type_family: "where" ty_fam_inst_eqn_list
#line 742 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3065 "parser.cc"
    break;

  case 108: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 744 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3071 "parser.cc"
    break;

  case 109: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 745 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3077 "parser.cc"
    break;

  case 110: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 746 "parser.y"
                                                           {}
#line 3083 "parser.cc"
    break;

  case 111: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 747 "parser.y"
                                                           {}
#line 3089 "parser.cc"
    break;

  case 112: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 749 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3095 "parser.cc"
    break;

  case 113: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 750 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3101 "parser.cc"
    break;

  case 114: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 751 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3107 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqns: %empty
#line 752 "parser.y"
                                                           {}
#line 3113 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn: type "=" ctype
#line 754 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3119 "parser.cc"
    break;

  case 117: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 757 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3125 "parser.cc"
    break;

  case 118: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 759 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3131 "parser.cc"
    break;

  case 119: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 761 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3137 "parser.cc"
    break;

  case 120: // at_decl_cls: "type" ty_fam_inst_eqn
#line 763 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3143 "parser.cc"
    break;

  case 121: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 764 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3149 "parser.cc"
    break;

  case 126: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 772 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3155 "parser.cc"
    break;

  case 127: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 775 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3165 "parser.cc"
    break;

  case 128: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 782 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3175 "parser.cc"
    break;

  case 129: // data_or_newtype: "data"
#line 788 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3181 "parser.cc"
    break;

  case 130: // data_or_newtype: "newtype"
#line 789 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3187 "parser.cc"
    break;

  case 131: // opt_kind_sig: %empty
#line 793 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3193 "parser.cc"
    break;

  case 132: // opt_kind_sig: "::" kind
#line 794 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3199 "parser.cc"
    break;

  case 133: // opt_datafam_kind_sig: %empty
#line 796 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3205 "parser.cc"
    break;

  case 134: // opt_datafam_kind_sig: "::" kind
#line 797 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3211 "parser.cc"
    break;

  case 135: // opt_tyfam_kind_sig: %empty
#line 799 "parser.y"
                                      {}
#line 3217 "parser.cc"
    break;

  case 136: // opt_tyfam_kind_sig: "::" kind
#line 800 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3223 "parser.cc"
    break;

  case 137: // opt_tyfam_kind_sig: "=" tv_bndr
#line 801 "parser.y"
                                      {}
#line 3229 "parser.cc"
    break;

  case 138: // opt_at_kind_inj_sig: %empty
#line 803 "parser.y"
                                      {}
#line 3235 "parser.cc"
    break;

  case 139: // opt_at_kind_inj_sig: "::" kind
#line 804 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3241 "parser.cc"
    break;

  case 140: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 805 "parser.y"
                                                                  {}
#line 3247 "parser.cc"
    break;

  case 141: // tycl_hdr: context "=>" type
#line 808 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3253 "parser.cc"
    break;

  case 142: // tycl_hdr: type
#line 809 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3259 "parser.cc"
    break;

  case 143: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 812 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3265 "parser.cc"
    break;

  case 144: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 813 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3271 "parser.cc"
    break;

  case 145: // datafam_inst_hdr: context "=>" type
#line 814 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3277 "parser.cc"
    break;

  case 146: // datafam_inst_hdr: type
#line 815 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3283 "parser.cc"
    break;

  case 150: // decl_cls: at_decl_cls
#line 861 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3289 "parser.cc"
    break;

  case 151: // decl_cls: decl
#line 862 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3295 "parser.cc"
    break;

  case 152: // decls_cls: decls_cls ";" decl_cls
#line 864 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3301 "parser.cc"
    break;

  case 153: // decls_cls: decls_cls ";"
#line 865 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3307 "parser.cc"
    break;

  case 154: // decls_cls: decl_cls
#line 866 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3313 "parser.cc"
    break;

  case 155: // decls_cls: %empty
#line 867 "parser.y"
                                           {}
#line 3319 "parser.cc"
    break;

  case 156: // decllist_cls: "{" decls_cls "}"
#line 869 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3325 "parser.cc"
    break;

  case 157: // decllist_cls: "vocurly" decls_cls close
#line 870 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3331 "parser.cc"
    break;

  case 158: // where_cls: "where" decllist_cls
#line 872 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3337 "parser.cc"
    break;

  case 159: // where_cls: %empty
#line 873 "parser.y"
                                           {}
#line 3343 "parser.cc"
    break;

  case 160: // decl_inst: at_decl_inst
#line 875 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3349 "parser.cc"
    break;

  case 161: // decl_inst: decl
#line 876 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3355 "parser.cc"
    break;

  case 162: // decls_inst: decls_inst ";" decl_inst
#line 878 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3361 "parser.cc"
    break;

  case 163: // decls_inst: decls_inst ";"
#line 879 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3367 "parser.cc"
    break;

  case 164: // decls_inst: decl_inst
#line 880 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3373 "parser.cc"
    break;

  case 165: // decls_inst: %empty
#line 881 "parser.y"
                                           {}
#line 3379 "parser.cc"
    break;

  case 166: // decllist_inst: "{" decls_inst "}"
#line 883 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3385 "parser.cc"
    break;

  case 167: // decllist_inst: "vocurly" decls_inst close
#line 884 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3391 "parser.cc"
    break;

  case 168: // where_inst: "where" decllist_inst
#line 886 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3397 "parser.cc"
    break;

  case 169: // where_inst: %empty
#line 887 "parser.y"
                                           {}
#line 3403 "parser.cc"
    break;

  case 170: // decls: decls ";" decl
#line 890 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3409 "parser.cc"
    break;

  case 171: // decls: decls ";"
#line 891 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3415 "parser.cc"
    break;

  case 172: // decls: decl
#line 892 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3421 "parser.cc"
    break;

  case 173: // decls: %empty
#line 893 "parser.y"
                        {}
#line 3427 "parser.cc"
    break;

  case 174: // decllist: "{" decls "}"
#line 895 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3433 "parser.cc"
    break;

  case 175: // decllist: "vocurly" decls close
#line 896 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3439 "parser.cc"
    break;

  case 176: // binds: decllist
#line 898 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3445 "parser.cc"
    break;

  case 177: // wherebinds: "where" binds
#line 900 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3451 "parser.cc"
    break;

  case 178: // wherebinds: %empty
#line 901 "parser.y"
                                 {}
#line 3457 "parser.cc"
    break;

  case 184: // opt_tyconsig: %empty
#line 927 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3463 "parser.cc"
    break;

  case 185: // opt_tyconsig: "::" gtycon
#line 928 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3469 "parser.cc"
    break;

  case 186: // sigtype: ctype
#line 937 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3475 "parser.cc"
    break;

  case 187: // sigtypedoc: ctypedoc
#line 939 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3481 "parser.cc"
    break;

  case 188: // sig_vars: sig_vars "," var
#line 941 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3487 "parser.cc"
    break;

  case 189: // sig_vars: var
#line 942 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3493 "parser.cc"
    break;

  case 190: // sigtypes1: sigtype
#line 944 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3499 "parser.cc"
    break;

  case 191: // sigtypes1: sigtypes1 "," sigtype
#line 945 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3505 "parser.cc"
    break;

  case 192: // ktype: ctype
#line 954 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3511 "parser.cc"
    break;

  case 193: // ktype: ctype "::" kind
#line 955 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3517 "parser.cc"
    break;

  case 194: // ctype: "forall" tv_bndrs "." ctype
#line 957 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3523 "parser.cc"
    break;

  case 195: // ctype: context "=>" ctype
#line 958 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3529 "parser.cc"
    break;

  case 196: // ctype: type
#line 960 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3535 "parser.cc"
    break;

  case 197: // ctypedoc: ctype
#line 962 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3541 "parser.cc"
    break;

  case 198: // context: btype
#line 971 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3547 "parser.cc"
    break;

  case 199: // context_no_ops: btype_no_ops
#line 973 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3553 "parser.cc"
    break;

  case 200: // type: btype
#line 975 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3559 "parser.cc"
    break;

  case 201: // type: btype "->" ctype
#line 976 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3565 "parser.cc"
    break;

  case 202: // typedoc: type
#line 978 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3571 "parser.cc"
    break;

  case 203: // btype: infixtype
#line 981 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3577 "parser.cc"
    break;

  case 204: // infixtype: ftype
#line 983 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3583 "parser.cc"
    break;

  case 205: // infixtype: btype tyop btype
#line 984 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3589 "parser.cc"
    break;

  case 206: // btype_no_ops: atype_docs
#line 986 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3595 "parser.cc"
    break;

  case 207: // btype_no_ops: btype_no_ops atype_docs
#line 987 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3601 "parser.cc"
    break;

  case 208: // ftype: atype
#line 989 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3607 "parser.cc"
    break;

  case 209: // ftype: ftype tyarg
#line 991 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3613 "parser.cc"
    break;

  case 210: // ftype: ftype "@" atype
#line 992 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3619 "parser.cc"
    break;

  case 211: // tyarg: atype
#line 994 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3625 "parser.cc"
    break;

  case 212: // tyop: qtyconop
#line 996 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3631 "parser.cc"
    break;

  case 213: // tyop: tyvarop
#line 997 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3637 "parser.cc"
    break;

  case 214: // atype_docs: atype
#line 1004 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3643 "parser.cc"
    break;

  case 215: // atype: ntgtycon
#line 1011 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3649 "parser.cc"
    break;

  case 216: // atype: tyvar
#line 1012 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3655 "parser.cc"
    break;

  case 217: // atype: "*"
#line 1013 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3661 "parser.cc"
    break;

  case 218: // atype: PREFIX_BANG atype
#line 1014 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3667 "parser.cc"
    break;

  case 219: // atype: PREFIX_TILDE atype
#line 1015 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3673 "parser.cc"
    break;

  case 220: // atype: "{" fielddecls "}"
#line 1016 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3679 "parser.cc"
    break;

  case 221: // atype: "(" ")"
#line 1017 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3685 "parser.cc"
    break;

  case 222: // atype: "(" comma_types1 "," ktype ")"
#line 1018 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3691 "parser.cc"
    break;

  case 223: // atype: "[" ktype "]"
#line 1024 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3697 "parser.cc"
    break;

  case 224: // atype: "(" ktype ")"
#line 1025 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3703 "parser.cc"
    break;

  case 225: // inst_type: sigtype
#line 1028 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3709 "parser.cc"
    break;

  case 228: // comma_types0: comma_types1
#line 1033 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3715 "parser.cc"
    break;

  case 229: // comma_types0: %empty
#line 1034 "parser.y"
                                       { /* default construction OK */ }
#line 3721 "parser.cc"
    break;

  case 230: // comma_types1: ktype
#line 1036 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3727 "parser.cc"
    break;

  case 231: // comma_types1: comma_types1 "," ktype
#line 1037 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3733 "parser.cc"
    break;

  case 232: // tv_bndrs: tv_bndrs tv_bndr
#line 1044 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3739 "parser.cc"
    break;

  case 233: // tv_bndrs: %empty
#line 1045 "parser.y"
                               { /* default construction OK */}
#line 3745 "parser.cc"
    break;

  case 234: // tv_bndr: tv_bndr_no_braces
#line 1047 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3751 "parser.cc"
    break;

  case 235: // tv_bndr: "{" tyvar "}"
#line 1048 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3757 "parser.cc"
    break;

  case 236: // tv_bndr: "{" tyvar "::" kind "}"
#line 1049 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3763 "parser.cc"
    break;

  case 237: // tv_bndr_no_braces: tyvar
#line 1052 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3769 "parser.cc"
    break;

  case 238: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1053 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3775 "parser.cc"
    break;

  case 239: // kind: ctype
#line 1071 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3781 "parser.cc"
    break;

  case 240: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1077 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3787 "parser.cc"
    break;

  case 241: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1078 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3793 "parser.cc"
    break;

  case 242: // gadt_constrlist: %empty
#line 1079 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3799 "parser.cc"
    break;

  case 243: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1081 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3805 "parser.cc"
    break;

  case 244: // gadt_constrs: gadt_constr
#line 1082 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3811 "parser.cc"
    break;

  case 245: // gadt_constr: optSemi con_list "::" sigtype
#line 1084 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3817 "parser.cc"
    break;

  case 246: // constrs: "=" constrs1
#line 1086 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3823 "parser.cc"
    break;

  case 247: // constrs1: constrs1 "|" constr
#line 1088 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3829 "parser.cc"
    break;

  case 248: // constrs1: constr
#line 1089 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3835 "parser.cc"
    break;

  case 249: // constr: forall context_no_ops "=>" constr_stuff
#line 1091 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3841 "parser.cc"
    break;

  case 250: // constr: forall constr_stuff
#line 1092 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3847 "parser.cc"
    break;

  case 251: // forall: "forall" tv_bndrs "."
#line 1094 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3853 "parser.cc"
    break;

  case 252: // forall: %empty
#line 1095 "parser.y"
                                {}
#line 3859 "parser.cc"
    break;

  case 253: // constr_stuff: btype_no_ops
#line 1097 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3865 "parser.cc"
    break;

  case 254: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1098 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3875 "parser.cc"
    break;

  case 255: // fielddecls: %empty
#line 1104 "parser.y"
                                {}
#line 3881 "parser.cc"
    break;

  case 256: // fielddecls: fielddecls1
#line 1105 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3887 "parser.cc"
    break;

  case 257: // fielddecls1: fielddecls1 "," fielddecl
#line 1107 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3893 "parser.cc"
    break;

  case 258: // fielddecls1: fielddecl
#line 1108 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3899 "parser.cc"
    break;

  case 259: // fielddecl: sig_vars "::" ctype
#line 1110 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3905 "parser.cc"
    break;

  case 270: // decl_no_th: sigdecl
#line 1129 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3911 "parser.cc"
    break;

  case 271: // decl_no_th: infixexp rhs
#line 1131 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3917 "parser.cc"
    break;

  case 272: // decl: decl_no_th
#line 1133 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3923 "parser.cc"
    break;

  case 273: // rhs: "=" exp wherebinds
#line 1137 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3929 "parser.cc"
    break;

  case 274: // rhs: gdrhs wherebinds
#line 1138 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3935 "parser.cc"
    break;

  case 275: // gdrhs: gdrhs gdrh
#line 1140 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3941 "parser.cc"
    break;

  case 276: // gdrhs: gdrh
#line 1141 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3947 "parser.cc"
    break;

  case 277: // gdrh: "|" guardquals "=" exp
#line 1145 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3953 "parser.cc"
    break;

  case 278: // sigdecl: sig_vars "::" sigtypedoc
#line 1155 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3959 "parser.cc"
    break;

  case 279: // sigdecl: infix prec ops
#line 1156 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3965 "parser.cc"
    break;

  case 280: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1158 "parser.y"
                                                    {}
#line 3971 "parser.cc"
    break;

  case 281: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1159 "parser.y"
                                            {}
#line 3977 "parser.cc"
    break;

  case 282: // sigdecl: "{-# SCC" qvar "#-}"
#line 1160 "parser.y"
                              {}
#line 3983 "parser.cc"
    break;

  case 283: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1161 "parser.y"
                                     {}
#line 3989 "parser.cc"
    break;

  case 284: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1162 "parser.y"
                                                               {}
#line 3995 "parser.cc"
    break;

  case 285: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1163 "parser.y"
                                                                      {}
#line 4001 "parser.cc"
    break;

  case 286: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1164 "parser.y"
                                                     {}
#line 4007 "parser.cc"
    break;

  case 291: // exp: infixexp "::" sigtype
#line 1176 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4013 "parser.cc"
    break;

  case 292: // exp: infixexp
#line 1177 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4019 "parser.cc"
    break;

  case 293: // infixexp: exp10
#line 1181 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4025 "parser.cc"
    break;

  case 294: // infixexp: infixexp qop exp10
#line 1182 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4031 "parser.cc"
    break;

  case 295: // exp10: PREFIX_MINUS fexp
#line 1184 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4037 "parser.cc"
    break;

  case 296: // exp10: fexp
#line 1185 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4043 "parser.cc"
    break;

  case 299: // fexp: fexp aexp
#line 1193 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4049 "parser.cc"
    break;

  case 300: // fexp: fexp "@" atype
#line 1194 "parser.y"
                                 {}
#line 4055 "parser.cc"
    break;

  case 301: // fexp: "static" aexp
#line 1195 "parser.y"
                                 {}
#line 4061 "parser.cc"
    break;

  case 302: // fexp: aexp
#line 1196 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4067 "parser.cc"
    break;

  case 303: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1199 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4073 "parser.cc"
    break;

  case 304: // aexp: PREFIX_TILDE aexp
#line 1200 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4079 "parser.cc"
    break;

  case 305: // aexp: PREFIX_BANG aexp
#line 1201 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4085 "parser.cc"
    break;

  case 306: // aexp: "\\" apats1 "->" exp
#line 1202 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4091 "parser.cc"
    break;

  case 307: // aexp: "let" binds "in" exp
#line 1203 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4097 "parser.cc"
    break;

  case 308: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1205 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4103 "parser.cc"
    break;

  case 309: // aexp: "case" exp "of" altslist
#line 1207 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4109 "parser.cc"
    break;

  case 310: // aexp: "do" stmtlist
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4115 "parser.cc"
    break;

  case 311: // aexp: "mdo" stmtlist
#line 1209 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4121 "parser.cc"
    break;

  case 312: // aexp: aexp1
#line 1211 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4127 "parser.cc"
    break;

  case 313: // aexp1: aexp1 "{" fbinds "}"
#line 1214 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4133 "parser.cc"
    break;

  case 314: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1215 "parser.y"
                                     { }
#line 4139 "parser.cc"
    break;

  case 315: // aexp1: aexp2
#line 1216 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4145 "parser.cc"
    break;

  case 316: // aexp2: qvar
#line 1219 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4151 "parser.cc"
    break;

  case 317: // aexp2: qcon
#line 1220 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4157 "parser.cc"
    break;

  case 318: // aexp2: literal
#line 1221 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4163 "parser.cc"
    break;

  case 319: // aexp2: "(" texp ")"
#line 1222 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4169 "parser.cc"
    break;

  case 320: // aexp2: "(" tup_exprs ")"
#line 1223 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4175 "parser.cc"
    break;

  case 321: // aexp2: "(" projection ")"
#line 1224 "parser.y"
                              {}
#line 4181 "parser.cc"
    break;

  case 322: // aexp2: "[" list "]"
#line 1229 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4187 "parser.cc"
    break;

  case 323: // aexp2: "_"
#line 1230 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4193 "parser.cc"
    break;

  case 326: // texp: exp
#line 1239 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4199 "parser.cc"
    break;

  case 327: // texp: infixexp qop
#line 1240 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4205 "parser.cc"
    break;

  case 328: // texp: qopm infixexp
#line 1241 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4211 "parser.cc"
    break;

  case 329: // tup_exprs: tup_exprs "," texp
#line 1246 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4217 "parser.cc"
    break;

  case 330: // tup_exprs: texp "," texp
#line 1247 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4223 "parser.cc"
    break;

  case 331: // list: texp
#line 1265 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4229 "parser.cc"
    break;

  case 332: // list: lexps
#line 1266 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4235 "parser.cc"
    break;

  case 333: // list: texp ".."
#line 1267 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4241 "parser.cc"
    break;

  case 334: // list: texp "," exp ".."
#line 1268 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4247 "parser.cc"
    break;

  case 335: // list: texp ".." exp
#line 1269 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4253 "parser.cc"
    break;

  case 336: // list: texp "," exp ".." exp
#line 1270 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4259 "parser.cc"
    break;

  case 337: // list: texp "|" squals
#line 1271 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4265 "parser.cc"
    break;

  case 338: // lexps: lexps "," texp
#line 1273 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4271 "parser.cc"
    break;

  case 339: // lexps: texp "," texp
#line 1274 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4277 "parser.cc"
    break;

  case 340: // squals: squals "," qual
#line 1287 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4283 "parser.cc"
    break;

  case 341: // squals: qual
#line 1289 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4289 "parser.cc"
    break;

  case 342: // guardquals: guardquals1
#line 1299 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4295 "parser.cc"
    break;

  case 343: // guardquals1: guardquals1 "," qual
#line 1301 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4301 "parser.cc"
    break;

  case 344: // guardquals1: qual
#line 1302 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4307 "parser.cc"
    break;

  case 345: // altslist: "{" alts "}"
#line 1305 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4313 "parser.cc"
    break;

  case 346: // altslist: "vocurly" alts close
#line 1306 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4319 "parser.cc"
    break;

  case 347: // altslist: "{" "}"
#line 1307 "parser.y"
                                 {}
#line 4325 "parser.cc"
    break;

  case 348: // altslist: "vocurly" close
#line 1308 "parser.y"
                                 {}
#line 4331 "parser.cc"
    break;

  case 349: // alts: alts1
#line 1310 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4337 "parser.cc"
    break;

  case 350: // alts: ";" alts
#line 1311 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4343 "parser.cc"
    break;

  case 351: // alts1: alts1 ";" alt
#line 1313 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4349 "parser.cc"
    break;

  case 352: // alts1: alts1 ";"
#line 1314 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4355 "parser.cc"
    break;

  case 353: // alts1: alt
#line 1315 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4361 "parser.cc"
    break;

  case 354: // alt: pat alt_rhs
#line 1317 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4367 "parser.cc"
    break;

  case 355: // alt_rhs: "->" exp wherebinds
#line 1319 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4373 "parser.cc"
    break;

  case 356: // alt_rhs: gdpats wherebinds
#line 1320 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4379 "parser.cc"
    break;

  case 357: // gdpats: gdpats gdpat
#line 1322 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4385 "parser.cc"
    break;

  case 358: // gdpats: gdpat
#line 1323 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4391 "parser.cc"
    break;

  case 359: // gdpat: "|" guardquals "->" exp
#line 1332 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4397 "parser.cc"
    break;

  case 360: // pat: exp
#line 1334 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4403 "parser.cc"
    break;

  case 361: // bindpat: exp
#line 1336 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4409 "parser.cc"
    break;

  case 362: // apat: aexp
#line 1338 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4415 "parser.cc"
    break;

  case 363: // apats1: apats1 apat
#line 1340 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4421 "parser.cc"
    break;

  case 364: // apats1: apat
#line 1341 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4427 "parser.cc"
    break;

  case 365: // stmtlist: "{" stmts "}"
#line 1344 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4433 "parser.cc"
    break;

  case 366: // stmtlist: "vocurly" stmts close
#line 1345 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4439 "parser.cc"
    break;

  case 367: // stmts: stmts ";" stmt
#line 1347 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4445 "parser.cc"
    break;

  case 368: // stmts: stmts ";"
#line 1348 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4451 "parser.cc"
    break;

  case 369: // stmts: stmt
#line 1349 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4457 "parser.cc"
    break;

  case 370: // stmts: %empty
#line 1350 "parser.y"
                       {}
#line 4463 "parser.cc"
    break;

  case 371: // stmt: qual
#line 1355 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4469 "parser.cc"
    break;

  case 372: // stmt: "rec" stmtlist
#line 1356 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4475 "parser.cc"
    break;

  case 373: // qual: bindpat "<-" exp
#line 1358 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4481 "parser.cc"
    break;

  case 374: // qual: exp
#line 1359 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4487 "parser.cc"
    break;

  case 375: // qual: "let" binds
#line 1360 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4493 "parser.cc"
    break;

  case 376: // fbinds: fbinds1
#line 1365 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4499 "parser.cc"
    break;

  case 377: // fbinds: %empty
#line 1366 "parser.y"
                        {}
#line 4505 "parser.cc"
    break;

  case 378: // fbinds1: fbind "," fbinds1
#line 1368 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4511 "parser.cc"
    break;

  case 379: // fbinds1: fbind
#line 1369 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4517 "parser.cc"
    break;

  case 380: // fbinds1: ".."
#line 1370 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4523 "parser.cc"
    break;

  case 381: // fbind: qvar "=" texp
#line 1372 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4529 "parser.cc"
    break;

  case 382: // fbind: qvar
#line 1373 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4535 "parser.cc"
    break;

  case 383: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1374 "parser.y"
                                                      {}
#line 4541 "parser.cc"
    break;

  case 384: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1375 "parser.y"
                                                      {}
#line 4547 "parser.cc"
    break;

  case 387: // qcon: gen_qcon
#line 1411 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4553 "parser.cc"
    break;

  case 388: // qcon: sysdcon
#line 1412 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4559 "parser.cc"
    break;

  case 389: // gen_qcon: qconid
#line 1414 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4565 "parser.cc"
    break;

  case 390: // gen_qcon: "(" qconsym ")"
#line 1415 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4571 "parser.cc"
    break;

  case 391: // con: conid
#line 1417 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4577 "parser.cc"
    break;

  case 392: // con: "(" consym ")"
#line 1418 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4583 "parser.cc"
    break;

  case 393: // con: sysdcon
#line 1419 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4589 "parser.cc"
    break;

  case 394: // con_list: con_list "," con
#line 1421 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4595 "parser.cc"
    break;

  case 395: // con_list: con
#line 1422 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4601 "parser.cc"
    break;

  case 396: // sysdcon_no_list: "(" ")"
#line 1424 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4607 "parser.cc"
    break;

  case 397: // sysdcon_no_list: "(" commas ")"
#line 1425 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4613 "parser.cc"
    break;

  case 398: // sysdcon_no_list: "(#" "#)"
#line 1426 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4619 "parser.cc"
    break;

  case 399: // sysdcon_no_list: "(#" commas "#)"
#line 1427 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4625 "parser.cc"
    break;

  case 400: // sysdcon: sysdcon_no_list
#line 1429 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4631 "parser.cc"
    break;

  case 401: // sysdcon: "[" "]"
#line 1430 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4637 "parser.cc"
    break;

  case 402: // conop: consym
#line 1432 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4643 "parser.cc"
    break;

  case 403: // conop: "`" conid "`"
#line 1433 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4649 "parser.cc"
    break;

  case 404: // qconop: qconsym
#line 1435 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4655 "parser.cc"
    break;

  case 405: // qconop: "`" qconid "`"
#line 1436 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4661 "parser.cc"
    break;

  case 406: // gtycon: ntgtycon
#line 1439 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4667 "parser.cc"
    break;

  case 407: // gtycon: "(" ")"
#line 1440 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4673 "parser.cc"
    break;

  case 408: // gtycon: "(#" "#)"
#line 1441 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4679 "parser.cc"
    break;

  case 409: // ntgtycon: oqtycon
#line 1443 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4685 "parser.cc"
    break;

  case 410: // ntgtycon: "(" commas ")"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4691 "parser.cc"
    break;

  case 411: // ntgtycon: "(#" commas "#)"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4697 "parser.cc"
    break;

  case 412: // ntgtycon: "(" "->" ")"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4703 "parser.cc"
    break;

  case 413: // ntgtycon: "[" "]"
#line 1447 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4709 "parser.cc"
    break;

  case 414: // oqtycon: qtycon
#line 1449 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4715 "parser.cc"
    break;

  case 415: // oqtycon: "(" qtyconsym ")"
#line 1450 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4721 "parser.cc"
    break;

  case 416: // oqtycon_no_varcon: qtycon
#line 1452 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4727 "parser.cc"
    break;

  case 417: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1453 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4733 "parser.cc"
    break;

  case 418: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1454 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4739 "parser.cc"
    break;

  case 419: // oqtycon_no_varcon: "(" ":" ")"
#line 1455 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4745 "parser.cc"
    break;

  case 420: // qtyconop: qtyconsym
#line 1458 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4751 "parser.cc"
    break;

  case 421: // qtyconop: "`" qtycon "`"
#line 1459 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4757 "parser.cc"
    break;

  case 422: // qtycondoc: qtycon
#line 1461 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4763 "parser.cc"
    break;

  case 423: // qtycon: "QCONID"
#line 1463 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4769 "parser.cc"
    break;

  case 424: // qtycon: tycon
#line 1464 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4775 "parser.cc"
    break;

  case 425: // tycon: "CONID"
#line 1468 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4781 "parser.cc"
    break;

  case 426: // qtyconsym: "QCONSYM"
#line 1470 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4787 "parser.cc"
    break;

  case 427: // qtyconsym: "QVARSYM"
#line 1471 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4793 "parser.cc"
    break;

  case 428: // qtyconsym: tyconsym
#line 1472 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4799 "parser.cc"
    break;

  case 429: // tyconsym: "CONSYM"
#line 1474 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4805 "parser.cc"
    break;

  case 430: // tyconsym: "VARSYM"
#line 1475 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4811 "parser.cc"
    break;

  case 431: // tyconsym: ":"
#line 1476 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4817 "parser.cc"
    break;

  case 432: // tyconsym: "-"
#line 1477 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4823 "parser.cc"
    break;

  case 433: // op: varop
#line 1482 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4829 "parser.cc"
    break;

  case 434: // op: conop
#line 1483 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4835 "parser.cc"
    break;

  case 435: // varop: varsym
#line 1485 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4841 "parser.cc"
    break;

  case 436: // varop: "`" varid "`"
#line 1486 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4847 "parser.cc"
    break;

  case 437: // qop: qvarop
#line 1488 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4853 "parser.cc"
    break;

  case 438: // qop: qconop
#line 1489 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4859 "parser.cc"
    break;

  case 439: // qopm: qvaropm
#line 1492 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4865 "parser.cc"
    break;

  case 440: // qopm: qconop
#line 1493 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4871 "parser.cc"
    break;

  case 441: // qvarop: qvarsym
#line 1498 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4877 "parser.cc"
    break;

  case 442: // qvarop: "`" qvarid "`"
#line 1499 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4883 "parser.cc"
    break;

  case 443: // qvaropm: qvarsym_no_minus
#line 1501 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4889 "parser.cc"
    break;

  case 444: // qvaropm: "`" qvarid "`"
#line 1502 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4895 "parser.cc"
    break;

  case 445: // tyvar: tyvarid
#line 1506 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4901 "parser.cc"
    break;

  case 446: // tyvarop: "`" tyvarid "`"
#line 1508 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4907 "parser.cc"
    break;

  case 447: // tyvarid: "VARID"
#line 1510 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4913 "parser.cc"
    break;

  case 448: // tyvarid: special_id
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4919 "parser.cc"
    break;

  case 449: // tyvarid: "unsafe"
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4925 "parser.cc"
    break;

  case 450: // tyvarid: "safe"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4931 "parser.cc"
    break;

  case 451: // tyvarid: "interruptible"
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4937 "parser.cc"
    break;

  case 452: // var: varid
#line 1517 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4943 "parser.cc"
    break;

  case 453: // var: "(" varsym ")"
#line 1518 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4949 "parser.cc"
    break;

  case 454: // qvar: qvarid
#line 1520 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4955 "parser.cc"
    break;

  case 455: // qvar: "(" varsym ")"
#line 1521 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4961 "parser.cc"
    break;

  case 456: // qvar: "(" qvarsym1 ")"
#line 1522 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4967 "parser.cc"
    break;

  case 457: // field: varid
#line 1524 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4973 "parser.cc"
    break;

  case 458: // qvarid: varid
#line 1526 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4979 "parser.cc"
    break;

  case 459: // qvarid: "QVARID"
#line 1527 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4985 "parser.cc"
    break;

  case 460: // varid: "VARID"
#line 1529 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4991 "parser.cc"
    break;

  case 461: // varid: special_id
#line 1530 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4997 "parser.cc"
    break;

  case 462: // varid: "unsafe"
#line 1531 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5003 "parser.cc"
    break;

  case 463: // varid: "safe"
#line 1532 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5009 "parser.cc"
    break;

  case 464: // varid: "interruptible"
#line 1533 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5015 "parser.cc"
    break;

  case 465: // varid: "forall"
#line 1534 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5021 "parser.cc"
    break;

  case 466: // varid: "family"
#line 1535 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5027 "parser.cc"
    break;

  case 467: // varid: "role"
#line 1536 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5033 "parser.cc"
    break;

  case 468: // qvarsym: varsym
#line 1538 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5039 "parser.cc"
    break;

  case 469: // qvarsym: qvarsym1
#line 1539 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5045 "parser.cc"
    break;

  case 470: // qvarsym_no_minus: varsym_no_minus
#line 1541 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5051 "parser.cc"
    break;

  case 471: // qvarsym_no_minus: qvarsym1
#line 1542 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5057 "parser.cc"
    break;

  case 472: // qvarsym1: "QVARSYM"
#line 1544 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5063 "parser.cc"
    break;

  case 473: // varsym: varsym_no_minus
#line 1546 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5069 "parser.cc"
    break;

  case 474: // varsym: "-"
#line 1547 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5075 "parser.cc"
    break;

  case 475: // varsym_no_minus: "VARSYM"
#line 1549 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5081 "parser.cc"
    break;

  case 476: // varsym_no_minus: special_sym
#line 1550 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5087 "parser.cc"
    break;

  case 477: // special_id: "as"
#line 1552 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5093 "parser.cc"
    break;

  case 478: // special_id: "qualified"
#line 1553 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5099 "parser.cc"
    break;

  case 479: // special_id: "hiding"
#line 1554 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5105 "parser.cc"
    break;

  case 480: // special_id: "export"
#line 1555 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5111 "parser.cc"
    break;

  case 481: // special_id: "label"
#line 1556 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5117 "parser.cc"
    break;

  case 482: // special_id: "dynamic"
#line 1557 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5123 "parser.cc"
    break;

  case 483: // special_id: "stdcall"
#line 1558 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5129 "parser.cc"
    break;

  case 484: // special_id: "ccall"
#line 1559 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5135 "parser.cc"
    break;

  case 485: // special_id: "capi"
#line 1560 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5141 "parser.cc"
    break;

  case 486: // special_id: "prim"
#line 1561 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5147 "parser.cc"
    break;

  case 487: // special_id: "javascript"
#line 1562 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5153 "parser.cc"
    break;

  case 488: // special_id: "group"
#line 1563 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5159 "parser.cc"
    break;

  case 489: // special_id: "stock"
#line 1564 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5165 "parser.cc"
    break;

  case 490: // special_id: "anyclass"
#line 1565 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5171 "parser.cc"
    break;

  case 491: // special_id: "via"
#line 1566 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5177 "parser.cc"
    break;

  case 492: // special_id: "unit"
#line 1567 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5183 "parser.cc"
    break;

  case 493: // special_id: "dependency"
#line 1568 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5189 "parser.cc"
    break;

  case 494: // special_id: "signature"
#line 1569 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5195 "parser.cc"
    break;

  case 495: // special_sym: "."
#line 1571 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5201 "parser.cc"
    break;

  case 496: // special_sym: "*"
#line 1572 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5207 "parser.cc"
    break;

  case 497: // qconid: conid
#line 1576 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5213 "parser.cc"
    break;

  case 498: // qconid: "QCONID"
#line 1577 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5219 "parser.cc"
    break;

  case 499: // conid: "CONID"
#line 1579 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5225 "parser.cc"
    break;

  case 500: // qconsym: consym
#line 1581 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5231 "parser.cc"
    break;

  case 501: // qconsym: "QCONSYM"
#line 1582 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5237 "parser.cc"
    break;

  case 502: // consym: "CONSYM"
#line 1584 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5243 "parser.cc"
    break;

  case 503: // consym: ":"
#line 1585 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5249 "parser.cc"
    break;

  case 504: // literal: "CHAR"
#line 1589 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5255 "parser.cc"
    break;

  case 505: // literal: "STRING"
#line 1590 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5261 "parser.cc"
    break;

  case 506: // literal: "INTEGER"
#line 1591 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5267 "parser.cc"
    break;

  case 507: // literal: "RATIONAL"
#line 1592 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5273 "parser.cc"
    break;

  case 508: // literal: "PRIMINTEGER"
#line 1593 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5279 "parser.cc"
    break;

  case 510: // close: error
#line 1601 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5285 "parser.cc"
    break;

  case 511: // modid: "CONID"
#line 1605 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5291 "parser.cc"
    break;

  case 512: // modid: "QCONID"
#line 1606 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5297 "parser.cc"
    break;

  case 513: // commas: commas ","
#line 1608 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5303 "parser.cc"
    break;

  case 514: // commas: ","
#line 1609 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5309 "parser.cc"
    break;


#line 5313 "parser.cc"

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


  const short parser::yypact_ninf_ = -666;

  const short parser::yytable_ninf_ = -474;

  const short
  parser::yypact_[] =
  {
      80,   -24,  -666,    85,  -666,  -666,  -666,  -666,  -666,   365,
      -5,   -52,  -666,    60,    87,    87,    23,  -666,  -666,  -666,
    -666,   160,  -666,  -666,  -666,    92,  -666,   165,   176,  1419,
     246,   266,   179,  -666,   866,  -666,   102,  -666,  -666,  -666,
     -24,  -666,   -24,  -666,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,  -666,  -666,  -666,   342,  -666,  -666,  -666,  -666,
    -666,   208,    99,  -666,   242,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,   353,  -666,   -24,  -666,   244,  -666,  2472,  4519,
     339,   280,   374,  2472,  -666,  -666,  -666,   472,   384,  -666,
    3580,   392,   374,  3152,   303,  4966,   263,  3152,  3152,  2744,
    3152,  1792,  1656,   288,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,    34,   303,   310,   179,  -666,  -666,  -666,  -666,    48,
      -8,  -666,  -666,   989,  -666,  2880,  -666,     5,  -666,  -666,
    -666,  -666,  -666,  -666,   352,    18,  -666,  -666,  -666,  -666,
     336,  -666,  -666,   359,  -666,  -666,  -666,  -666,   372,  -666,
     378,   382,   405,  -666,  -666,  -666,  4624,  -666,  4661,  -666,
    -666,  -666,  -666,   527,  -666,  1656,   511,   445,  -666,  -666,
    -666,  4519,  4519,  -666,  5065,  3681,  3264,   449,  -666,   549,
     489,  -666,   326,  -666,  3868,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,  4519,  3969,  2200,  2200,  -666,   467,   509,   512,
     513,   518,  3969,  1286,  1286,  -666,   573,  4519,  4519,   100,
     521,   847,   121,   565,  -666,  -666,   -12,  4966,  -666,   391,
      44,   498,    -9,  -666,   137,  -666,  -666,  -666,  -666,  3016,
    -666,  2880,  -666,  -666,  -666,  4866,  -666,  -666,  -666,   445,
     173,   499,   490,  -666,  2472,  -666,  -666,  -666,  -666,  -666,
    -666,  5151,  -666,  -666,   247,   169,   301,   382,   497,   502,
     505,   305,  -666,   307,  3969,  4966,  4966,  -666,   657,   244,
     541,   488,  4519,  3969,  5065,  2472,  2608,  4866,  -666,    28,
    -666,  -666,  2472,  -666,  -666,  -666,  -666,  4519,  -666,  5151,
    4903,  3152,  -666,  -666,  -666,  -666,  -666,  -666,  -666,   517,
     522,   503,  -666,   526,    60,   -24,    53,   394,  3969,  -666,
    -666,   216,   150,   531,   507,  -666,  -666,  -666,  -666,   530,
     560,   553,  -666,  -666,   533,  -666,  -666,  -666,  -666,  -666,
    -666,   534,   544,   540,  -666,   330,   341,   433,  -666,  4519,
    3969,   808,  4519,  -666,  -666,  -666,  4519,  -666,  -666,   580,
    -666,   552,   547,   384,   374,   591,   592,    -3,  -666,  -666,
      33,  -666,   655,  -666,  -666,  -666,  -666,  -666,  -666,   659,
       2,  -666,  -666,   989,    42,  2472,  -666,   607,   429,  3969,
     204,  3969,   556,   557,   581,   615,  -666,   618,   583,   255,
     263,   620,  2472,  -666,   582,   584,  2472,  2472,  2608,  1928,
    -666,  1928,   130,  -666,  -666,  5151,  -666,  -666,  1928,  -666,
    1928,   159,  -666,  -666,  -666,  -666,   630,   628,   629,  5012,
     593,  -666,  -666,  -666,  -666,  -666,  4070,    89,   441,  -666,
    -666,  -666,  -666,   683,   633,   598,  -666,   602,   384,  -666,
    -666,  -666,  -666,  -666,  -666,   613,  -666,   610,   649,   636,
     637,  -666,  -666,  -666,  4802,  -666,  -666,  -666,   627,  1456,
    -666,  -666,  2064,  1520,  -666,  -666,   634,  3969,  -666,  5065,
    5204,  -666,  3969,  3969,  -666,  -666,  3969,  -666,  -666,  -666,
    1150,  1150,  -666,  -666,  -666,   621,   623,   699,  -666,  3969,
    -666,  -666,  3969,   573,  -666,  2472,  -666,  2200,  -666,  2472,
     436,  -666,  -666,  1286,  -666,  -666,  3969,  3969,  5313,   664,
    -666,  -666,   404,  -666,  -666,  5065,   640,  -666,  -666,  -666,
    -666,   642,   271,   349,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,   635,  -666,   674,  -666,  -666,  -666,  -666,  -666,
    -666,  3969,  3969,   638,   641,   657,  -666,   441,   665,  -666,
    -666,   678,  3969,   734,   739,   759,  -666,  2472,  2608,  -666,
    -666,  -666,  4903,  1928,  5151,  -666,  1456,   -24,  -666,   242,
     656,   152,  -666,  -666,  2336,  -666,   666,   654,  -666,   465,
      60,  -666,  -666,  -666,  -666,  3969,  5346,  5346,  -666,  -666,
    -666,  -666,  -666,   660,   737,  3782,  -666,  -666,   227,  -666,
      43,  -666,  -666,  -666,  -666,  -666,  -666,   467,  1008,  1008,
    -666,  -666,  -666,  -666,  -666,  5346,   748,   696,  -666,  -666,
    -666,  2608,  2472,  -666,    54,   126,  -666,  -666,  -666,  5118,
     739,   759,  4519,  -666,  -666,  -666,   700,  -666,  4519,   457,
     759,   164,  -666,   759,  -666,  -666,  -666,  -666,  -666,   -13,
    -666,   667,  -666,  -666,  -666,  4765,  -666,  -666,  -666,  2472,
    2608,  2472,  -666,    55,  -666,  -666,  -666,   133,   713,  -666,
    -666,  4519,  4519,  4519,  -666,   475,  -666,  1150,  -666,   785,
    -666,   782,  -666,   782,  -666,   228,  -666,    46,  -666,   719,
     458,  -666,  3969,  -666,  -666,  -666,  3969,  -666,  4519,  4519,
     759,  -666,  -666,  5237,   734,   722,  3370,  -666,  -666,  -666,
     467,   467,  -666,  -666,  -666,  -666,  4156,   226,   761,  -666,
    -666,  -666,  1928,  5151,  -666,  -666,  -666,   729,   683,  -666,
    -666,  3969,  -666,  3969,   580,  -666,   491,  3969,  4261,  -666,
    -666,  2472,  -666,  4519,   541,  -666,  1008,  -666,  5346,  4347,
    4433,  -666,  -666,  -666,  -666,   725,   699,  -666,  -666,  -666,
    4519,   694,  -666,  4519,   250,  -666,   263,    51,  -666,  -666,
     702,   705,  -666,  4519,  -666,  -666,  -666,  2472,  -666,   717,
     712,  -666,   755,  -666,  -666,  3264,   742,   745,  -666,  -666,
    4070,  -666,  5346,  -666,   726,   269,  -666,    60,    61,  4519,
    3475,  -666,  4519,  -666,   467,   185,  -666,  4519,  -666,  -666,
    -666,  -666,  -666,   713,  5346,   441,  -666,  -666,  -666,  4519,
    -666,  -666,  -666,  -666,  3969,  -666,  -666,   739,   759,  -666,
    -666,   759,  -666,  -666
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   511,   512,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   510,   509,    12,   183,   179,     0,     0,    20,
       0,    46,    41,    15,    14,   182,     0,     6,     7,   477,
       0,   479,     0,   478,   465,   480,   481,   482,   463,   464,
     462,   466,   467,   483,   484,   485,   486,   487,   488,   489,
     490,   491,   492,   494,   493,     0,   460,   425,   459,   423,
      22,     0,    19,    24,    28,    36,   416,   424,    35,   454,
     458,   461,     0,    45,     0,    38,    42,   323,     0,     0,
     129,     0,     0,     0,    63,    64,    65,    96,     0,   130,
       0,     0,     0,     0,   287,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   499,   498,   504,   505,   506,   507,
     508,   287,   287,    61,    68,    71,    72,    73,    74,   149,
       0,    77,   270,    78,   293,   296,   302,   312,   315,   317,
     387,   400,   388,   189,   316,   458,   389,   497,   318,   180,
       0,    27,    26,     0,   474,   496,   495,   475,     0,   472,
       0,     0,     0,   473,   476,    17,     0,    21,    31,    25,
      40,    40,     3,    48,    37,     0,     0,   292,   450,   451,
     449,     0,     0,   217,   255,     0,     0,     0,   447,   159,
       0,   142,   200,   203,   204,   208,   215,   409,   414,   216,
     445,   448,     0,   229,   370,   370,   310,   298,     0,     0,
       0,     0,     0,   173,   173,   176,     0,     0,     0,     0,
       0,   200,   409,     0,   311,   301,     0,     0,   288,     0,
       0,     0,     0,   395,   184,   393,   391,   362,   364,     0,
     304,   295,   305,   503,   401,     0,   502,   501,   326,   292,
     331,     0,   332,   440,     0,   439,   443,   471,   470,   404,
     500,     0,   396,   514,     0,     0,     0,   471,     0,   470,
     404,     0,   398,     0,     0,     0,     0,    62,     0,    69,
     149,     0,     0,     0,     0,     0,     0,     0,   271,   178,
     276,   438,     0,   437,   441,   469,   468,     0,   299,     0,
     377,     0,   181,   419,   418,   417,   456,   455,    23,     0,
       0,    32,    34,     0,     0,     0,    50,     0,     0,   219,
     218,     0,     0,     0,   256,   258,   452,   233,   413,     0,
     192,     0,   196,   431,     0,   432,   221,   430,   429,   427,
     426,   230,     0,     0,   428,     0,     0,     0,    79,     0,
       0,     0,     0,   212,   420,   213,     0,   209,   211,   133,
     230,     0,   228,     0,     0,   374,     0,     0,   369,   371,
       0,   297,     0,    93,    92,    94,    95,   225,   186,   169,
       0,   272,   172,     0,     0,     0,    89,     0,   135,     0,
       0,     0,     0,     0,     0,     0,   282,     0,     0,     0,
       0,     0,     0,   363,     0,     0,   327,   333,     0,     0,
     322,     0,   328,   325,   457,     0,   321,   319,     0,   320,
       0,   455,   390,   397,   513,   399,     0,     0,     0,     0,
     279,   434,    67,   433,   435,   402,     0,     0,   131,   278,
     197,   187,   188,   178,     0,   342,   344,     0,     0,   274,
     275,   294,   300,   314,   380,     0,   376,   379,   382,     0,
     458,   303,    30,    29,     0,     9,    10,    47,     0,    54,
      44,    49,     0,     0,   309,   291,     0,     0,   220,     0,
       0,   223,     0,     0,   412,   224,     0,   415,   410,   411,
     155,   155,   158,   141,   201,     0,     0,   205,   210,     0,
      84,    75,     0,   375,   372,     0,   365,   368,   366,     0,
       0,    88,   174,   171,   175,   307,     0,     0,     0,   101,
     239,    85,     0,    86,    80,     0,     0,   289,   281,   283,
     392,     0,     0,     0,   185,   406,   394,   280,   306,   444,
     405,   335,   337,   341,   326,   339,   338,   324,   330,   329,
     286,     0,     0,     0,     0,     0,   233,   131,     0,   146,
     148,     0,     0,   252,   242,   260,   273,     0,     0,   442,
     177,   313,     0,     0,     0,    33,    54,     0,    56,    28,
       0,    53,    58,   347,     0,   360,     0,   349,   353,     0,
       0,   348,   453,   259,   257,     0,     0,     0,   232,   234,
     237,   193,   195,   231,   122,     0,   150,   154,     0,   151,
       0,   421,   446,   134,   231,   373,   367,   298,   165,   165,
     168,   170,   116,   136,   137,     0,   106,     0,   290,   407,
     408,     0,   334,   190,     0,     0,   436,   403,    66,     0,
     242,   260,     0,   147,   132,   233,   246,   248,     0,     0,
     260,     0,    81,   261,   263,   277,   343,   378,   381,   384,
     386,     0,    60,    59,    51,     0,    55,   350,   345,   352,
       0,     0,   354,   178,   358,   346,   194,     0,     0,   222,
     123,     0,     0,     0,   120,   138,   156,   153,   157,     0,
     129,   124,   160,   124,   164,     0,   161,     0,   102,     0,
       0,    83,     0,   340,   336,   284,     0,   285,     0,     0,
     260,    90,   145,     0,   252,     0,   253,   206,   214,   250,
     298,   298,    82,    99,    97,    98,     0,     0,   264,   267,
     422,   262,     0,     0,    52,    57,   351,     0,   178,   356,
     357,     0,   235,     0,   133,   121,   138,     0,     0,   118,
     152,     0,   125,     0,   149,   166,   163,   167,     0,   115,
     115,   107,    76,   191,   144,     0,   198,    91,   251,   247,
       0,     0,   207,     0,     0,   244,     0,     0,   268,   202,
     226,     0,   265,     0,   266,   383,   385,     0,   355,     0,
       0,   117,     0,   119,   139,     0,     0,   216,   308,   126,
       0,   162,   103,   105,     0,     0,   114,     0,     0,     0,
     253,   249,   254,   240,   298,     0,   241,     0,   269,   100,
     359,   236,   238,   216,     0,   131,   104,   110,   108,   113,
     111,   109,   143,   243,     0,   227,   140,   242,   260,   112,
     245,   260,   127,   128
  };

  const short
  parser::yypgoto_[] =
  {
    -666,  -666,  -666,  -666,  -666,  -666,  -666,    36,  -666,  -666,
    -666,  -666,   668,   251,  -666,  -666,   -11,   709,  -666,  -666,
    -666,  -666,  -666,  -666,  -666,  -666,   259,  -666,   171,  -666,
    -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,  -666,  -666,    13,  -666,  -666,  -666,    84,  -201,
    -666,  -666,   156,  -666,   819,  -520,   110,  -666,   111,   574,
      58,  -268,   178,   375,  -666,  -666,   112,   256,  -666,  -666,
     673,  -666,  -300,  -421,   865,  -666,  -666,  -308,   187,  -155,
     355,  -143,    98,  -666,   -84,  -666,   -85,  -666,   -26,  -666,
    -312,  -666,  -666,  -666,  -629,  -125,   617,    95,  -666,   708,
    -502,   396,  -596,  -358,  -601,   194,   113,  -519,  -666,   210,
    -666,   155,  -666,  -666,   455,  -592,  -666,   275,   209,   903,
    -178,  -666,  -666,   650,  -666,   461,  -666,    94,    14,  -252,
    -184,   829,   -27,  -666,  -666,  -666,   -81,  -666,  -666,  -666,
    -666,   273,  -666,  -666,  -428,  -666,   272,  -666,  -666,   274,
    -666,  -666,   706,  -666,   -75,   741,   437,  -261,  -666,   377,
    -666,  -666,  -666,  -666,   550,   175,  -666,   -97,  -665,   -47,
    -666,   554,   -72,  -666,  -666,  -666,   -22,  -666,  -189,  -666,
     397,  -666,   707,  -666,  -666,  -666,  -434,  -666,  -350,  -253,
      66,  -240,   -21,   -14,  -666,  -666,   -39,   -41,   -98,   -89,
    -666,   -10,  -104,   -48,  -199,  -666,  -295,   -34,   -96
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   172,     6,    10,    19,    30,
      71,    72,    73,   169,   310,   311,   579,    86,    11,    20,
      21,    32,    84,   316,   470,   471,   580,   581,   582,   278,
     123,   430,    33,    34,   124,   125,   126,   127,   219,   128,
     212,   727,   784,   626,   698,   802,   701,   761,   805,   806,
     606,   681,   753,   692,   693,   564,   500,   519,   749,   189,
     557,   282,   607,   608,   492,   348,   694,   695,   620,   511,
     380,   215,   216,   449,    27,    36,   401,   377,   439,   130,
     634,   341,   520,   441,   331,   715,   332,   780,   192,   193,
     716,   194,   357,   352,   717,   195,   379,   781,   361,   342,
     480,   598,   599,   521,   650,   774,   775,   565,   646,   647,
     648,   719,   323,   324,   325,   652,   653,   654,   728,   381,
     609,   288,   289,   290,   132,   227,   228,   248,   177,   134,
     776,   135,   136,   137,   138,   264,   265,   266,   251,   252,
     542,   444,   445,   474,   586,   587,   588,   672,   673,   674,
     589,   366,   238,   239,   206,   367,   368,   369,   455,   456,
     457,   659,   139,   140,   233,   234,   141,   142,   431,   253,
     534,   196,   197,    75,   353,   729,   198,    77,   343,   344,
     432,   433,   292,   254,   293,   255,   199,   355,   200,   143,
     144,   459,    79,    80,   294,   256,   257,   296,   163,    81,
     164,   146,   147,   259,   260,   148,    24,     9,   271
  };

  const short
  parser::yytable_[] =
  {
     201,   496,   236,   354,   191,   190,   151,    76,   152,   235,
     475,   201,   436,   258,   269,   220,   386,   273,    74,   466,
     145,   413,   566,   372,   162,   446,   161,   224,   222,   322,
     250,   442,   354,   398,    22,   382,   382,   640,   641,   710,
     451,    13,   329,    22,    22,   590,   600,    22,   133,   711,
     173,   773,    22,   274,   639,   448,   319,   320,   722,   453,
     360,    22,    22,   503,   270,   468,    18,   280,   732,   358,
     243,   268,   283,   267,   221,   508,   225,   269,   393,   435,
     237,   240,   448,   242,   600,    12,   291,   772,   733,   514,
     345,   346,   201,   201,   295,    78,   201,   201,  -452,     7,
      17,     1,   506,     8,   262,   201,   299,   512,   298,   300,
     263,   284,   286,   201,   201,   507,   246,   359,   767,   281,
     513,   396,   394,   201,   601,   249,   249,   270,   201,   201,
     291,   705,   387,   388,   162,    29,   267,  -452,   295,   670,
      23,   613,   226,   713,    76,   773,    76,   543,   570,    23,
      23,   507,   796,    23,   451,    74,   667,   312,    23,   623,
     513,   687,   677,   678,   756,   469,   560,    23,    23,   814,
     326,   230,   452,   706,    31,   547,   221,   397,   591,   829,
     389,   772,   176,   772,     2,   201,   723,   207,   162,   249,
     161,   221,   221,   201,   201,    25,   796,   191,   190,   145,
     145,   -87,   291,   707,   644,   600,   313,   314,   201,   243,
     295,   149,   237,   741,   298,   724,   725,   399,   166,   390,
      26,   150,   561,   154,   404,    35,   155,   383,   383,   201,
     477,   498,    78,   156,    78,   405,   841,   434,   742,  -453,
     -87,   167,    37,   633,   633,   706,   842,   414,   524,   843,
     287,   407,   739,    38,   157,   246,   400,   408,   159,   247,
     201,   201,   201,   201,   493,   834,   447,   201,   412,   284,
     326,   665,   627,    82,   461,   699,   726,   405,  -453,   600,
     476,   467,   417,   330,   330,   414,   460,    67,   418,   504,
      83,    69,   409,   395,   666,   675,   236,    85,   365,   365,
     201,   330,   201,   235,   400,   837,   838,   656,   354,   154,
     378,   258,   155,   258,   797,   688,   522,   788,   523,   156,
     258,   165,   258,   221,   322,   554,   497,    67,   545,   495,
     546,    69,   686,   755,   660,   621,   291,   548,   726,   549,
     157,   427,   428,   603,   295,   687,   756,   201,   415,    67,
     333,   559,   558,    69,   168,   813,   435,   334,   600,   614,
     416,   823,   174,   531,   335,   291,   458,   532,   814,   533,
     703,   231,   378,   295,   828,   232,   202,   113,    67,   443,
     365,   440,    69,   789,   629,   790,   114,   829,   201,   794,
     263,   201,   203,   201,   201,   337,   338,   201,   763,   339,
     340,   414,   757,   272,   684,   333,   223,   263,   803,   446,
     201,   226,   350,   201,   419,   553,   378,  -198,   423,   335,
     420,   153,   425,   249,   424,   249,   424,   201,   201,   201,
      14,    15,   249,   689,   249,   154,   345,   346,   155,   301,
     696,   696,    76,   488,   277,   156,   351,    76,   494,   424,
     337,   338,   826,   575,   339,   340,   489,   170,   810,   171,
     424,   812,   201,   201,   630,   326,   157,   158,   263,   302,
     159,   160,   303,   201,   699,   258,   145,   145,   204,   515,
     205,   745,   816,   333,   154,   304,   800,   155,   213,   330,
     214,   305,   658,   786,   156,   306,   538,   335,   472,   145,
     473,   541,   365,   544,   383,   383,   201,   201,   201,   517,
     518,   326,   830,   831,   434,   157,   201,   435,   307,   159,
     685,   562,   563,   718,   243,   318,   840,   383,   337,   338,
      78,   315,   339,   340,   317,    78,   201,   490,   154,   491,
     618,   155,   619,   662,   208,   209,   210,   211,   156,   670,
     201,   671,   799,   201,    76,   747,   748,   712,   460,   201,
     414,   720,   759,   721,   760,   287,   585,   585,   263,   157,
     246,   747,   792,   159,   247,   593,   347,   354,   696,   221,
     349,   602,   275,   276,   330,   371,   373,   249,   385,   374,
     375,   718,   201,   201,   201,   376,   744,   387,   746,   615,
     330,   365,   391,   617,   145,   145,   392,   244,   410,   411,
     421,   435,   281,   201,   622,  -473,   221,   201,   422,   201,
     201,   437,   464,   764,   201,   765,   479,   201,   839,   730,
     462,   465,   383,   383,   258,   463,   478,   201,   458,   481,
     482,   779,    78,    76,   483,   718,   484,   485,   718,   378,
     378,   785,   201,   487,   201,   221,   221,   221,   201,   201,
     499,   655,   365,   486,   201,   501,   502,   554,   387,   201,
     201,   201,   236,   145,   387,   387,  -361,   505,   585,   235,
     509,   201,   221,   766,   201,   718,   510,   718,   516,   525,
     527,   526,   528,   676,   201,   529,   530,   537,   819,   345,
     221,   383,   539,   201,   540,   730,   201,   550,   551,   552,
     448,   201,   555,   201,   567,   559,   558,   568,   571,   414,
     201,   201,   569,   201,   832,   365,   704,   221,   201,   572,
     573,    78,   779,   221,   221,   201,   243,   574,  -457,   576,
     201,   611,   145,   612,   387,   201,   249,   592,   625,   628,
     154,   328,   632,   155,   631,   643,   642,   221,   636,    39,
     156,   637,   645,   585,   365,   738,   649,    41,   651,   664,
     383,   668,   669,   679,   680,   700,   702,   429,   333,    43,
     734,   157,   246,   221,   714,    45,    46,    47,   178,   179,
     180,   221,   335,   743,    53,    54,   751,    55,    56,    57,
     440,   752,    58,   221,   378,   758,    59,    60,    61,    62,
      63,    64,    39,   770,   783,   787,   809,   114,   818,   351,
      41,   817,   821,   337,   338,   822,   824,   339,   340,  -237,
     663,   827,    43,   279,   308,   661,   735,   836,    45,    46,
      47,   178,   179,   180,   808,   798,   622,    53,    54,   754,
      55,    56,    57,   129,   791,    58,   438,   793,   825,    59,
      60,    61,    62,    63,    64,   750,   610,   597,   801,    87,
      39,    88,    89,    90,    91,   697,    92,   188,    41,    93,
      28,   820,    94,    95,    96,    97,    98,   384,    99,   762,
      43,   426,   100,   330,    44,   101,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,   635,    55,    56,
      57,   362,   835,    58,   624,   777,   103,    59,    60,    61,
      62,    63,    64,   104,   769,   811,   333,   833,   731,   105,
     188,    67,   378,   350,   594,    69,   782,   131,   241,   450,
     335,   736,   106,   737,   616,   403,   370,   740,   107,   657,
     536,   815,   638,   535,     0,   108,   406,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,   351,     0,     0,
       0,   337,   338,     0,   111,   339,   340,     0,   112,     0,
     113,     0,     0,     0,     0,     0,     0,     0,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,     0,   120,     0,     0,     0,     0,   121,
     122,    87,    39,    88,     0,   690,     0,     0,    92,     0,
      41,    93,     0,     0,    94,    95,    96,     0,    98,     0,
      99,     0,    43,     0,   691,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,   104,     0,     0,   243,     0,
     285,   105,     0,   286,     0,     0,     0,     0,     0,     0,
       0,     0,   154,     0,   106,   155,     0,     0,     0,     0,
     107,     0,   156,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,   287,
       0,     0,     0,   157,   246,     0,   111,   159,   247,     0,
     112,     0,   113,     0,     0,     0,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,     0,     0,     0,
       0,   121,   122,    87,    39,    88,     0,   604,     0,     0,
      92,     0,    41,    93,     0,     0,    94,    95,    96,     0,
      98,     0,     0,     0,    43,     0,   605,     0,    44,     0,
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
      59,    60,    61,    62,    63,    64,     0,   577,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,   578,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    23,   111,     0,
       0,     0,   175,     0,   113,     0,     0,     0,   584,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   243,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,   154,
     110,     0,   155,     0,     0,     0,     0,     0,   261,   156,
       0,     0,     0,     0,   111,     0,     0,     0,   175,   262,
     113,     0,     0,     0,     0,   263,   245,     0,    66,   114,
     157,   246,    68,   115,   159,   247,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   243,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,   155,     0,
       0,     0,     0,     0,     0,   156,     0,     0,     0,     0,
     111,   244,     0,     0,   175,     0,   113,     0,     0,     0,
       0,     0,   245,     0,    66,   114,   157,   246,    68,   115,
     159,   247,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   243,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,   155,     0,     0,     0,     0,     0,
       0,   156,     0,     0,     0,     0,   111,     0,     0,     0,
     175,     0,   113,     0,     0,     0,     0,     0,   245,     0,
      66,   114,   157,   246,    68,   115,   159,   247,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   583,
       0,     0,   111,     0,     0,     0,   175,     0,   113,     0,
       0,     0,   584,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,   364,    58,     0,     0,
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
     113,     0,     0,     0,   584,     0,     0,     0,    66,   114,
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
      41,    93,     0,     0,     0,     0,     0,     0,   363,     0,
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
       0,     0,   107,     0,     0,     0,     0,     0,   297,   108,
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
       0,     0,   402,     0,     0,   108,     0,     0,     0,     0,
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
     120,     0,   327,     0,    45,    46,    47,   178,   179,   180,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   333,     0,     0,     0,     0,     0,     0,
     334,     0,     0,   181,     0,     0,     0,   335,   182,     0,
     183,     0,     0,     0,     0,     0,     0,     0,   184,     0,
       0,     0,   185,     0,    39,     0,   186,   336,   187,     0,
       0,     0,    41,   263,     0,     0,   188,    67,   337,   338,
       0,    69,   339,   340,    43,     0,     0,     0,     0,     0,
      45,    46,    47,   178,   179,   180,     0,     0,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   243,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,  -199,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,     0,   185,    39,
       0,     0,   186,     0,   187,     0,     0,    41,     0,     0,
     771,     0,   188,    67,     0,   246,     0,    69,     0,    43,
       0,     0,     0,     0,     0,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   243,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,   182,
       0,   183,     0,     0,     0,     0,     0,     0,     0,   184,
       0,     0,     0,   185,    39,     0,     0,   186,     0,   187,
       0,     0,    41,     0,     0,   771,     0,   188,    67,   217,
     246,     0,    69,     0,    43,     0,     0,     0,     0,     0,
      45,    46,    47,   178,   179,   180,     0,   218,     0,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,    39,     0,     0,   185,     0,
       0,     0,   186,    41,   187,     0,     0,     0,     0,     0,
       0,     0,   188,    67,     0,    43,     0,    69,     0,   327,
       0,    45,    46,    47,   178,   179,   180,     0,     0,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,   182,     0,   183,     0,     0,
       0,     0,     0,     0,     0,   184,    39,     0,     0,   185,
     328,     0,     0,   186,    41,   187,     0,     0,     0,     0,
       0,   682,     0,   188,    67,     0,    43,     0,    69,     0,
       0,     0,    45,    46,    47,   178,   179,   180,     0,   683,
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
       0,     0,     0,     0,     0,     0,   356,   181,     0,     0,
       0,     0,   182,     0,   183,     0,     0,     0,     0,     0,
       0,     0,   184,    39,     0,     0,   185,     0,     0,     0,
     186,    41,   187,     0,     0,     0,     0,     0,     0,     0,
     188,    67,     0,    43,     0,    69,     0,   327,     0,    45,
      46,    47,   178,   179,   180,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
       0,     0,     0,   182,     0,   183,     0,     0,     0,     0,
       0,     0,     0,   184,    39,     0,     0,   185,     0,     0,
       0,   186,    41,   187,     0,     0,     0,     0,     0,     0,
       0,   188,    67,     0,    43,     0,    69,     0,   556,     0,
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
       0,     0,     0,     0,     0,   181,     0,     0,     0,     0,
     182,     0,   183,     0,     0,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,    39,     0,     0,   186,   778,
     187,     0,     0,    41,     0,     0,     0,     0,   188,    67,
       0,     0,     0,    69,     0,    43,     0,     0,     0,   327,
       0,    45,    46,    47,   178,   179,   180,     0,     0,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,    39,     0,     0,     0,   182,     0,   183,     0,    41,
       0,     0,     0,     0,     0,   184,     0,     0,     0,   185,
       0,    43,     0,   795,     0,   187,     0,    45,    46,    47,
     178,   179,   180,   188,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   804,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,    39,     0,     0,
       0,   182,     0,   183,     0,    41,     0,     0,     0,     0,
       0,   184,     0,     0,     0,   185,     0,    43,     0,   186,
       0,   187,     0,    45,    46,    47,   178,   179,   180,   188,
      67,     0,    53,    54,    69,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   807,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,    39,     0,     0,     0,   182,     0,   183,
       0,    41,     0,     0,     0,     0,     0,   184,     0,     0,
       0,   185,     0,    43,     0,   186,     0,   187,     0,    45,
      46,    47,   178,   179,   180,   188,    67,     0,    53,    54,
      69,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
       0,     0,     0,   182,     0,   183,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,   185,    39,     0,
       0,   186,    40,   187,     0,     0,    41,     0,     0,     0,
       0,   188,    67,     0,     0,    42,    69,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    39,    55,    56,    57,     0,
       0,    58,     0,    41,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,     0,     0,   309,
       0,     0,     0,     0,     0,     0,    66,    67,     0,     0,
      68,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       0,     0,     0,    65,     0,     0,     0,    41,     0,     0,
       0,     0,     0,    66,    67,     0,   577,    68,    69,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    39,    55,    56,    57,
       0,     0,    58,     0,    41,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,    65,    41,     0,
       0,     0,     0,     0,     0,     0,     0,    66,    67,     0,
      43,    68,    69,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    39,    55,    56,
      57,     0,     0,    58,    65,    41,     0,    59,    60,    61,
      62,    63,    64,     0,    66,    67,     0,    43,    68,    69,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,     0,    41,     0,
       0,   454,     0,     0,     0,     0,     0,     0,    66,   114,
      43,     0,    68,   115,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,   229,    39,    59,    60,    61,
      62,    63,    64,     0,    41,    66,     0,     0,     0,    68,
       0,     0,     0,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,    39,
       0,     0,     0,     0,     0,     0,     0,    41,   229,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    66,    43,
       0,     0,    68,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,    39,     0,     0,     0,     0,     0,     0,     0,
      41,     0,     0,     0,    66,   114,     0,     0,     0,     0,
       0,     0,    43,     0,     0,     0,     0,     0,    45,    46,
      47,   178,   179,   180,     0,    39,     0,    53,    54,     0,
      55,    56,    57,    41,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,    43,     0,   321,     0,    44,
       0,    45,    46,    47,    48,    49,    50,    66,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    39,   708,
       0,     0,     0,     0,     0,     0,    41,     0,     0,     0,
       0,   709,   596,     0,     0,     0,     0,     0,    43,     0,
     597,     0,     0,     0,    45,    46,    47,   178,   179,   180,
     188,    39,     0,    53,    54,     0,    55,    56,    57,    41,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,    43,     0,     0,     0,     0,     0,    45,    46,    47,
     178,   179,   180,    66,     0,     0,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   595,   596,     0,
       0,     0,     0,     0,     0,     0,   597,    39,     0,     0,
       0,     0,     0,     0,     0,    41,   188,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    43,     0,     0,
     768,   596,     0,    45,    46,    47,   178,   179,   180,   597,
      39,     0,    53,    54,     0,    55,    56,    57,    41,   188,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
      43,     0,     0,     0,     0,     0,    45,    46,    47,   178,
     179,   180,     0,     0,     0,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   596,     0,     0,
       0,     0,     0,     0,     0,   597,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188
  };

  const short
  parser::yycheck_[] =
  {
      89,   351,   106,   192,    89,    89,    40,    29,    42,   106,
     318,   100,   280,   111,   112,   100,   217,   113,    29,   314,
      34,   261,   443,   207,    65,   286,    65,   102,   100,   184,
     111,   284,   221,   232,     1,   213,   214,   557,   557,   640,
     292,     5,   185,     1,     1,   473,   480,     1,    34,   641,
      84,   716,     1,    19,   556,    27,   181,   182,   650,   299,
     203,     1,     1,   363,   112,    12,   118,    19,    81,   194,
      79,   112,    80,   112,   100,   370,   103,   175,    90,   278,
     107,   108,    27,   110,   518,     0,   133,   716,   101,   384,
     186,   187,   181,   182,   133,    29,   185,   186,    80,   123,
     105,    21,   105,   127,   113,   194,   101,   105,   135,   104,
     119,   119,    84,   202,   203,   118,   125,   202,   710,    71,
     118,    77,   134,   212,   482,   111,   112,   175,   217,   218,
     177,    77,   217,   218,   175,   112,   175,   119,   177,    84,
     107,   499,   108,   645,   166,   810,   168,   408,   448,   107,
     107,   118,   748,   107,   406,   166,   584,   168,   107,   517,
     118,   118,   596,   597,   118,   112,    77,   107,   107,   118,
     184,   105,   297,   119,    14,   415,   202,   133,   473,   118,
      80,   810,    88,   812,   104,   274,    22,    93,   229,   175,
     229,   217,   218,   282,   283,   108,   792,   282,   282,   213,
     214,    80,   249,    77,   562,   639,   170,   171,   297,    79,
     249,   109,   239,    80,   241,    51,    52,    80,   119,   119,
     133,   119,   133,    93,   245,   133,    96,   213,   214,   318,
      80,   356,   166,   103,   168,   245,   837,   278,   105,    80,
     119,   142,    77,   551,   552,   119,   838,   261,   391,   841,
     120,    78,   673,    77,   124,   125,   119,    84,   128,   129,
     349,   350,   351,   352,   349,    80,   287,   356,   254,   119,
     284,   119,   525,    27,   301,   625,   112,   287,   119,   713,
     321,   315,   113,   185,   186,   299,   300,   123,   119,   364,
      24,   127,   119,   227,   142,   590,   400,   118,   204,   205,
     389,   203,   391,   400,   119,   825,   825,   568,   497,    93,
     212,   409,    96,   411,   748,   610,   112,   738,   390,   103,
     418,   113,   420,   349,   479,   429,   352,   123,   409,   351,
     411,   127,   105,   105,   574,   513,   383,   418,   112,   420,
     124,   275,   276,   486,   383,   118,   118,   436,   101,   123,
      79,   436,   436,   127,   112,   105,   555,    86,   792,   502,
     113,   795,   118,   108,    93,   412,   300,   112,   118,   114,
     631,   108,   274,   412,   105,   112,    37,   114,   123,   285,
     286,   283,   127,   741,   113,   743,   123,   118,   477,   747,
     119,   480,   112,   482,   483,   124,   125,   486,   706,   128,
     129,   415,   697,   115,   605,    79,    14,   119,   758,   670,
     499,   108,    86,   502,   113,   429,   318,    91,   113,    93,
     119,    79,   115,   409,   119,   411,   119,   516,   517,   518,
      65,    66,   418,   617,   420,    93,   532,   533,    96,    87,
     618,   619,   464,   113,   134,   103,   120,   469,   350,   119,
     124,   125,   802,   464,   128,   129,   115,   104,   770,   106,
     119,   773,   551,   552,   115,   479,   124,   125,   119,   133,
     128,   129,   113,   562,   824,   573,   490,   491,   104,   385,
     106,   682,   777,    79,    93,   113,   754,    96,   104,   391,
     106,   113,   573,   733,   103,   113,   402,    93,   104,   513,
     106,   407,   408,   409,   490,   491,   595,   596,   597,    80,
      81,   525,   807,   808,   555,   124,   605,   716,   113,   128,
     605,    80,    81,   648,    79,    80,   834,   513,   124,   125,
     464,     4,   128,   129,    23,   469,   625,   104,    93,   106,
     104,    96,   106,   577,    72,    73,    74,    75,   103,    84,
     639,    86,   753,   642,   576,    80,    81,   642,   572,   648,
     574,   104,   104,   106,   106,   120,   472,   473,   119,   124,
     125,    80,    81,   128,   129,   477,    27,   766,   756,   605,
      91,   483,   121,   122,   486,   118,    77,   573,    15,    77,
      77,   716,   681,   682,   683,    77,   681,   682,   683,   505,
     502,   507,    81,   509,   618,   619,    41,   109,   109,   119,
     113,   810,    71,   702,   516,   113,   642,   706,   113,   708,
     709,   133,   119,   708,   713,   709,   119,   716,   829,   651,
     113,   105,   618,   619,   732,   113,   105,   726,   572,   109,
      80,   726,   576,   665,    91,   770,   113,   113,   773,   551,
     552,   732,   741,   113,   743,   681,   682,   683,   747,   748,
      80,   567,   568,   119,   753,   113,   119,   771,   753,   758,
     759,   760,   776,   687,   759,   760,    85,    85,   584,   776,
      25,   770,   708,   709,   773,   810,    27,   812,    81,   133,
     109,   134,    77,   595,   783,    77,   113,    77,   783,   795,
     726,   687,   120,   792,   120,   727,   795,    77,    80,    80,
      27,   800,   119,   802,    81,   800,   800,   119,   105,   733,
     809,   810,   120,   812,   809,   631,   632,   753,   817,   119,
      81,   665,   817,   759,   760,   824,    79,   101,   101,   112,
     829,   120,   756,   120,   829,   834,   732,   113,    84,   109,
      93,   109,    78,    96,   119,    77,    91,   783,   120,     4,
     103,   120,    28,   669,   670,   671,    27,    12,     9,   113,
     756,   105,   118,   113,    37,    27,    80,   120,    79,    24,
     113,   124,   125,   809,    84,    30,    31,    32,    33,    34,
      35,   817,    93,    80,    39,    40,    11,    42,    43,    44,
     702,    19,    47,   829,   706,    86,    51,    52,    53,    54,
      55,    56,     4,    91,    53,    86,    91,   123,   113,   120,
      12,   119,   105,   124,   125,   113,    84,   128,   129,    84,
     579,   105,    24,   124,   166,   576,   665,   824,    30,    31,
      32,    33,    34,    35,   760,   751,   748,    39,    40,   693,
      42,    43,    44,    34,   744,    47,   282,   746,   800,    51,
      52,    53,    54,    55,    56,   687,   491,   112,   756,     3,
       4,     5,     6,     7,     8,   619,    10,   122,    12,    13,
      15,   787,    16,    17,    18,    19,    20,   214,    22,   702,
      24,   274,    26,   795,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,   552,    42,    43,
      44,   203,   817,    47,   518,   721,    50,    51,    52,    53,
      54,    55,    56,    57,   714,   770,    79,   814,   653,    63,
     122,   123,   834,    86,   479,   127,   727,    34,   109,   289,
      93,   669,    76,   670,   507,   239,   205,   673,    82,   572,
     400,   776,   555,   399,    -1,    89,   249,    -1,    92,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,    -1,
      -1,   124,   125,    -1,   108,   128,   129,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,    -1,    -1,    -1,    -1,   143,
     144,     3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      22,    -1,    24,    -1,    26,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    79,    -1,
      81,    63,    -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    93,    -1,    76,    96,    -1,    -1,    -1,    -1,
      82,    -1,   103,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   120,
      -1,    -1,    -1,   124,   125,    -1,   108,   128,   129,    -1,
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
      -1,   112,     8,   114,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   122,   123,    -1,    -1,    21,   127,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,    78,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,   112,    -1,    -1,    -1,    12,    -1,    -1,
      -1,    -1,    -1,   122,   123,    -1,    21,   126,   127,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,   112,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,
      24,   126,   127,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,   112,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,   122,   123,    -1,    24,   126,   127,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
      24,    -1,   126,   127,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,   112,     4,    51,    52,    53,
      54,    55,    56,    -1,    12,   122,    -1,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    24,
      -1,    -1,   126,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    -1,    -1,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,     4,    -1,    39,    40,    -1,
      42,    43,    44,    12,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    24,    -1,   112,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,   122,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,     4,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   103,   104,    -1,    -1,    -1,    -1,    -1,    24,    -1,
     112,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
     122,     4,    -1,    39,    40,    -1,    42,    43,    44,    12,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,   122,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,
     103,   104,    -1,    30,    31,    32,    33,    34,    35,   112,
       4,    -1,    39,    40,    -1,    42,    43,    44,    12,   122,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   146,   147,   148,   151,   123,   127,   352,
     152,   163,     0,   152,    65,    66,   149,   105,   118,   153,
     164,   165,     1,   107,   351,   108,   133,   219,   219,   112,
     154,    14,   166,   177,   178,   133,   220,    77,    77,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   112,   122,   123,   126,   127,
     142,   155,   156,   157,   161,   318,   321,   322,   335,   337,
     338,   344,    27,    24,   167,   118,   162,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    29,    36,    50,    57,    63,    76,    82,    89,    92,
      94,   108,   112,   114,   123,   127,   132,   133,   134,   135,
     138,   143,   144,   175,   179,   180,   181,   182,   184,   199,
     224,   264,   269,   273,   274,   276,   277,   278,   279,   307,
     308,   311,   312,   334,   335,   338,   346,   347,   350,   109,
     119,   352,   352,    79,    93,    96,   103,   124,   125,   128,
     129,   341,   342,   343,   345,   113,   119,   142,   112,   158,
     104,   106,   150,   352,   118,   112,   272,   273,    33,    34,
      35,    89,    94,    96,   104,   108,   112,   114,   122,   204,
     229,   231,   233,   234,   236,   240,   316,   317,   321,   331,
     333,   344,    37,   112,   104,   106,   299,   272,    72,    73,
      74,    75,   185,   104,   106,   216,   217,    19,    37,   183,
     231,   233,   317,    14,   299,   277,   108,   270,   271,   112,
     335,   108,   112,   309,   310,   312,   347,   277,   297,   298,
     277,   276,   277,    79,   109,   120,   125,   129,   272,   273,
     281,   283,   284,   314,   328,   330,   340,   341,   343,   348,
     349,   102,   113,   119,   280,   281,   282,   341,   342,   343,
     348,   353,   115,   353,    19,   270,   270,   134,   174,   162,
      19,    71,   206,    80,   119,    81,    84,   120,   266,   267,
     268,   314,   327,   329,   339,   341,   342,    88,   277,   101,
     104,    87,   133,   113,   113,   113,   113,   113,   157,    78,
     159,   160,   161,   152,   152,     4,   168,    23,    80,   240,
     240,   112,   224,   257,   258,   259,   338,    28,   109,   226,
     227,   229,   231,    79,    86,    93,   113,   124,   125,   128,
     129,   226,   244,   323,   324,   353,   353,    27,   210,    91,
      86,   120,   238,   319,   323,   332,    88,   237,   240,   231,
     226,   243,   244,    20,    46,   272,   296,   300,   301,   302,
     300,   118,   275,    77,    77,    77,    77,   222,   227,   241,
     215,   264,   265,   273,   215,    15,   194,   231,   231,    80,
     119,    81,    41,    90,   134,   335,    77,   133,   349,    80,
     119,   221,    86,   297,   337,   346,   327,    78,    84,   119,
     109,   119,   273,   336,   338,   101,   113,   113,   119,   113,
     119,   113,   113,   113,   119,   115,   241,   335,   335,   120,
     176,   313,   325,   326,   342,   349,   206,   133,   204,   223,
     227,   228,   334,   272,   286,   287,   302,   337,    27,   218,
     268,   274,   240,   336,    78,   303,   304,   305,   335,   336,
     338,   277,   113,   113,   119,   105,   351,   352,    12,   112,
     169,   170,   104,   106,   288,   222,   342,    80,   105,   119,
     245,   109,    80,    91,   113,   113,   119,   113,   113,   115,
     104,   106,   209,   231,   227,   321,   333,   233,   240,    80,
     201,   113,   119,   217,   299,    85,   105,   118,   351,    25,
      27,   214,   105,   118,   351,   272,    81,    80,    81,   202,
     227,   248,   112,   317,   226,   133,   134,   109,    77,    77,
     113,   108,   112,   114,   315,   316,   309,    77,   272,   120,
     120,   272,   285,   302,   272,   281,   281,   336,   281,   281,
      77,    80,    80,   338,   347,   119,    28,   205,   229,   231,
      77,   133,    80,    81,   200,   252,   218,    81,   119,   120,
     217,   105,   119,    81,   101,   161,   112,    21,   142,   161,
     171,   172,   173,   105,   118,   272,   289,   290,   291,   295,
     289,   351,   113,   227,   259,   103,   104,   112,   246,   247,
     331,   248,   227,   226,     7,    26,   195,   207,   208,   265,
     208,   120,   120,   248,   226,   272,   301,   272,   104,   106,
     213,   265,   227,   248,   246,    84,   188,   334,   109,   113,
     115,   119,    78,   222,   225,   225,   120,   120,   325,   245,
     200,   252,    91,    77,   248,    28,   253,   254,   255,    27,
     249,     9,   260,   261,   262,   272,   302,   304,   281,   306,
     336,   171,   352,   158,   113,   119,   142,   289,   105,   118,
      84,    86,   292,   293,   294,   351,   227,   331,   331,   113,
      37,   196,    19,    37,   194,   231,   105,   118,   351,   275,
       7,    26,   198,   199,   211,   212,   265,   212,   189,   333,
      27,   191,    80,   302,   272,    77,   119,    77,    91,   103,
     249,   260,   231,   245,    84,   230,   235,   239,   240,   256,
     104,   106,   260,    22,    51,    52,   112,   186,   263,   320,
     321,   262,    81,   101,   113,   173,   291,   286,   272,   218,
     294,    80,   105,    80,   231,   194,   231,    80,    81,   203,
     207,    11,    19,   197,   197,   105,   118,   351,    86,   104,
     106,   192,   223,   222,   231,   229,   233,   260,   103,   254,
      91,   120,   239,   313,   250,   251,   275,   250,   113,   231,
     232,   242,   263,    53,   187,   281,   336,    86,   218,   248,
     248,   201,    81,   203,   248,   112,   247,   331,   272,   194,
     206,   211,   190,   333,    78,   193,   194,    78,   193,    91,
     235,   256,   235,   105,   118,   310,   351,   119,   113,   231,
     272,   105,   113,   331,    84,   205,   333,   105,   105,   118,
     351,   351,   231,   251,    80,   242,   189,   200,   252,   194,
     222,   249,   260,   260
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
     199,   200,   200,   201,   201,   202,   202,   202,   203,   203,
     203,   204,   204,   205,   205,   205,   205,   206,   206,   206,
     207,   207,   208,   208,   208,   208,   209,   209,   210,   210,
     211,   211,   212,   212,   212,   212,   213,   213,   214,   214,
     215,   215,   215,   215,   216,   216,   217,   218,   218,   219,
     219,   220,   220,   220,   221,   221,   222,   223,   224,   224,
     225,   225,   226,   226,   227,   227,   227,   228,   229,   230,
     231,   231,   232,   233,   234,   234,   235,   235,   236,   236,
     236,   237,   238,   238,   239,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   241,   242,   242,   243,   243,
     244,   244,   245,   245,   246,   246,   246,   247,   247,   248,
     249,   249,   249,   250,   250,   251,   252,   253,   253,   254,
     254,   255,   255,   256,   256,   257,   257,   258,   258,   259,
     260,   260,   261,   261,   262,   262,   262,   263,   263,   263,
     264,   264,   265,   266,   266,   267,   267,   268,   269,   269,
     269,   269,   269,   269,   269,   269,   269,   270,   270,   271,
     271,   272,   272,   273,   273,   274,   274,   275,   275,   276,
     276,   276,   276,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   277,   278,   278,   278,   279,   279,   279,   279,
     279,   279,   279,   279,   280,   280,   281,   281,   281,   282,
     282,   283,   283,   283,   283,   283,   283,   283,   284,   284,
     285,   285,   286,   287,   287,   288,   288,   288,   288,   289,
     289,   290,   290,   290,   291,   292,   292,   293,   293,   294,
     295,   296,   297,   298,   298,   299,   299,   300,   300,   300,
     300,   301,   301,   302,   302,   302,   303,   303,   304,   304,
     304,   305,   305,   305,   305,   306,   306,   307,   307,   308,
     308,   309,   309,   309,   310,   310,   311,   311,   311,   311,
     312,   312,   313,   313,   314,   314,   315,   315,   315,   316,
     316,   316,   316,   316,   317,   317,   318,   318,   318,   318,
     319,   319,   320,   321,   321,   322,   323,   323,   323,   324,
     324,   324,   324,   325,   325,   326,   326,   327,   327,   328,
     328,   329,   329,   330,   330,   331,   332,   333,   333,   333,
     333,   333,   334,   334,   335,   335,   335,   336,   337,   337,
     338,   338,   338,   338,   338,   338,   338,   338,   339,   339,
     340,   340,   341,   342,   342,   343,   343,   344,   344,   344,
     344,   344,   344,   344,   344,   344,   344,   344,   344,   344,
     344,   344,   344,   344,   344,   345,   345,   346,   346,   347,
     348,   348,   349,   349,   350,   350,   350,   350,   350,   351,
     351,   352,   352,   353,   353
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
       0,     1,     1,     1,     1,     4,     7,     1,     1,     3,
       4,     5,     6,     6,     4,     4,     3,     1,     4,     3,
       6,     7,     2,     2,     2,     2,     0,     1,     1,     1,
       2,     0,     2,     3,     2,     1,     0,     2,     3,     3,
       3,     3,     3,     2,     1,     0,     3,     4,     3,     4,
       2,     3,     0,     1,     0,     1,     3,     6,     7,     1,
       1,     0,     2,     0,     2,     0,     2,     2,     0,     2,
       4,     3,     1,     6,     4,     3,     1,     4,     3,     0,
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
  "at_decl_inst", "data_or_newtype", "opt_kind_sig",
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
       0,   515,   515,   532,   533,   535,   539,   540,   541,   543,
     544,   546,   547,   550,   552,   553,   554,   562,   563,   565,
     566,   567,   568,   570,   571,   573,   574,   575,   577,   578,
     579,   581,   582,   584,   585,   587,   588,   592,   593,   595,
     596,   598,   600,   601,   603,   616,   617,   619,   620,   622,
     623,   627,   628,   630,   631,   632,   633,   635,   636,   638,
     639,   644,   645,   647,   648,   649,   651,   652,   656,   658,
     659,   661,   662,   663,   664,   667,   668,   674,   676,   679,
     682,   683,   685,   686,   687,   689,   691,   692,   695,   696,
     697,   703,   710,   711,   712,   713,   714,   716,   717,   718,
     720,   731,   732,   734,   736,   737,   741,   742,   744,   745,
     746,   747,   749,   750,   751,   752,   754,   757,   759,   761,
     763,   764,   766,   766,   768,   768,   772,   774,   781,   788,
     789,   793,   794,   796,   797,   799,   800,   801,   803,   804,
     805,   808,   809,   812,   813,   814,   815,   817,   818,   819,
     861,   862,   864,   865,   866,   867,   869,   870,   872,   873,
     875,   876,   878,   879,   880,   881,   883,   884,   886,   887,
     890,   891,   892,   893,   895,   896,   898,   900,   901,   909,
     910,   912,   913,   914,   927,   928,   937,   939,   941,   942,
     944,   945,   954,   955,   957,   958,   960,   962,   971,   973,
     975,   976,   978,   981,   983,   984,   986,   987,   989,   991,
     992,   994,   996,   997,  1004,  1011,  1012,  1013,  1014,  1015,
    1016,  1017,  1018,  1024,  1025,  1028,  1030,  1031,  1033,  1034,
    1036,  1037,  1044,  1045,  1047,  1048,  1049,  1052,  1053,  1071,
    1077,  1078,  1079,  1081,  1082,  1084,  1086,  1088,  1089,  1091,
    1092,  1094,  1095,  1097,  1098,  1104,  1105,  1107,  1108,  1110,
    1112,  1113,  1115,  1116,  1118,  1119,  1120,  1122,  1123,  1124,
    1129,  1131,  1133,  1137,  1138,  1140,  1141,  1145,  1155,  1156,
    1158,  1159,  1160,  1161,  1162,  1163,  1164,  1167,  1168,  1170,
    1171,  1176,  1177,  1181,  1182,  1184,  1185,  1187,  1188,  1193,
    1194,  1195,  1196,  1199,  1200,  1201,  1202,  1203,  1205,  1207,
    1208,  1209,  1211,  1214,  1215,  1216,  1219,  1220,  1221,  1222,
    1223,  1224,  1229,  1230,  1233,  1234,  1239,  1240,  1241,  1246,
    1247,  1265,  1266,  1267,  1268,  1269,  1270,  1271,  1273,  1274,
    1287,  1289,  1299,  1301,  1302,  1305,  1306,  1307,  1308,  1310,
    1311,  1313,  1314,  1315,  1317,  1319,  1320,  1322,  1323,  1332,
    1334,  1336,  1338,  1340,  1341,  1344,  1345,  1347,  1348,  1349,
    1350,  1355,  1356,  1358,  1359,  1360,  1365,  1366,  1368,  1369,
    1370,  1372,  1373,  1374,  1375,  1378,  1379,  1411,  1412,  1414,
    1415,  1417,  1418,  1419,  1421,  1422,  1424,  1425,  1426,  1427,
    1429,  1430,  1432,  1433,  1435,  1436,  1439,  1440,  1441,  1443,
    1444,  1445,  1446,  1447,  1449,  1450,  1452,  1453,  1454,  1455,
    1458,  1459,  1461,  1463,  1464,  1468,  1470,  1471,  1472,  1474,
    1475,  1476,  1477,  1482,  1483,  1485,  1486,  1488,  1489,  1492,
    1493,  1498,  1499,  1501,  1502,  1506,  1508,  1510,  1511,  1512,
    1513,  1514,  1517,  1518,  1520,  1521,  1522,  1524,  1526,  1527,
    1529,  1530,  1531,  1532,  1533,  1534,  1535,  1536,  1538,  1539,
    1541,  1542,  1544,  1546,  1547,  1549,  1550,  1552,  1553,  1554,
    1555,  1556,  1557,  1558,  1559,  1560,  1561,  1562,  1563,  1564,
    1565,  1566,  1567,  1568,  1569,  1571,  1572,  1576,  1577,  1579,
    1581,  1582,  1584,  1585,  1589,  1590,  1591,  1592,  1593,  1598,
    1601,  1605,  1606,  1608,  1609
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
#line 7376 "parser.cc"

#line 1618 "parser.y"


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

