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

  case 27: // export_subspec: %empty
#line 576 "parser.y"
                                      {}
#line 2653 "parser.cc"
    break;

  case 28: // export_subspec: "(" qcnames ")"
#line 577 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2659 "parser.cc"
    break;

  case 29: // export_subspec: "(" ".." ")"
#line 578 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2665 "parser.cc"
    break;

  case 30: // qcnames: %empty
#line 580 "parser.y"
                   {}
#line 2671 "parser.cc"
    break;

  case 31: // qcnames: qcnames1
#line 581 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2677 "parser.cc"
    break;

  case 32: // qcnames1: qcnames1 "," qcname
#line 583 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2683 "parser.cc"
    break;

  case 33: // qcnames1: qcname
#line 584 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2689 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 586 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2695 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 587 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2701 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 597 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2707 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 599 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2713 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 600 "parser.y"
                         { }
#line 2719 "parser.cc"
    break;

  case 43: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 602 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2727 "parser.cc"
    break;

  case 44: // optqualified: "qualified"
#line 615 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2733 "parser.cc"
    break;

  case 45: // optqualified: %empty
#line 616 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2739 "parser.cc"
    break;

  case 46: // maybeas: "as" modid
#line 618 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2745 "parser.cc"
    break;

  case 47: // maybeas: %empty
#line 619 "parser.y"
                               { }
#line 2751 "parser.cc"
    break;

  case 48: // maybeimpspec: impspec
#line 621 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2757 "parser.cc"
    break;

  case 49: // maybeimpspec: %empty
#line 622 "parser.y"
                               { }
#line 2763 "parser.cc"
    break;

  case 50: // impspec: "(" importlist ")"
#line 626 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2769 "parser.cc"
    break;

  case 51: // impspec: "hiding" "(" importlist ")"
#line 627 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2775 "parser.cc"
    break;

  case 52: // importlist: importlist1
#line 629 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2781 "parser.cc"
    break;

  case 53: // importlist: %empty
#line 630 "parser.y"
                                      {}
#line 2787 "parser.cc"
    break;

  case 54: // importlist: importlist1 ','
#line 631 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2793 "parser.cc"
    break;

  case 55: // importlist: ','
#line 632 "parser.y"
                                      {}
#line 2799 "parser.cc"
    break;

  case 56: // importlist1: importlist1 "," import
#line 634 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2805 "parser.cc"
    break;

  case 57: // importlist1: import
#line 635 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2811 "parser.cc"
    break;

  case 58: // import: qcname export_subspec
#line 637 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2817 "parser.cc"
    break;

  case 59: // import: "module" modid
#line 638 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2823 "parser.cc"
    break;

  case 60: // prec: %empty
#line 643 "parser.y"
                   { }
#line 2829 "parser.cc"
    break;

  case 61: // prec: "INTEGER"
#line 644 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2835 "parser.cc"
    break;

  case 62: // infix: "infix"
#line 646 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2841 "parser.cc"
    break;

  case 63: // infix: "infixl"
#line 647 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2847 "parser.cc"
    break;

  case 64: // infix: "infixr"
#line 648 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2853 "parser.cc"
    break;

  case 65: // ops: ops "," op
#line 650 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2859 "parser.cc"
    break;

  case 66: // ops: op
#line 651 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2865 "parser.cc"
    break;

  case 67: // topdecls: topdecls_semi topdecl
#line 655 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2871 "parser.cc"
    break;

  case 68: // topdecls_semi: topdecls_semi topdecl semis1
#line 657 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2877 "parser.cc"
    break;

  case 69: // topdecls_semi: %empty
#line 658 "parser.y"
                                            { }
#line 2883 "parser.cc"
    break;

  case 70: // topdecl: cl_decl
#line 660 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2889 "parser.cc"
    break;

  case 71: // topdecl: ty_decl
#line 661 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2895 "parser.cc"
    break;

  case 72: // topdecl: standalone_kind_sig
#line 662 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2901 "parser.cc"
    break;

  case 73: // topdecl: inst_decl
#line 663 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2907 "parser.cc"
    break;

  case 74: // topdecl: "default" "(" comma_types0 ")"
#line 666 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2913 "parser.cc"
    break;

  case 75: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 667 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2919 "parser.cc"
    break;

  case 76: // topdecl: decl_no_th
#line 673 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2925 "parser.cc"
    break;

  case 77: // topdecl: infixexp
#line 675 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2931 "parser.cc"
    break;

  case 78: // cl_decl: "class" tycl_hdr where_cls
#line 678 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2937 "parser.cc"
    break;

  case 79: // ty_decl: "type" type "=" ktype
#line 681 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2943 "parser.cc"
    break;

  case 80: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 682 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2949 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 684 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 2955 "parser.cc"
    break;

  case 82: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 685 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2961 "parser.cc"
    break;

  case 83: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 686 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 2967 "parser.cc"
    break;

  case 84: // standalone_kind_sig: "type" sks_vars "::" kind
#line 688 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 2973 "parser.cc"
    break;

  case 85: // sks_vars: sks_vars "," oqtycon
#line 690 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2979 "parser.cc"
    break;

  case 86: // sks_vars: oqtycon
#line 691 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2985 "parser.cc"
    break;

  case 87: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 694 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2991 "parser.cc"
    break;

  case 88: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 695 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 2997 "parser.cc"
    break;

  case 89: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 697 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 3007 "parser.cc"
    break;

  case 90: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 703 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 3017 "parser.cc"
    break;

  case 91: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 709 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3023 "parser.cc"
    break;

  case 92: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 710 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3029 "parser.cc"
    break;

  case 93: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 711 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3035 "parser.cc"
    break;

  case 94: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 712 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3041 "parser.cc"
    break;

  case 95: // overlap_pragma: %empty
#line 713 "parser.y"
                                               {}
#line 3047 "parser.cc"
    break;

  case 105: // where_type_family: %empty
#line 740 "parser.y"
                                                           {}
#line 3053 "parser.cc"
    break;

  case 106: // where_type_family: "where" ty_fam_inst_eqn_list
#line 741 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3059 "parser.cc"
    break;

  case 107: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 743 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3065 "parser.cc"
    break;

  case 108: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 744 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3071 "parser.cc"
    break;

  case 109: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 745 "parser.y"
                                                           {}
#line 3077 "parser.cc"
    break;

  case 110: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 746 "parser.y"
                                                           {}
#line 3083 "parser.cc"
    break;

  case 111: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 748 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3089 "parser.cc"
    break;

  case 112: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 749 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3095 "parser.cc"
    break;

  case 113: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 750 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3101 "parser.cc"
    break;

  case 114: // ty_fam_inst_eqns: %empty
#line 751 "parser.y"
                                                           {}
#line 3107 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqn: type "=" ctype
#line 753 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3113 "parser.cc"
    break;

  case 116: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 756 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3119 "parser.cc"
    break;

  case 117: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 758 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3125 "parser.cc"
    break;

  case 118: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 760 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3131 "parser.cc"
    break;

  case 119: // at_decl_cls: "type" ty_fam_inst_eqn
#line 762 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3137 "parser.cc"
    break;

  case 120: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 763 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3143 "parser.cc"
    break;

  case 125: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 771 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3149 "parser.cc"
    break;

  case 126: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 774 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3159 "parser.cc"
    break;

  case 127: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 781 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3169 "parser.cc"
    break;

  case 128: // data_or_newtype: "data"
#line 787 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3175 "parser.cc"
    break;

  case 129: // data_or_newtype: "newtype"
#line 788 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3181 "parser.cc"
    break;

  case 130: // opt_kind_sig: %empty
#line 792 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3187 "parser.cc"
    break;

  case 131: // opt_kind_sig: "::" kind
#line 793 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3193 "parser.cc"
    break;

  case 132: // opt_datafam_kind_sig: %empty
#line 795 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3199 "parser.cc"
    break;

  case 133: // opt_datafam_kind_sig: "::" kind
#line 796 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3205 "parser.cc"
    break;

  case 134: // opt_tyfam_kind_sig: %empty
#line 798 "parser.y"
                                      {}
#line 3211 "parser.cc"
    break;

  case 135: // opt_tyfam_kind_sig: "::" kind
#line 799 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3217 "parser.cc"
    break;

  case 136: // opt_tyfam_kind_sig: "=" tv_bndr
#line 800 "parser.y"
                                      {}
#line 3223 "parser.cc"
    break;

  case 137: // opt_at_kind_inj_sig: %empty
#line 802 "parser.y"
                                      {}
#line 3229 "parser.cc"
    break;

  case 138: // opt_at_kind_inj_sig: "::" kind
#line 803 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3235 "parser.cc"
    break;

  case 139: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 804 "parser.y"
                                                                  {}
#line 3241 "parser.cc"
    break;

  case 140: // tycl_hdr: context "=>" type
#line 807 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3247 "parser.cc"
    break;

  case 141: // tycl_hdr: type
#line 808 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3253 "parser.cc"
    break;

  case 142: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 811 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3259 "parser.cc"
    break;

  case 143: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 812 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3265 "parser.cc"
    break;

  case 144: // datafam_inst_hdr: context "=>" type
#line 813 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3271 "parser.cc"
    break;

  case 145: // datafam_inst_hdr: type
#line 814 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3277 "parser.cc"
    break;

  case 149: // decl_cls: at_decl_cls
#line 860 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3283 "parser.cc"
    break;

  case 150: // decl_cls: decl
#line 861 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3289 "parser.cc"
    break;

  case 151: // decls_cls: decls_cls ";" decl_cls
#line 863 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3295 "parser.cc"
    break;

  case 152: // decls_cls: decls_cls ";"
#line 864 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3301 "parser.cc"
    break;

  case 153: // decls_cls: decl_cls
#line 865 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3307 "parser.cc"
    break;

  case 154: // decls_cls: %empty
#line 866 "parser.y"
                                           {}
#line 3313 "parser.cc"
    break;

  case 155: // decllist_cls: "{" decls_cls "}"
#line 868 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3319 "parser.cc"
    break;

  case 156: // decllist_cls: "vocurly" decls_cls close
#line 869 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3325 "parser.cc"
    break;

  case 157: // where_cls: "where" decllist_cls
#line 871 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3331 "parser.cc"
    break;

  case 158: // where_cls: %empty
#line 872 "parser.y"
                                           {}
#line 3337 "parser.cc"
    break;

  case 159: // decl_inst: at_decl_inst
#line 874 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3343 "parser.cc"
    break;

  case 160: // decl_inst: decl
#line 875 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3349 "parser.cc"
    break;

  case 161: // decls_inst: decls_inst ";" decl_inst
#line 877 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3355 "parser.cc"
    break;

  case 162: // decls_inst: decls_inst ";"
#line 878 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3361 "parser.cc"
    break;

  case 163: // decls_inst: decl_inst
#line 879 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3367 "parser.cc"
    break;

  case 164: // decls_inst: %empty
#line 880 "parser.y"
                                           {}
#line 3373 "parser.cc"
    break;

  case 165: // decllist_inst: "{" decls_inst "}"
#line 882 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3379 "parser.cc"
    break;

  case 166: // decllist_inst: "vocurly" decls_inst close
#line 883 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3385 "parser.cc"
    break;

  case 167: // where_inst: "where" decllist_inst
#line 885 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3391 "parser.cc"
    break;

  case 168: // where_inst: %empty
#line 886 "parser.y"
                                           {}
#line 3397 "parser.cc"
    break;

  case 169: // decls: decls ";" decl
#line 889 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3403 "parser.cc"
    break;

  case 170: // decls: decls ";"
#line 890 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3409 "parser.cc"
    break;

  case 171: // decls: decl
#line 891 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3415 "parser.cc"
    break;

  case 172: // decls: %empty
#line 892 "parser.y"
                        {}
#line 3421 "parser.cc"
    break;

  case 173: // decllist: "{" decls "}"
#line 894 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3427 "parser.cc"
    break;

  case 174: // decllist: "vocurly" decls close
#line 895 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3433 "parser.cc"
    break;

  case 175: // binds: decllist
#line 897 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3439 "parser.cc"
    break;

  case 176: // wherebinds: "where" binds
#line 899 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3445 "parser.cc"
    break;

  case 177: // wherebinds: %empty
#line 900 "parser.y"
                                 {}
#line 3451 "parser.cc"
    break;

  case 183: // opt_tyconsig: %empty
#line 926 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3457 "parser.cc"
    break;

  case 184: // opt_tyconsig: "::" gtycon
#line 927 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3463 "parser.cc"
    break;

  case 185: // sigtype: ctype
#line 936 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3469 "parser.cc"
    break;

  case 186: // sigtypedoc: ctypedoc
#line 938 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3475 "parser.cc"
    break;

  case 187: // sig_vars: sig_vars "," var
#line 940 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3481 "parser.cc"
    break;

  case 188: // sig_vars: var
#line 941 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3487 "parser.cc"
    break;

  case 189: // sigtypes1: sigtype
#line 943 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3493 "parser.cc"
    break;

  case 190: // sigtypes1: sigtypes1 "," sigtype
#line 944 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3499 "parser.cc"
    break;

  case 191: // ktype: ctype
#line 953 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3505 "parser.cc"
    break;

  case 192: // ktype: ctype "::" kind
#line 954 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3511 "parser.cc"
    break;

  case 193: // ctype: "forall" tv_bndrs "." ctype
#line 956 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3517 "parser.cc"
    break;

  case 194: // ctype: context "=>" ctype
#line 957 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3523 "parser.cc"
    break;

  case 195: // ctype: type
#line 959 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3529 "parser.cc"
    break;

  case 196: // ctypedoc: ctype
#line 961 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3535 "parser.cc"
    break;

  case 197: // context: btype
#line 970 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3541 "parser.cc"
    break;

  case 198: // context_no_ops: btype_no_ops
#line 972 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3547 "parser.cc"
    break;

  case 199: // type: btype
#line 974 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3553 "parser.cc"
    break;

  case 200: // type: btype "->" ctype
#line 975 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3559 "parser.cc"
    break;

  case 201: // typedoc: type
#line 977 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3565 "parser.cc"
    break;

  case 202: // btype: infixtype
#line 980 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3571 "parser.cc"
    break;

  case 203: // infixtype: ftype
#line 982 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3577 "parser.cc"
    break;

  case 204: // infixtype: btype tyop btype
#line 983 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3583 "parser.cc"
    break;

  case 205: // btype_no_ops: atype_docs
#line 985 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3589 "parser.cc"
    break;

  case 206: // btype_no_ops: btype_no_ops atype_docs
#line 986 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3595 "parser.cc"
    break;

  case 207: // ftype: atype
#line 988 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3601 "parser.cc"
    break;

  case 208: // ftype: ftype tyarg
#line 990 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3607 "parser.cc"
    break;

  case 209: // ftype: ftype "@" atype
#line 991 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3613 "parser.cc"
    break;

  case 210: // tyarg: atype
#line 993 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3619 "parser.cc"
    break;

  case 211: // tyop: qtyconop
#line 995 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3625 "parser.cc"
    break;

  case 212: // tyop: tyvarop
#line 996 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3631 "parser.cc"
    break;

  case 213: // atype_docs: atype
#line 1003 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3637 "parser.cc"
    break;

  case 214: // atype: ntgtycon
#line 1010 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3643 "parser.cc"
    break;

  case 215: // atype: tyvar
#line 1011 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3649 "parser.cc"
    break;

  case 216: // atype: "*"
#line 1012 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3655 "parser.cc"
    break;

  case 217: // atype: PREFIX_BANG atype
#line 1013 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3661 "parser.cc"
    break;

  case 218: // atype: PREFIX_TILDE atype
#line 1014 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3667 "parser.cc"
    break;

  case 219: // atype: "{" fielddecls "}"
#line 1015 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3673 "parser.cc"
    break;

  case 220: // atype: "(" ")"
#line 1016 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3679 "parser.cc"
    break;

  case 221: // atype: "(" comma_types1 "," ktype ")"
#line 1017 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3685 "parser.cc"
    break;

  case 222: // atype: "[" ktype "]"
#line 1023 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3691 "parser.cc"
    break;

  case 223: // atype: "(" ktype ")"
#line 1024 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3697 "parser.cc"
    break;

  case 224: // inst_type: sigtype
#line 1027 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3703 "parser.cc"
    break;

  case 227: // comma_types0: comma_types1
#line 1032 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3709 "parser.cc"
    break;

  case 228: // comma_types0: %empty
#line 1033 "parser.y"
                                       { /* default construction OK */ }
#line 3715 "parser.cc"
    break;

  case 229: // comma_types1: ktype
#line 1035 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3721 "parser.cc"
    break;

  case 230: // comma_types1: comma_types1 "," ktype
#line 1036 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3727 "parser.cc"
    break;

  case 231: // tv_bndrs: tv_bndrs tv_bndr
#line 1043 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3733 "parser.cc"
    break;

  case 232: // tv_bndrs: %empty
#line 1044 "parser.y"
                               { /* default construction OK */}
#line 3739 "parser.cc"
    break;

  case 233: // tv_bndr: tv_bndr_no_braces
#line 1046 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3745 "parser.cc"
    break;

  case 234: // tv_bndr: "{" tyvar "}"
#line 1047 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3751 "parser.cc"
    break;

  case 235: // tv_bndr: "{" tyvar "::" kind "}"
#line 1048 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3757 "parser.cc"
    break;

  case 236: // tv_bndr_no_braces: tyvar
#line 1051 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3763 "parser.cc"
    break;

  case 237: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1052 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3769 "parser.cc"
    break;

  case 238: // kind: ctype
#line 1070 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3775 "parser.cc"
    break;

  case 239: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1076 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3781 "parser.cc"
    break;

  case 240: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1077 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3787 "parser.cc"
    break;

  case 241: // gadt_constrlist: %empty
#line 1078 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3793 "parser.cc"
    break;

  case 242: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1080 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3799 "parser.cc"
    break;

  case 243: // gadt_constrs: gadt_constr
#line 1081 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3805 "parser.cc"
    break;

  case 244: // gadt_constr: optSemi con_list "::" sigtype
#line 1083 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3811 "parser.cc"
    break;

  case 245: // constrs: "=" constrs1
#line 1085 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3817 "parser.cc"
    break;

  case 246: // constrs1: constrs1 "|" constr
#line 1087 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3823 "parser.cc"
    break;

  case 247: // constrs1: constr
#line 1088 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3829 "parser.cc"
    break;

  case 248: // constr: forall context_no_ops "=>" constr_stuff
#line 1090 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3835 "parser.cc"
    break;

  case 249: // constr: forall constr_stuff
#line 1091 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3841 "parser.cc"
    break;

  case 250: // forall: "forall" tv_bndrs "."
#line 1093 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3847 "parser.cc"
    break;

  case 251: // forall: %empty
#line 1094 "parser.y"
                                {}
#line 3853 "parser.cc"
    break;

  case 252: // constr_stuff: btype_no_ops
#line 1096 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3859 "parser.cc"
    break;

  case 253: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1097 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3869 "parser.cc"
    break;

  case 254: // fielddecls: %empty
#line 1103 "parser.y"
                                {}
#line 3875 "parser.cc"
    break;

  case 255: // fielddecls: fielddecls1
#line 1104 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3881 "parser.cc"
    break;

  case 256: // fielddecls1: fielddecls1 "," fielddecl
#line 1106 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3887 "parser.cc"
    break;

  case 257: // fielddecls1: fielddecl
#line 1107 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3893 "parser.cc"
    break;

  case 258: // fielddecl: sig_vars "::" ctype
#line 1109 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3899 "parser.cc"
    break;

  case 269: // decl_no_th: sigdecl
#line 1128 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3905 "parser.cc"
    break;

  case 270: // decl_no_th: infixexp rhs
#line 1130 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3911 "parser.cc"
    break;

  case 271: // decl: decl_no_th
#line 1132 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3917 "parser.cc"
    break;

  case 272: // rhs: "=" exp wherebinds
#line 1136 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3923 "parser.cc"
    break;

  case 273: // rhs: gdrhs wherebinds
#line 1137 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3929 "parser.cc"
    break;

  case 274: // gdrhs: gdrhs gdrh
#line 1139 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3935 "parser.cc"
    break;

  case 275: // gdrhs: gdrh
#line 1140 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3941 "parser.cc"
    break;

  case 276: // gdrh: "|" guardquals "=" exp
#line 1144 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3947 "parser.cc"
    break;

  case 277: // sigdecl: sig_vars "::" sigtypedoc
#line 1154 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3953 "parser.cc"
    break;

  case 278: // sigdecl: infix prec ops
#line 1155 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3959 "parser.cc"
    break;

  case 279: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1157 "parser.y"
                                                    {}
#line 3965 "parser.cc"
    break;

  case 280: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1158 "parser.y"
                                            {}
#line 3971 "parser.cc"
    break;

  case 281: // sigdecl: "{-# SCC" qvar "#-}"
#line 1159 "parser.y"
                              {}
#line 3977 "parser.cc"
    break;

  case 282: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1160 "parser.y"
                                     {}
#line 3983 "parser.cc"
    break;

  case 283: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1161 "parser.y"
                                                               {}
#line 3989 "parser.cc"
    break;

  case 284: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1162 "parser.y"
                                                                      {}
#line 3995 "parser.cc"
    break;

  case 285: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1163 "parser.y"
                                                     {}
#line 4001 "parser.cc"
    break;

  case 290: // exp: infixexp "::" sigtype
#line 1175 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4007 "parser.cc"
    break;

  case 291: // exp: infixexp
#line 1176 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4013 "parser.cc"
    break;

  case 292: // infixexp: exp10
#line 1180 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4019 "parser.cc"
    break;

  case 293: // infixexp: infixexp qop exp10
#line 1181 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4025 "parser.cc"
    break;

  case 294: // exp10: PREFIX_MINUS fexp
#line 1183 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4031 "parser.cc"
    break;

  case 295: // exp10: fexp
#line 1184 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4037 "parser.cc"
    break;

  case 298: // fexp: fexp aexp
#line 1192 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4043 "parser.cc"
    break;

  case 299: // fexp: fexp "@" atype
#line 1193 "parser.y"
                                 {}
#line 4049 "parser.cc"
    break;

  case 300: // fexp: "static" aexp
#line 1194 "parser.y"
                                 {}
#line 4055 "parser.cc"
    break;

  case 301: // fexp: aexp
#line 1195 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4061 "parser.cc"
    break;

  case 302: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1198 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4067 "parser.cc"
    break;

  case 303: // aexp: PREFIX_TILDE aexp
#line 1199 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4073 "parser.cc"
    break;

  case 304: // aexp: PREFIX_BANG aexp
#line 1200 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4079 "parser.cc"
    break;

  case 305: // aexp: "\\" apats1 "->" exp
#line 1201 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4085 "parser.cc"
    break;

  case 306: // aexp: "let" binds "in" exp
#line 1202 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4091 "parser.cc"
    break;

  case 307: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1204 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4097 "parser.cc"
    break;

  case 308: // aexp: "case" exp "of" altslist
#line 1206 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4103 "parser.cc"
    break;

  case 309: // aexp: "do" stmtlist
#line 1207 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4109 "parser.cc"
    break;

  case 310: // aexp: "mdo" stmtlist
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4115 "parser.cc"
    break;

  case 311: // aexp: aexp1
#line 1210 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4121 "parser.cc"
    break;

  case 312: // aexp1: aexp1 "{" fbinds "}"
#line 1213 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4127 "parser.cc"
    break;

  case 313: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1214 "parser.y"
                                     { }
#line 4133 "parser.cc"
    break;

  case 314: // aexp1: aexp2
#line 1215 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4139 "parser.cc"
    break;

  case 315: // aexp2: qvar
#line 1218 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4145 "parser.cc"
    break;

  case 316: // aexp2: qcon
#line 1219 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4151 "parser.cc"
    break;

  case 317: // aexp2: literal
#line 1220 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4157 "parser.cc"
    break;

  case 318: // aexp2: "(" texp ")"
#line 1221 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4163 "parser.cc"
    break;

  case 319: // aexp2: "(" tup_exprs ")"
#line 1222 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4169 "parser.cc"
    break;

  case 320: // aexp2: "(" projection ")"
#line 1223 "parser.y"
                              {}
#line 4175 "parser.cc"
    break;

  case 321: // aexp2: "[" list "]"
#line 1228 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4181 "parser.cc"
    break;

  case 322: // aexp2: "_"
#line 1229 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4187 "parser.cc"
    break;

  case 325: // texp: exp
#line 1238 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4193 "parser.cc"
    break;

  case 326: // texp: infixexp qop
#line 1239 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4199 "parser.cc"
    break;

  case 327: // texp: qopm infixexp
#line 1240 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4205 "parser.cc"
    break;

  case 328: // tup_exprs: tup_exprs "," texp
#line 1245 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4211 "parser.cc"
    break;

  case 329: // tup_exprs: texp "," texp
#line 1246 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4217 "parser.cc"
    break;

  case 330: // list: texp
#line 1264 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4223 "parser.cc"
    break;

  case 331: // list: lexps
#line 1265 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4229 "parser.cc"
    break;

  case 332: // list: texp ".."
#line 1266 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4235 "parser.cc"
    break;

  case 333: // list: texp "," exp ".."
#line 1267 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4241 "parser.cc"
    break;

  case 334: // list: texp ".." exp
#line 1268 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4247 "parser.cc"
    break;

  case 335: // list: texp "," exp ".." exp
#line 1269 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4253 "parser.cc"
    break;

  case 336: // list: texp "|" squals
#line 1270 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4259 "parser.cc"
    break;

  case 337: // lexps: lexps "," texp
#line 1272 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4265 "parser.cc"
    break;

  case 338: // lexps: texp "," texp
#line 1273 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4271 "parser.cc"
    break;

  case 339: // squals: squals "," qual
#line 1286 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4277 "parser.cc"
    break;

  case 340: // squals: qual
#line 1288 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4283 "parser.cc"
    break;

  case 341: // guardquals: guardquals1
#line 1298 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4289 "parser.cc"
    break;

  case 342: // guardquals1: guardquals1 "," qual
#line 1300 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4295 "parser.cc"
    break;

  case 343: // guardquals1: qual
#line 1301 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4301 "parser.cc"
    break;

  case 344: // altslist: "{" alts "}"
#line 1304 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4307 "parser.cc"
    break;

  case 345: // altslist: "vocurly" alts close
#line 1305 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4313 "parser.cc"
    break;

  case 346: // altslist: "{" "}"
#line 1306 "parser.y"
                                 {}
#line 4319 "parser.cc"
    break;

  case 347: // altslist: "vocurly" close
#line 1307 "parser.y"
                                 {}
#line 4325 "parser.cc"
    break;

  case 348: // alts: alts1
#line 1309 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4331 "parser.cc"
    break;

  case 349: // alts: ";" alts
#line 1310 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4337 "parser.cc"
    break;

  case 350: // alts1: alts1 ";" alt
#line 1312 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4343 "parser.cc"
    break;

  case 351: // alts1: alts1 ";"
#line 1313 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4349 "parser.cc"
    break;

  case 352: // alts1: alt
#line 1314 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4355 "parser.cc"
    break;

  case 353: // alt: pat alt_rhs
#line 1316 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4361 "parser.cc"
    break;

  case 354: // alt_rhs: "->" exp wherebinds
#line 1318 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4367 "parser.cc"
    break;

  case 355: // alt_rhs: gdpats wherebinds
#line 1319 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4373 "parser.cc"
    break;

  case 356: // gdpats: gdpats gdpat
#line 1321 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4379 "parser.cc"
    break;

  case 357: // gdpats: gdpat
#line 1322 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4385 "parser.cc"
    break;

  case 358: // gdpat: "|" guardquals "->" exp
#line 1331 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4391 "parser.cc"
    break;

  case 359: // pat: exp
#line 1333 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4397 "parser.cc"
    break;

  case 360: // bindpat: exp
#line 1335 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4403 "parser.cc"
    break;

  case 361: // apat: aexp
#line 1337 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4409 "parser.cc"
    break;

  case 362: // apats1: apats1 apat
#line 1339 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4415 "parser.cc"
    break;

  case 363: // apats1: apat
#line 1340 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4421 "parser.cc"
    break;

  case 364: // stmtlist: "{" stmts "}"
#line 1343 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4427 "parser.cc"
    break;

  case 365: // stmtlist: "vocurly" stmts close
#line 1344 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4433 "parser.cc"
    break;

  case 366: // stmts: stmts ";" stmt
#line 1346 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4439 "parser.cc"
    break;

  case 367: // stmts: stmts ";"
#line 1347 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4445 "parser.cc"
    break;

  case 368: // stmts: stmt
#line 1348 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4451 "parser.cc"
    break;

  case 369: // stmts: %empty
#line 1349 "parser.y"
                       {}
#line 4457 "parser.cc"
    break;

  case 370: // stmt: qual
#line 1354 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4463 "parser.cc"
    break;

  case 371: // stmt: "rec" stmtlist
#line 1355 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4469 "parser.cc"
    break;

  case 372: // qual: bindpat "<-" exp
#line 1357 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4475 "parser.cc"
    break;

  case 373: // qual: exp
#line 1358 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4481 "parser.cc"
    break;

  case 374: // qual: "let" binds
#line 1359 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4487 "parser.cc"
    break;

  case 375: // fbinds: fbinds1
#line 1364 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4493 "parser.cc"
    break;

  case 376: // fbinds: %empty
#line 1365 "parser.y"
                        {}
#line 4499 "parser.cc"
    break;

  case 377: // fbinds1: fbind "," fbinds1
#line 1367 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4505 "parser.cc"
    break;

  case 378: // fbinds1: fbind
#line 1368 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4511 "parser.cc"
    break;

  case 379: // fbinds1: ".."
#line 1369 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4517 "parser.cc"
    break;

  case 380: // fbind: qvar "=" texp
#line 1371 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4523 "parser.cc"
    break;

  case 381: // fbind: qvar
#line 1372 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4529 "parser.cc"
    break;

  case 382: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1373 "parser.y"
                                                      {}
#line 4535 "parser.cc"
    break;

  case 383: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1374 "parser.y"
                                                      {}
#line 4541 "parser.cc"
    break;

  case 386: // qcon: gen_qcon
#line 1410 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4547 "parser.cc"
    break;

  case 387: // qcon: sysdcon
#line 1411 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4553 "parser.cc"
    break;

  case 388: // gen_qcon: qconid
#line 1413 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4559 "parser.cc"
    break;

  case 389: // gen_qcon: "(" qconsym ")"
#line 1414 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4565 "parser.cc"
    break;

  case 390: // con: conid
#line 1416 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4571 "parser.cc"
    break;

  case 391: // con: "(" consym ")"
#line 1417 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4577 "parser.cc"
    break;

  case 392: // con: sysdcon
#line 1418 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4583 "parser.cc"
    break;

  case 393: // con_list: con_list "," con
#line 1420 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4589 "parser.cc"
    break;

  case 394: // con_list: con
#line 1421 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4595 "parser.cc"
    break;

  case 395: // sysdcon_no_list: "(" ")"
#line 1423 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4601 "parser.cc"
    break;

  case 396: // sysdcon_no_list: "(" commas ")"
#line 1424 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4607 "parser.cc"
    break;

  case 397: // sysdcon_no_list: "(#" "#)"
#line 1425 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4613 "parser.cc"
    break;

  case 398: // sysdcon_no_list: "(#" commas "#)"
#line 1426 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4619 "parser.cc"
    break;

  case 399: // sysdcon: sysdcon_no_list
#line 1428 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4625 "parser.cc"
    break;

  case 400: // sysdcon: "[" "]"
#line 1429 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4631 "parser.cc"
    break;

  case 401: // conop: consym
#line 1431 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4637 "parser.cc"
    break;

  case 402: // conop: "`" conid "`"
#line 1432 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4643 "parser.cc"
    break;

  case 403: // qconop: qconsym
#line 1434 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4649 "parser.cc"
    break;

  case 404: // qconop: "`" qconid "`"
#line 1435 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4655 "parser.cc"
    break;

  case 405: // gtycon: ntgtycon
#line 1438 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4661 "parser.cc"
    break;

  case 406: // gtycon: "(" ")"
#line 1439 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4667 "parser.cc"
    break;

  case 407: // gtycon: "(#" "#)"
#line 1440 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4673 "parser.cc"
    break;

  case 408: // ntgtycon: oqtycon
#line 1442 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4679 "parser.cc"
    break;

  case 409: // ntgtycon: "(" commas ")"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4685 "parser.cc"
    break;

  case 410: // ntgtycon: "(#" commas "#)"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4691 "parser.cc"
    break;

  case 411: // ntgtycon: "(" "->" ")"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4697 "parser.cc"
    break;

  case 412: // ntgtycon: "[" "]"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4703 "parser.cc"
    break;

  case 413: // oqtycon: qtycon
#line 1448 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4709 "parser.cc"
    break;

  case 414: // oqtycon: "(" qtyconsym ")"
#line 1449 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4715 "parser.cc"
    break;

  case 415: // oqtycon_no_varcon: qtycon
#line 1451 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4721 "parser.cc"
    break;

  case 416: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1452 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4727 "parser.cc"
    break;

  case 417: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1453 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4733 "parser.cc"
    break;

  case 418: // oqtycon_no_varcon: "(" ":" ")"
#line 1454 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4739 "parser.cc"
    break;

  case 419: // qtyconop: qtyconsym
#line 1457 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4745 "parser.cc"
    break;

  case 420: // qtyconop: "`" qtycon "`"
#line 1458 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4751 "parser.cc"
    break;

  case 421: // qtycondoc: qtycon
#line 1460 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4757 "parser.cc"
    break;

  case 422: // qtycon: "QCONID"
#line 1462 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4763 "parser.cc"
    break;

  case 423: // qtycon: tycon
#line 1463 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4769 "parser.cc"
    break;

  case 424: // tycon: "CONID"
#line 1467 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4775 "parser.cc"
    break;

  case 425: // qtyconsym: "QCONSYM"
#line 1469 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4781 "parser.cc"
    break;

  case 426: // qtyconsym: "QVARSYM"
#line 1470 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4787 "parser.cc"
    break;

  case 427: // qtyconsym: tyconsym
#line 1471 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4793 "parser.cc"
    break;

  case 428: // tyconsym: "CONSYM"
#line 1473 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4799 "parser.cc"
    break;

  case 429: // tyconsym: "VARSYM"
#line 1474 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4805 "parser.cc"
    break;

  case 430: // tyconsym: ":"
#line 1475 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4811 "parser.cc"
    break;

  case 431: // tyconsym: "-"
#line 1476 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4817 "parser.cc"
    break;

  case 432: // op: varop
#line 1481 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4823 "parser.cc"
    break;

  case 433: // op: conop
#line 1482 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4829 "parser.cc"
    break;

  case 434: // varop: varsym
#line 1484 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4835 "parser.cc"
    break;

  case 435: // varop: "`" varid "`"
#line 1485 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4841 "parser.cc"
    break;

  case 436: // qop: qvarop
#line 1487 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4847 "parser.cc"
    break;

  case 437: // qop: qconop
#line 1488 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4853 "parser.cc"
    break;

  case 438: // qopm: qvaropm
#line 1491 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4859 "parser.cc"
    break;

  case 439: // qopm: qconop
#line 1492 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4865 "parser.cc"
    break;

  case 440: // qvarop: qvarsym
#line 1497 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4871 "parser.cc"
    break;

  case 441: // qvarop: "`" qvarid "`"
#line 1498 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4877 "parser.cc"
    break;

  case 442: // qvaropm: qvarsym_no_minus
#line 1500 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4883 "parser.cc"
    break;

  case 443: // qvaropm: "`" qvarid "`"
#line 1501 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4889 "parser.cc"
    break;

  case 444: // tyvar: tyvarid
#line 1505 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4895 "parser.cc"
    break;

  case 445: // tyvarop: "`" tyvarid "`"
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4901 "parser.cc"
    break;

  case 446: // tyvarid: "VARID"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4907 "parser.cc"
    break;

  case 447: // tyvarid: special_id
#line 1510 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4913 "parser.cc"
    break;

  case 448: // tyvarid: "unsafe"
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4919 "parser.cc"
    break;

  case 449: // tyvarid: "safe"
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4925 "parser.cc"
    break;

  case 450: // tyvarid: "interruptible"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4931 "parser.cc"
    break;

  case 451: // var: varid
#line 1516 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4937 "parser.cc"
    break;

  case 452: // var: "(" varsym ")"
#line 1517 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4943 "parser.cc"
    break;

  case 453: // qvar: qvarid
#line 1519 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4949 "parser.cc"
    break;

  case 454: // qvar: "(" varsym ")"
#line 1520 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4955 "parser.cc"
    break;

  case 455: // qvar: "(" qvarsym1 ")"
#line 1521 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4961 "parser.cc"
    break;

  case 456: // field: varid
#line 1523 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4967 "parser.cc"
    break;

  case 457: // qvarid: varid
#line 1525 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4973 "parser.cc"
    break;

  case 458: // qvarid: "QVARID"
#line 1526 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4979 "parser.cc"
    break;

  case 459: // varid: "VARID"
#line 1528 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4985 "parser.cc"
    break;

  case 460: // varid: special_id
#line 1529 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4991 "parser.cc"
    break;

  case 461: // varid: "unsafe"
#line 1530 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4997 "parser.cc"
    break;

  case 462: // varid: "safe"
#line 1531 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5003 "parser.cc"
    break;

  case 463: // varid: "interruptible"
#line 1532 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5009 "parser.cc"
    break;

  case 464: // varid: "forall"
#line 1533 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5015 "parser.cc"
    break;

  case 465: // varid: "family"
#line 1534 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5021 "parser.cc"
    break;

  case 466: // varid: "role"
#line 1535 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5027 "parser.cc"
    break;

  case 467: // qvarsym: varsym
#line 1537 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5033 "parser.cc"
    break;

  case 468: // qvarsym: qvarsym1
#line 1538 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5039 "parser.cc"
    break;

  case 469: // qvarsym_no_minus: varsym_no_minus
#line 1540 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5045 "parser.cc"
    break;

  case 470: // qvarsym_no_minus: qvarsym1
#line 1541 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5051 "parser.cc"
    break;

  case 471: // qvarsym1: "QVARSYM"
#line 1543 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5057 "parser.cc"
    break;

  case 472: // varsym: varsym_no_minus
#line 1545 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5063 "parser.cc"
    break;

  case 473: // varsym: "-"
#line 1546 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5069 "parser.cc"
    break;

  case 474: // varsym_no_minus: "VARSYM"
#line 1548 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5075 "parser.cc"
    break;

  case 475: // varsym_no_minus: special_sym
#line 1549 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5081 "parser.cc"
    break;

  case 476: // special_id: "as"
#line 1551 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5087 "parser.cc"
    break;

  case 477: // special_id: "qualified"
#line 1552 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5093 "parser.cc"
    break;

  case 478: // special_id: "hiding"
#line 1553 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5099 "parser.cc"
    break;

  case 479: // special_id: "export"
#line 1554 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5105 "parser.cc"
    break;

  case 480: // special_id: "label"
#line 1555 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5111 "parser.cc"
    break;

  case 481: // special_id: "dynamic"
#line 1556 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5117 "parser.cc"
    break;

  case 482: // special_id: "stdcall"
#line 1557 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5123 "parser.cc"
    break;

  case 483: // special_id: "ccall"
#line 1558 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5129 "parser.cc"
    break;

  case 484: // special_id: "capi"
#line 1559 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5135 "parser.cc"
    break;

  case 485: // special_id: "prim"
#line 1560 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5141 "parser.cc"
    break;

  case 486: // special_id: "javascript"
#line 1561 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5147 "parser.cc"
    break;

  case 487: // special_id: "group"
#line 1562 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5153 "parser.cc"
    break;

  case 488: // special_id: "stock"
#line 1563 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5159 "parser.cc"
    break;

  case 489: // special_id: "anyclass"
#line 1564 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5165 "parser.cc"
    break;

  case 490: // special_id: "via"
#line 1565 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5171 "parser.cc"
    break;

  case 491: // special_id: "unit"
#line 1566 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5177 "parser.cc"
    break;

  case 492: // special_id: "dependency"
#line 1567 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5183 "parser.cc"
    break;

  case 493: // special_id: "signature"
#line 1568 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5189 "parser.cc"
    break;

  case 494: // special_sym: "."
#line 1570 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5195 "parser.cc"
    break;

  case 495: // special_sym: "*"
#line 1571 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5201 "parser.cc"
    break;

  case 496: // qconid: conid
#line 1575 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5207 "parser.cc"
    break;

  case 497: // qconid: "QCONID"
#line 1576 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5213 "parser.cc"
    break;

  case 498: // conid: "CONID"
#line 1578 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5219 "parser.cc"
    break;

  case 499: // qconsym: consym
#line 1580 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5225 "parser.cc"
    break;

  case 500: // qconsym: "QCONSYM"
#line 1581 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5231 "parser.cc"
    break;

  case 501: // consym: "CONSYM"
#line 1583 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5237 "parser.cc"
    break;

  case 502: // consym: ":"
#line 1584 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5243 "parser.cc"
    break;

  case 503: // literal: "CHAR"
#line 1588 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5249 "parser.cc"
    break;

  case 504: // literal: "STRING"
#line 1589 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5255 "parser.cc"
    break;

  case 505: // literal: "INTEGER"
#line 1590 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5261 "parser.cc"
    break;

  case 506: // literal: "RATIONAL"
#line 1591 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5267 "parser.cc"
    break;

  case 507: // literal: "PRIMINTEGER"
#line 1592 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5273 "parser.cc"
    break;

  case 509: // close: error
#line 1600 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5279 "parser.cc"
    break;

  case 510: // modid: "CONID"
#line 1604 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5285 "parser.cc"
    break;

  case 511: // modid: "QCONID"
#line 1605 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5291 "parser.cc"
    break;

  case 512: // commas: commas ","
#line 1607 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5297 "parser.cc"
    break;

  case 513: // commas: ","
#line 1608 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5303 "parser.cc"
    break;


#line 5307 "parser.cc"

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


  const short parser::yypact_ninf_ = -669;

  const short parser::yytable_ninf_ = -473;

  const short
  parser::yypact_[] =
  {
      29,   251,  -669,    97,  -669,  -669,  -669,  -669,  -669,    41,
      48,    19,  -669,    44,   -28,   -28,    56,  -669,  -669,  -669,
    -669,   173,  -669,  -669,  -669,   150,  -669,   140,   241,  1371,
     329,   347,   268,  -669,   827,  -669,   148,  -669,  -669,  -669,
    -669,   251,  -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,
    -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,
    -669,  -669,  -669,  -669,   950,  -669,  -669,  -669,  -669,  -669,
     307,   -48,  -669,   311,  -669,  -669,  -669,  -669,  -669,  -669,
    -669,   -16,  -669,   251,  -669,   322,  -669,  2424,  4471,   415,
     351,   238,  2424,  -669,  -669,  -669,   363,   260,  -669,  3532,
     456,   238,  3104,   368,  4955,   369,  3104,  3104,  2696,  3104,
    1744,  1608,   230,  -669,  -669,  -669,  -669,  -669,  -669,  -669,
      40,   368,   339,   268,  -669,  -669,  -669,  -669,    65,    94,
    -669,  -669,   656,  -669,  2832,  -669,   229,  -669,  -669,  -669,
    -669,  -669,  -669,   401,   119,  -669,  -669,  -669,  -669,   358,
    -669,   380,  -669,  -669,  -669,  -669,   388,  -669,   391,   396,
     399,  -669,  -669,  -669,  4576,  -669,  4613,  -669,  -669,  -669,
    -669,   510,  -669,  1608,   495,   667,  -669,  -669,  -669,  4471,
    4471,  -669,  5054,  3633,  3216,   410,  -669,   503,   442,  -669,
     374,  -669,  3820,  -669,  -669,  -669,  -669,  -669,  -669,  -669,
    4471,  3921,  2152,  2152,  -669,   416,   461,   463,   464,   468,
    3921,  1247,  1247,  -669,   531,  4471,  4471,   129,   466,   403,
     131,   507,  -669,  -669,    -5,  4955,  -669,   245,   -12,   441,
     156,  -669,   139,  -669,  -669,  -669,  -669,  2968,  -669,  2832,
    -669,  -669,  -669,  4818,  -669,  -669,  -669,   667,    89,   445,
     443,  -669,  2424,  -669,  -669,  -669,  -669,  -669,  -669,  5177,
    -669,  -669,    -2,    69,   262,   396,   448,   450,   455,   269,
    -669,   291,  3921,  4955,  4955,  -669,   793,   322,   498,   437,
    4471,  3921,  5054,  2424,  2560,  4818,  -669,    33,  -669,  -669,
    2424,  -669,  -669,  -669,  -669,  4471,  -669,  5177,  4918,  3104,
    -669,  -669,  -669,  -669,  -669,  -669,  -669,   459,   460,   458,
    -669,   470,    44,   251,    35,   315,  3921,  -669,  -669,   267,
     149,   474,   462,  -669,  -669,  -669,  -669,   471,   502,   494,
    -669,  -669,   473,  -669,  -669,  -669,  -669,  -669,  -669,   475,
     488,   476,  -669,   286,   296,   341,  -669,  4471,  3921,  4855,
    4471,  -669,  -669,  -669,  4471,  -669,  -669,   520,  -669,   501,
     489,   260,   238,   526,   536,   113,  -669,  -669,    28,  -669,
     562,  -669,  -669,  -669,  -669,  -669,  -669,   598,   168,  -669,
    -669,   656,    76,  2424,  -669,   546,   197,  3921,   162,  3921,
     499,   500,   519,   558,  -669,   560,   528,   490,   369,   565,
    2424,  -669,   523,   525,  2424,  2424,  2560,  1880,  -669,  1880,
     808,  -669,  -669,  5177,  -669,  -669,  1880,  -669,  1880,   153,
    -669,  -669,  -669,  -669,   573,   559,   579,  5017,   542,  -669,
    -669,  -669,  -669,  -669,  4022,   -10,   223,  -669,  -669,  -669,
    -669,   638,   594,   569,  -669,   557,   260,  -669,  -669,  -669,
    -669,  -669,  -669,   574,  -669,   570,   597,   589,   590,  -669,
    -669,  -669,  4754,  -669,  -669,  -669,   580,  1408,  -669,  -669,
    2016,  1472,  -669,  -669,   581,  3921,  -669,  5054,  5214,  -669,
    3921,  3921,  -669,  -669,  3921,  -669,  -669,  -669,  1111,  1111,
    -669,  -669,  -669,   576,   583,   481,  -669,  3921,  -669,  -669,
    3921,   531,  -669,  2424,  -669,  2152,  -669,  2424,   345,  -669,
    -669,  1247,  -669,  -669,  3921,  3921,  5327,   616,  -669,  -669,
     305,  -669,  -669,  5054,   592,  -669,  -669,  -669,  -669,   595,
     686,   303,  -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,
     587,  -669,   629,  -669,  -669,  -669,  -669,  -669,  -669,  3921,
    3921,   588,   591,   793,  -669,   223,   622,  -669,  -669,   639,
    3921,   690,   701,   720,  -669,  2424,  2560,  -669,  -669,  -669,
    4918,  1880,  5177,  -669,  1408,   251,  -669,   311,   617,   169,
    -669,  -669,  2288,  -669,   631,   620,  -669,   382,    44,  -669,
    -669,  -669,  -669,  3921,  5413,  5413,  -669,  -669,  -669,  -669,
    -669,   632,   705,  3734,  -669,  -669,   187,  -669,    82,  -669,
    -669,  -669,  -669,  -669,  -669,   416,   969,   969,  -669,  -669,
    -669,  -669,  -669,  5413,   721,   670,  -669,  -669,  -669,  2560,
    2424,  -669,    26,    31,  -669,  -669,  -669,  5091,   701,   720,
    4471,  -669,  -669,  -669,   669,  -669,  4471,   365,   720,   164,
    -669,   720,  -669,  -669,  -669,  -669,  -669,    84,  -669,   641,
    -669,  -669,  -669,  4717,  -669,  -669,  -669,  2424,  2560,  2424,
    -669,    42,  -669,  -669,  -669,    13,   671,  -669,  -669,  4471,
    4471,  4471,  -669,   377,  -669,  1111,  -669,   744,  -669,   738,
    -669,   738,  -669,   222,  -669,    88,  -669,   672,   381,  -669,
    3921,  -669,  -669,  -669,  3921,  -669,  4471,  4471,   720,  -669,
    -669,  5251,   690,   673,  3322,  -669,  -669,  -669,   416,   416,
    -669,  -669,  -669,  -669,  4108,   231,   708,  -669,  -669,  -669,
    1880,  5177,  -669,  -669,  -669,   680,   638,  -669,  -669,  3921,
    -669,  3921,   520,  -669,   436,  3921,  4213,  -669,  -669,  2424,
    -669,  4471,   498,  -669,   969,  -669,  5413,  4299,  4385,  -669,
    -669,  -669,  -669,   676,   481,  -669,  -669,  -669,  4471,   645,
    -669,  4471,   247,  -669,   369,   114,  -669,  -669,   650,   660,
    -669,  4471,  -669,  -669,  -669,  2424,  -669,   678,   661,  -669,
    5360,  -669,  -669,  3216,   691,   693,  -669,  -669,  4022,  -669,
    5413,  -669,   688,   254,  -669,    44,   138,  4471,  3427,  -669,
    4471,  -669,   416,   161,  -669,  4471,  -669,  -669,  -669,  -669,
    -669,   671,  5413,   223,  -669,  -669,  -669,  4471,  -669,  -669,
    -669,  -669,  3921,  -669,  -669,   701,   720,  -669,  -669,   720,
    -669,  -669
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   510,   511,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    69,   509,   508,    12,   182,   178,     0,     0,    20,
       0,    45,    40,    15,    14,   181,     0,     6,     7,   476,
     478,     0,   477,   464,   479,   480,   481,   462,   463,   461,
     465,   466,   482,   483,   484,   485,   486,   487,   488,   489,
     490,   491,   493,   492,     0,   459,   424,   458,   422,    22,
       0,    19,    24,    27,    35,   415,   423,    34,   453,   457,
     460,     0,    44,     0,    37,    41,   322,     0,     0,   128,
       0,     0,     0,    62,    63,    64,    95,     0,   129,     0,
       0,     0,     0,   286,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   498,   497,   503,   504,   505,   506,   507,
     286,   286,    60,    67,    70,    71,    72,    73,   148,     0,
      76,   269,    77,   292,   295,   301,   311,   314,   316,   386,
     399,   387,   188,   315,   457,   388,   496,   317,   179,     0,
      26,     0,   473,   495,   494,   474,     0,   471,     0,     0,
       0,   472,   475,    17,     0,    21,    30,    25,    39,    39,
       3,    47,    36,     0,     0,   291,   449,   450,   448,     0,
       0,   216,   254,     0,     0,     0,   446,   158,     0,   141,
     199,   202,   203,   207,   214,   408,   413,   215,   444,   447,
       0,   228,   369,   369,   309,   297,     0,     0,     0,     0,
       0,   172,   172,   175,     0,     0,     0,     0,     0,   199,
     408,     0,   310,   300,     0,     0,   287,     0,     0,     0,
       0,   394,   183,   392,   390,   361,   363,     0,   303,   294,
     304,   502,   400,     0,   501,   500,   325,   291,   330,     0,
     331,   439,     0,   438,   442,   470,   469,   403,   499,     0,
     395,   513,     0,     0,     0,   470,     0,   469,   403,     0,
     397,     0,     0,     0,     0,    61,     0,    68,   148,     0,
       0,     0,     0,     0,     0,     0,   270,   177,   275,   437,
       0,   436,   440,   468,   467,     0,   298,     0,   376,     0,
     180,   418,   417,   416,   455,   454,    23,     0,     0,    31,
      33,     0,     0,     0,    49,     0,     0,   218,   217,     0,
       0,     0,   255,   257,   451,   232,   412,     0,   191,     0,
     195,   430,     0,   431,   220,   429,   428,   426,   425,   229,
       0,     0,   427,     0,     0,     0,    78,     0,     0,     0,
       0,   211,   419,   212,     0,   208,   210,   132,   229,     0,
     227,     0,     0,   373,     0,     0,   368,   370,     0,   296,
       0,    92,    91,    93,    94,   224,   185,   168,     0,   271,
     171,     0,     0,     0,    88,     0,   134,     0,     0,     0,
       0,     0,     0,     0,   281,     0,     0,     0,     0,     0,
       0,   362,     0,     0,   326,   332,     0,     0,   321,     0,
     327,   324,   456,     0,   320,   318,     0,   319,     0,   454,
     389,   396,   512,   398,     0,     0,     0,     0,   278,   433,
      66,   432,   434,   401,     0,     0,   130,   277,   196,   186,
     187,   177,     0,   341,   343,     0,     0,   273,   274,   293,
     299,   313,   379,     0,   375,   378,   381,     0,   457,   302,
      29,    28,     0,     9,    10,    46,     0,    53,    43,    48,
       0,     0,   308,   290,     0,     0,   219,     0,     0,   222,
       0,     0,   411,   223,     0,   414,   409,   410,   154,   154,
     157,   140,   200,     0,     0,   204,   209,     0,    83,    74,
       0,   374,   371,     0,   364,   367,   365,     0,     0,    87,
     173,   170,   174,   306,     0,     0,     0,   100,   238,    84,
       0,    85,    79,     0,     0,   288,   280,   282,   391,     0,
       0,     0,   184,   405,   393,   279,   305,   443,   404,   334,
     336,   340,   325,   338,   337,   323,   329,   328,   285,     0,
       0,     0,     0,     0,   232,   130,     0,   145,   147,     0,
       0,   251,   241,   259,   272,     0,     0,   441,   176,   312,
       0,     0,     0,    32,    53,     0,    55,    27,     0,    52,
      57,   346,     0,   359,     0,   348,   352,     0,     0,   347,
     452,   258,   256,     0,     0,     0,   231,   233,   236,   192,
     194,   230,   121,     0,   149,   153,     0,   150,     0,   420,
     445,   133,   230,   372,   366,   297,   164,   164,   167,   169,
     115,   135,   136,     0,   105,     0,   289,   406,   407,     0,
     333,   189,     0,     0,   435,   402,    65,     0,   241,   259,
       0,   146,   131,   232,   245,   247,     0,     0,   259,     0,
      80,   260,   262,   276,   342,   377,   380,   383,   385,     0,
      59,    58,    50,     0,    54,   349,   344,   351,     0,     0,
     353,   177,   357,   345,   193,     0,     0,   221,   122,     0,
       0,     0,   119,   137,   155,   152,   156,     0,   128,   123,
     159,   123,   163,     0,   160,     0,   101,     0,     0,    82,
       0,   339,   335,   283,     0,   284,     0,     0,   259,    89,
     144,     0,   251,     0,   252,   205,   213,   249,   297,   297,
      81,    98,    96,    97,     0,     0,   263,   266,   421,   261,
       0,     0,    51,    56,   350,     0,   177,   355,   356,     0,
     234,     0,   132,   120,   137,     0,     0,   117,   151,     0,
     124,     0,   148,   165,   162,   166,     0,   114,   114,   106,
      75,   190,   143,     0,   197,    90,   250,   246,     0,     0,
     206,     0,     0,   243,     0,     0,   267,   201,   225,     0,
     264,     0,   265,   382,   384,     0,   354,     0,     0,   116,
       0,   118,   138,     0,     0,   215,   307,   125,     0,   161,
     102,   104,     0,     0,   113,     0,     0,     0,   252,   248,
     253,   239,   297,     0,   240,     0,   268,    99,   358,   235,
     237,   215,     0,   130,   103,   109,   107,   112,   110,   108,
     142,   242,     0,   226,   139,   241,   259,   111,   244,   259,
     126,   127
  };

  const short
  parser::yypgoto_[] =
  {
    -669,  -669,  -669,  -669,  -669,  -669,  -669,    34,  -669,  -669,
    -669,  -669,   614,   217,  -669,  -669,    -8,   674,  -669,  -669,
    -669,  -669,  -669,  -669,  -669,  -669,   224,  -669,   137,  -669,
    -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,  -669,
    -669,  -669,  -669,  -669,   -21,  -669,  -669,  -669,    45,  -196,
    -669,  -669,   115,  -669,   770,  -521,    66,  -669,    63,   529,
      14,  -266,   128,   327,  -669,  -669,    64,   200,  -669,  -669,
     607,  -669,  -308,  -416,   805,  -669,  -669,  -296,   121,  -154,
     272,  -153,  -147,  -669,   -83,  -669,   -84,  -669,   -25,  -669,
    -367,  -669,  -669,  -669,  -644,  -124,   551,     9,  -669,   624,
    -473,   313,  -660,  -356,  -596,   117,    30,  -514,  -669,   126,
    -669,    73,  -669,  -669,   371,  -599,  -669,   199,   127,   820,
    -203,  -669,  -669,   586,  -669,   405,  -669,   159,    32,  -247,
    -183,   760,   118,  -669,  -669,  -669,   -92,  -669,  -669,  -669,
    -669,   207,  -669,  -669,  -427,  -669,   209,  -669,  -669,   214,
    -669,  -669,   651,  -669,   -68,   689,   386,  -252,  -669,   323,
    -669,  -669,  -669,  -669,   496,   123,  -669,  -102,  -668,   -75,
    -669,   505,   -72,  -669,  -669,  -669,    46,  -669,  -188,  -669,
     342,  -669,   652,  -669,  -669,  -669,  -414,  -669,  -325,  -244,
      -3,  -236,  -171,   -33,  -669,  -669,   -13,   -29,   -97,   -88,
    -669,  -133,   -98,   -53,  -214,  -669,  -295,   -31,  -106
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   170,     6,    10,    19,    30,
      70,    71,    72,   167,   308,   309,   577,    85,    11,    20,
      21,    32,    83,   314,   468,   469,   578,   579,   580,   276,
     122,   428,    33,    34,   123,   124,   125,   126,   217,   127,
     210,   725,   782,   624,   696,   800,   699,   759,   803,   804,
     604,   679,   751,   690,   691,   562,   498,   517,   747,   187,
     555,   280,   605,   606,   490,   346,   692,   693,   618,   509,
     378,   213,   214,   447,    27,    36,   399,   375,   437,   129,
     632,   339,   518,   439,   329,   713,   330,   778,   190,   191,
     714,   192,   355,   350,   715,   193,   377,   779,   359,   340,
     478,   596,   597,   519,   648,   772,   773,   563,   644,   645,
     646,   717,   321,   322,   323,   650,   651,   652,   726,   379,
     607,   286,   287,   288,   131,   225,   226,   246,   175,   133,
     774,   134,   135,   136,   137,   262,   263,   264,   249,   250,
     540,   442,   443,   472,   584,   585,   586,   670,   671,   672,
     587,   364,   236,   237,   204,   365,   366,   367,   453,   454,
     455,   657,   138,   139,   231,   232,   140,   141,   429,   251,
     532,   194,   195,    74,   351,   727,   196,    76,   341,   342,
     430,   431,   290,   252,   291,   253,   197,   353,   198,   142,
     143,   457,    78,    79,   292,   254,   255,   294,   161,    80,
     162,   145,   146,   257,   258,   147,    24,     9,   269
  };

  const short
  parser::yytable_[] =
  {
     199,   144,   352,   233,   189,   188,   271,   234,   380,   380,
     150,   199,   434,   256,   267,   218,   396,   464,   248,   384,
     473,    73,   370,   411,   494,   564,    77,   220,   320,    22,
     327,   352,   444,   222,   638,   160,   328,   328,   440,    13,
     709,   639,   708,   449,   588,    22,   771,   466,   358,   720,
       1,   159,   171,   501,   328,   317,   318,   289,   268,   272,
     446,   451,   433,   376,   598,   394,   132,   558,   356,   446,
     770,   164,   402,   506,   219,    75,   267,    22,   343,   344,
      25,   637,   266,    22,   278,   391,   794,   512,   168,    22,
     169,   199,   199,   739,   165,   199,   199,    12,   265,   413,
     289,   228,   598,   703,   199,    26,    14,    15,   705,   765,
     403,   414,   199,   199,   445,    22,   357,   284,   740,   293,
     268,   395,   199,   559,   599,   376,   668,   199,   199,   392,
     794,   385,   386,     2,   438,    23,   279,    18,   568,    22,
     771,   611,   247,   247,   160,   704,   505,   467,   224,   324,
     704,    23,   403,    17,   541,   665,    73,   449,   310,   621,
     265,    77,   293,    77,   770,   730,   770,   405,    29,   376,
     711,   450,   289,   406,   281,   219,   589,   545,   144,   144,
     675,   676,   415,    23,   199,   731,   721,    31,   416,    23,
     219,   219,   199,   199,   511,    23,   189,   188,   160,  -451,
     685,   492,   311,   312,   642,   247,   754,   199,   407,   387,
      75,   -86,    75,   282,   159,   722,   723,    37,   504,   397,
     223,    23,   393,   598,   235,   238,   412,   240,   199,   475,
     496,   505,   812,  -452,   293,   241,   522,   840,  -451,   839,
     841,   832,   328,   381,   381,    23,   174,   432,   388,   324,
     -86,   205,   296,   631,   631,   737,   827,   148,   398,   199,
     199,   199,   199,   491,   412,   458,   199,   149,   282,   260,
     425,   426,  -452,   510,   520,   261,   724,   515,   516,   625,
     398,   244,   465,    35,   410,    66,   511,    66,   663,    68,
     474,    68,   684,   673,   502,   456,   233,   598,   697,   199,
     234,   199,   835,   560,   561,   685,   289,   352,   619,   836,
     256,   664,   256,   686,   654,   543,   521,   544,    38,   256,
     786,   256,   219,   320,   546,   495,   547,   753,   591,   552,
     297,   601,   795,   298,   600,   289,   658,   328,   152,   433,
     754,   153,   202,   724,   203,   270,   199,   612,   154,   261,
     557,   556,   811,   328,    66,   235,    81,   296,    68,   826,
     152,   363,   363,   153,   211,   812,   212,   620,   293,   155,
     154,    82,   827,   157,     7,   417,   598,   701,     8,   821,
     412,   418,   421,   787,   331,   788,    84,   199,   422,   792,
     199,   155,   199,   199,   551,   493,   199,   293,   333,   486,
     755,   808,   376,   376,   810,   422,   423,   682,   761,   199,
     422,   487,   199,   694,   694,   422,   444,   459,   628,   470,
     163,   471,   261,   166,   343,   344,   199,   199,   199,   335,
     336,   801,   687,   337,   338,   206,   207,   208,   209,   247,
     172,   247,   441,   363,   324,   488,   674,   489,   247,   616,
     247,   617,   200,   331,   573,   144,   144,   745,   746,    77,
     348,   199,   199,   201,    77,  -197,   668,   333,   669,   718,
     221,   719,   199,   275,   256,   824,   224,   229,   144,   656,
     814,   230,   331,   112,   743,   757,   798,   758,   299,   348,
     324,   300,   113,   301,   349,   784,   333,   697,   335,   336,
     433,   302,   337,   338,   303,   199,   199,   199,    75,   304,
     828,   829,   305,    75,   313,   199,   745,   790,   315,   683,
     381,   381,   716,   349,   432,   273,   274,   335,   336,   261,
     345,   337,   338,   347,   369,   199,   838,   458,   371,   412,
     372,   373,   513,   381,   660,   374,   383,   389,   390,   199,
     242,   694,   199,   438,   408,   797,   710,   376,   199,   536,
     331,   419,   409,  -472,   539,   363,   542,   456,   420,   279,
     435,    77,   460,   461,   333,   463,   352,   462,   219,   476,
     479,   477,   480,   144,   144,   481,   482,   507,   483,   485,
     716,   199,   199,   199,   433,   742,   385,   744,   529,   620,
     497,   349,   530,   247,   531,   335,   336,   484,   500,   337,
     338,  -360,   199,    66,   499,   219,   199,    68,   199,   199,
      75,   503,   762,   199,   763,   508,   199,   514,   525,   583,
     583,   837,   523,   256,   524,   526,   199,   527,   783,   549,
     777,   528,   535,   537,   716,   538,   328,   716,   381,   381,
     548,   199,   144,   199,   219,   219,   219,   199,   199,   550,
      77,   553,   613,   199,   363,   446,   615,   385,   199,   199,
     199,   552,   233,   385,   385,   565,   234,   567,   571,   569,
     199,   219,   764,   199,   716,   376,   716,   343,   566,   570,
     572,  -456,   574,   199,   590,   728,   609,   817,   412,   219,
     623,   626,   199,   610,   326,   199,   629,   630,   634,    75,
     199,   635,   199,   640,   557,   556,   641,   381,   643,   199,
     199,   144,   199,   830,   653,   363,   219,   199,   647,   649,
     662,   777,   219,   219,   199,   241,   666,   283,   667,   199,
     284,   583,   678,   385,   199,   677,   241,   316,   698,   152,
     700,   741,   153,   712,   732,   749,   219,   750,   756,   154,
     152,   781,   247,   153,   768,   331,   785,   807,   113,   815,
     154,   728,   332,   816,   820,   822,   285,  -236,   306,   333,
     155,   244,   219,   819,   157,   245,   381,   285,   363,   702,
     219,   155,   244,   825,   661,   157,   245,   277,   659,   627,
     733,   834,   219,   806,   128,   261,   752,   791,   789,   436,
     335,   336,   823,   748,   337,   338,   608,   695,   799,   382,
      28,   760,   633,   424,   833,   360,   583,   363,   736,   622,
      86,    39,    87,    88,    89,    90,   775,    91,   767,    40,
      92,   809,   831,    93,    94,    95,    96,    97,   592,    98,
     729,    42,   780,    99,   130,    43,   100,    44,    45,    46,
      47,    48,    49,   101,    50,    51,    52,    53,   239,    54,
      55,    56,   241,   448,    57,   735,   734,   102,    58,    59,
      60,    61,    62,    63,   103,   738,   152,   241,   401,   153,
     104,   614,   368,   655,   534,   636,   154,   813,     0,   404,
       0,   152,   533,   105,   153,     0,     0,     0,   796,   106,
       0,   154,     0,   427,     0,     0,   107,   155,   244,   108,
       0,   109,     0,     0,     0,     0,     0,     0,   285,     0,
       0,     0,   155,   244,     0,   110,   157,   245,     0,   111,
       0,   112,     0,     0,   818,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,     0,     0,     0,     0,   115,
     116,   117,   118,     0,     0,   119,     0,     0,     0,     0,
     120,   121,    86,    39,    87,     0,   688,     0,     0,    91,
       0,    40,    92,     0,     0,    93,    94,    95,     0,    97,
       0,    98,     0,    42,     0,   689,     0,    43,     0,    44,
      45,    46,    47,    48,    49,   101,    50,    51,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,   102,
      58,    59,    60,    61,    62,    63,   103,     0,     0,   151,
       0,     0,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   152,     0,   105,   153,     0,     0,     0,
       0,   106,     0,   154,     0,     0,     0,     0,   107,     0,
       0,   108,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   155,   156,     0,   110,   157,   158,
       0,   111,     0,   112,     0,     0,     0,     0,     0,     0,
       0,    65,   113,     0,     0,    67,   114,     0,     0,     0,
       0,   115,   116,   117,   118,     0,     0,   119,     0,     0,
       0,     0,   120,   121,    86,    39,    87,     0,   602,     0,
       0,    91,     0,    40,    92,     0,     0,    93,    94,    95,
       0,    97,     0,     0,     0,    42,     0,   603,     0,    43,
       0,    44,    45,    46,    47,    48,    49,   101,    50,    51,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,   102,    58,    59,    60,    61,    62,    63,   103,     0,
       0,     0,     0,     0,   104,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
     107,     0,     0,   108,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,   111,     0,   112,     0,     0,     0,     0,
       0,     0,     0,    65,   113,     0,     0,    67,   114,     0,
       0,     0,     0,   115,   116,   117,   118,     0,     0,   119,
      86,    39,    87,     0,   120,   121,     0,    91,     0,    40,
      92,     0,     0,    93,    94,    95,     0,    97,     0,     0,
       0,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   101,    50,    51,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,   102,    58,    59,
      60,    61,    62,    63,   103,     0,     0,     0,     0,     0,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,   106,
       0,     0,     0,     0,     0,     0,   107,     0,     0,   108,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   111,
       0,   112,     0,     0,     0,     0,     0,     0,     0,    65,
     113,     0,     0,    67,   114,    39,     0,     0,     0,   115,
     116,   117,   118,    40,     0,   119,     0,     0,     0,     0,
     120,   121,    41,     0,     0,    42,     0,     0,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,     0,
      40,     0,    58,    59,    60,    61,    62,    63,     0,   575,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,    22,     0,    86,    39,    87,     0,     0,
       0,     0,    91,    64,    40,    92,     0,     0,     0,     0,
       0,     0,    97,    65,    66,     0,    42,    67,    68,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   101,    50,
      51,    52,    53,    69,    54,    55,    56,     0,     0,    57,
      64,     0,   102,    58,    59,    60,    61,    62,    63,     0,
      65,    66,     0,     0,    67,    68,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     576,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,   107,     0,     0,   108,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    23,
     110,     0,     0,     0,   173,     0,   112,     0,     0,     0,
     582,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    86,    39,    87,     0,     0,     0,     0,    91,     0,
      40,    92,     0,     0,     0,     0,     0,     0,    97,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   101,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   102,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   241,     0,     0,
     106,     0,     0,     0,     0,     0,     0,   107,     0,     0,
     108,   152,   109,     0,   153,     0,     0,     0,     0,     0,
     259,   154,     0,     0,     0,     0,   110,     0,     0,     0,
     173,   260,   112,     0,     0,     0,     0,   261,   243,     0,
      65,   113,   155,   244,    67,   114,   157,   245,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    86,    39,    87,
       0,     0,     0,     0,    91,     0,    40,    92,     0,     0,
       0,     0,     0,     0,    97,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     101,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   102,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   241,     0,     0,   106,     0,     0,     0,
       0,     0,     0,   107,     0,     0,   108,     0,   109,     0,
     153,     0,     0,     0,     0,     0,     0,   154,     0,     0,
       0,     0,   110,   242,     0,     0,   173,     0,   112,     0,
       0,     0,     0,     0,   243,     0,    65,   113,   155,   244,
      67,   114,   157,   245,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    86,    39,    87,     0,     0,     0,     0,
      91,     0,    40,    92,     0,     0,     0,     0,     0,     0,
      97,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   101,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     102,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   241,
       0,     0,   106,     0,     0,     0,     0,     0,     0,   107,
       0,     0,   108,     0,   109,     0,   153,     0,     0,     0,
       0,     0,     0,   154,     0,     0,     0,     0,   110,     0,
       0,     0,   173,     0,   112,     0,     0,     0,     0,     0,
     243,     0,    65,   113,   155,   244,    67,   114,   157,   245,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    86,
      39,    87,     0,     0,     0,     0,    91,     0,    40,    92,
       0,     0,     0,     0,     0,     0,    97,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   101,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   102,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   107,     0,     0,   108,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   581,     0,     0,   110,     0,     0,     0,   173,     0,
     112,     0,     0,     0,   582,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    86,    39,    87,     0,     0,
       0,     0,    91,     0,    40,    92,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   101,    50,
      51,    52,    53,     0,    54,    55,    56,     0,   362,    57,
       0,     0,   102,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,   107,     0,     0,   108,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   173,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    86,    39,    87,     0,     0,     0,     0,    91,     0,
      40,    92,     0,     0,     0,     0,     0,     0,    97,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   101,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   102,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,     0,   107,     0,     0,
     108,     0,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     173,     0,   112,     0,     0,     0,   582,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    86,    39,    87,
       0,     0,     0,     0,    91,     0,    40,    92,     0,     0,
       0,     0,     0,     0,    97,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     101,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   102,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,   107,     0,     0,   108,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   173,     0,   112,     0,
       0,     0,     0,     0,     0,     0,    65,   113,     0,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
       0,     0,   119,    86,    39,    87,     0,     0,     0,     0,
      91,     0,    40,    92,     0,     0,     0,     0,     0,     0,
     361,     0,     0,     0,    42,     0,     0,     0,    43,     0,
      44,    45,    46,    47,    48,    49,   101,    50,    51,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
     102,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   106,     0,     0,     0,     0,     0,     0,   107,
       0,     0,   108,     0,   109,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,   173,     0,   112,     0,     0,     0,     0,     0,
       0,     0,    65,   113,     0,     0,    67,   114,     0,     0,
       0,     0,   115,   116,   117,   118,     0,     0,   119,    86,
      39,    87,     0,     0,     0,     0,    91,     0,    40,    92,
       0,     0,     0,     0,     0,     0,    97,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   101,    50,    51,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,   102,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   173,     0,
     112,     0,     0,     0,     0,     0,     0,     0,    65,   113,
       0,     0,    67,   114,     0,     0,     0,     0,   115,   116,
     117,   118,     0,     0,   119,    86,    39,    87,     0,     0,
       0,     0,    91,     0,    40,    92,     0,     0,     0,     0,
       0,     0,    97,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   101,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     295,   107,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   173,     0,   112,     0,     0,     0,
       0,     0,     0,     0,    65,   113,     0,     0,    67,   114,
       0,     0,     0,     0,   115,   116,   117,   118,     0,     0,
     119,    86,    39,    87,     0,     0,     0,     0,    91,     0,
      40,    92,     0,     0,     0,     0,     0,     0,    97,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   101,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,   400,     0,     0,   107,     0,     0,
       0,     0,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
     173,     0,   112,     0,     0,     0,     0,     0,     0,     0,
      65,   113,     0,     0,    67,   114,     0,     0,     0,     0,
     115,   116,   117,   118,     0,     0,   119,    86,    39,    87,
       0,     0,     0,     0,    91,     0,    40,    92,     0,     0,
       0,     0,     0,     0,    97,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     101,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   106,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,   173,     0,   112,     0,
      39,     0,     0,     0,     0,     0,    65,   113,    40,     0,
      67,   114,     0,     0,     0,     0,   115,   116,   117,   118,
      42,     0,   119,     0,   325,     0,    44,    45,    46,   176,
     177,   178,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   331,     0,     0,     0,     0,
       0,     0,   332,     0,     0,   179,     0,     0,     0,   333,
     180,     0,   181,     0,     0,     0,     0,     0,     0,     0,
     182,     0,     0,     0,   183,     0,    39,     0,   184,   334,
     185,     0,     0,     0,    40,   261,     0,     0,   186,    66,
     335,   336,     0,    68,   337,   338,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   241,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,  -198,     0,     0,   180,     0,   181,     0,
       0,     0,     0,     0,     0,     0,   182,     0,     0,     0,
     183,    39,     0,     0,   184,     0,   185,     0,     0,    40,
       0,     0,   769,     0,   186,    66,     0,   244,     0,    68,
       0,    42,     0,     0,     0,     0,     0,    44,    45,    46,
     176,   177,   178,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   241,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   179,     0,     0,     0,
       0,   180,     0,   181,     0,     0,     0,     0,     0,     0,
       0,   182,     0,     0,     0,   183,    39,     0,     0,   184,
       0,   185,     0,     0,    40,     0,     0,   769,     0,   186,
      66,   215,   244,     0,    68,     0,    42,     0,     0,     0,
       0,     0,    44,    45,    46,   176,   177,   178,     0,   216,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,     0,     0,     0,   180,     0,   181,     0,
       0,     0,     0,     0,     0,     0,   182,    39,     0,     0,
     183,     0,     0,     0,   184,    40,   185,     0,     0,     0,
       0,     0,     0,     0,   186,    66,     0,    42,     0,    68,
       0,   325,     0,    44,    45,    46,   176,   177,   178,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   179,     0,     0,     0,     0,   180,     0,   181,
       0,     0,     0,     0,     0,     0,     0,   182,    39,     0,
       0,   183,   326,     0,     0,   184,    40,   185,     0,     0,
       0,     0,     0,   680,     0,   186,    66,     0,    42,     0,
      68,     0,     0,     0,    44,    45,    46,   176,   177,   178,
       0,   681,     0,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,    39,     0,     0,     0,   180,     0,
     181,     0,    40,     0,     0,     0,     0,     0,   182,     0,
       0,     0,   183,     0,    42,     0,   184,     0,   185,     0,
      44,    45,    46,   176,   177,   178,   186,    66,     0,    52,
      53,    68,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   354,   179,
       0,     0,     0,     0,   180,     0,   181,     0,     0,     0,
       0,     0,     0,     0,   182,    39,     0,     0,   183,     0,
       0,     0,   184,    40,   185,     0,     0,     0,     0,     0,
       0,     0,   186,    66,     0,    42,     0,    68,     0,   325,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,   180,     0,   181,     0,     0,
       0,     0,     0,     0,     0,   182,    39,     0,     0,   183,
       0,     0,     0,   184,    40,   185,     0,     0,     0,     0,
       0,     0,     0,   186,    66,     0,    42,     0,    68,     0,
     554,     0,    44,    45,    46,   176,   177,   178,     0,     0,
       0,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
     184,   776,   185,     0,     0,    40,     0,     0,     0,     0,
     186,    66,     0,     0,     0,    68,     0,    42,     0,     0,
       0,   325,     0,    44,    45,    46,   176,   177,   178,     0,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   179,    39,     0,     0,     0,   180,     0,   181,
       0,    40,     0,     0,     0,     0,     0,   182,     0,     0,
       0,   183,     0,    42,     0,   793,     0,   185,     0,    44,
      45,    46,   176,   177,   178,   186,    66,     0,    52,    53,
      68,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   802,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,    39,
       0,     0,     0,   180,     0,   181,     0,    40,     0,     0,
       0,     0,     0,   182,     0,     0,     0,   183,     0,    42,
       0,   184,     0,   185,     0,    44,    45,    46,   176,   177,
     178,   186,    66,     0,    52,    53,    68,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   805,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   179,    39,     0,     0,     0,   180,
       0,   181,     0,    40,     0,     0,     0,     0,     0,   182,
       0,     0,     0,   183,     0,    42,     0,   184,     0,   185,
       0,    44,    45,    46,   176,   177,   178,   186,    66,     0,
      52,    53,    68,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,   180,     0,   181,     0,     0,
       0,     0,     0,     0,     0,   182,     0,     0,     0,   183,
      39,     0,     0,   184,     0,   185,     0,     0,    40,     0,
       0,     0,     0,   186,    66,     0,     0,    41,    68,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,     0,    50,    51,    52,    53,    39,    54,    55,
      56,     0,     0,    57,     0,    40,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,    42,     0,     0,
       0,    43,     0,    44,    45,    46,    47,    48,    49,     0,
      50,    51,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,   307,     0,     0,     0,     0,     0,     0,    65,    66,
       0,     0,    67,    68,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,    64,     0,     0,     0,    40,
       0,     0,     0,     0,     0,    65,    66,     0,   575,    67,
      68,    42,     0,     0,     0,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,     0,    40,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,    64,
      40,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,     0,    42,    67,    68,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    39,
      54,    55,    56,     0,     0,    57,    64,    40,     0,    58,
      59,    60,    61,    62,    63,     0,    65,    66,     0,    42,
      67,    68,     0,     0,     0,    44,    45,    46,   176,   177,
     178,     0,     0,     0,    52,    53,     0,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,   113,    42,     0,    67,   114,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,    39,
      54,    55,    56,     0,     0,    57,     0,    40,     0,    58,
      59,    60,    61,    62,    63,     0,     0,   186,    66,    42,
       0,     0,    68,    43,     0,    44,    45,    46,    47,    48,
      49,     0,    50,    51,    52,    53,   452,    54,    55,    56,
       0,     0,    57,     0,     0,     0,    58,    59,    60,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
     227,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    42,     0,     0,    67,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,     0,    40,   227,    58,    59,
      60,    61,    62,    63,     0,     0,     0,    65,    42,     0,
       0,    67,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,    39,    54,    55,    56,     0,
       0,    57,     0,    40,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,    65,
     113,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   319,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,     0,     0,     0,
       0,    39,   706,     0,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,   707,   594,     0,     0,     0,     0,
       0,    42,     0,   595,     0,    43,     0,    44,    45,    46,
      47,    48,    49,   186,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,     0,    40,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,    42,     0,
       0,     0,     0,     0,    44,    45,    46,   176,   177,   178,
       0,     0,     0,    52,    53,    39,    54,    55,    56,     0,
       0,    57,     0,    40,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   176,   177,   178,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,    65,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   593,   594,     0,
       0,     0,     0,     0,     0,     0,   595,     0,     0,     0,
       0,    39,     0,     0,     0,     0,   186,     0,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,     0,     0,   766,   594,     0,    44,    45,    46,
     176,   177,   178,   595,    39,     0,    52,    53,     0,    54,
      55,    56,    40,   186,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,    42,     0,     0,     0,     0,     0,
      44,    45,    46,   176,   177,   178,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,    39,     0,     0,
       0,     0,     0,     0,     0,    40,     0,     0,     0,     0,
       0,   594,     0,     0,     0,     0,     0,    42,     0,   595,
       0,     0,     0,    44,    45,    46,   176,   177,   178,   186,
       0,     0,    52,    53,     0,    54,    55,    56,     0,     0,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
       0,     0,   595,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   186,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   186
  };

  const short
  parser::yycheck_[] =
  {
      88,    34,   190,   105,    88,    88,   112,   105,   211,   212,
      41,    99,   278,   110,   111,    99,   230,   312,   110,   215,
     316,    29,   205,   259,   349,   441,    29,    99,   182,     1,
     183,   219,   284,   101,   555,    64,   183,   184,   282,     5,
     639,   555,   638,   290,   471,     1,   714,    12,   201,   648,
      21,    64,    83,   361,   201,   179,   180,   132,   111,    19,
      27,   297,   276,   210,   478,    77,    34,    77,   192,    27,
     714,   119,   243,   368,    99,    29,   173,     1,   184,   185,
     108,   554,   111,     1,    19,    90,   746,   382,   104,     1,
     106,   179,   180,    80,   142,   183,   184,     0,   111,   101,
     175,   104,   516,    77,   192,   133,    65,    66,    77,   708,
     243,   113,   200,   201,   285,     1,   200,    84,   105,   132,
     173,   133,   210,   133,   480,   272,    84,   215,   216,   134,
     790,   215,   216,   104,   281,   107,    71,   118,   446,     1,
     808,   497,   110,   111,   173,   119,   118,   112,   108,   182,
     119,   107,   285,   105,   406,   582,   164,   404,   166,   515,
     173,   164,   175,   166,   808,    81,   810,    78,   112,   316,
     643,   295,   247,    84,    80,   200,   471,   413,   211,   212,
     594,   595,   113,   107,   272,   101,    22,    14,   119,   107,
     215,   216,   280,   281,   118,   107,   280,   280,   227,    80,
     118,   348,   168,   169,   560,   173,   118,   295,   119,    80,
     164,    80,   166,   119,   227,    51,    52,    77,   105,    80,
     102,   107,   225,   637,   106,   107,   259,   109,   316,    80,
     354,   118,   118,    80,   247,    79,   389,   836,   119,   835,
     839,    80,   389,   211,   212,   107,    87,   276,   119,   282,
     119,    92,   134,   549,   550,   671,   118,   109,   119,   347,
     348,   349,   350,   347,   297,   298,   354,   119,   119,   113,
     273,   274,   119,   105,   112,   119,   112,    80,    81,   523,
     119,   125,   313,   133,   252,   123,   118,   123,   119,   127,
     319,   127,   105,   588,   362,   298,   398,   711,   623,   387,
     398,   389,   823,    80,    81,   118,   381,   495,   511,   823,
     407,   142,   409,   608,   566,   407,   388,   409,    77,   416,
     736,   418,   347,   477,   416,   350,   418,   105,   475,   427,
     101,   484,   746,   104,   481,   410,   572,   484,    93,   553,
     118,    96,   104,   112,   106,   115,   434,   500,   103,   119,
     434,   434,   105,   500,   123,   237,    27,   239,   127,   105,
      93,   202,   203,    96,   104,   118,   106,   514,   381,   124,
     103,    24,   118,   128,   123,   113,   790,   629,   127,   793,
     413,   119,   113,   739,    79,   741,   118,   475,   119,   745,
     478,   124,   480,   481,   427,   349,   484,   410,    93,   113,
     695,   768,   549,   550,   771,   119,   115,   603,   704,   497,
     119,   115,   500,   616,   617,   119,   668,   299,   115,   104,
     113,   106,   119,   112,   530,   531,   514,   515,   516,   124,
     125,   756,   615,   128,   129,    72,    73,    74,    75,   407,
     118,   409,   283,   284,   477,   104,   593,   106,   416,   104,
     418,   106,    37,    79,   462,   488,   489,    80,    81,   462,
      86,   549,   550,   112,   467,    91,    84,    93,    86,   104,
      14,   106,   560,   134,   571,   800,   108,   108,   511,   571,
     775,   112,    79,   114,   680,   104,   752,   106,    87,    86,
     523,   133,   123,   113,   120,   731,    93,   822,   124,   125,
     714,   113,   128,   129,   113,   593,   594,   595,   462,   113,
     805,   806,   113,   467,     4,   603,    80,    81,    23,   603,
     488,   489,   646,   120,   553,   120,   121,   124,   125,   119,
      27,   128,   129,    91,   118,   623,   832,   570,    77,   572,
      77,    77,   383,   511,   575,    77,    15,    81,    41,   637,
     109,   754,   640,   700,   109,   751,   640,   704,   646,   400,
      79,   113,   119,   113,   405,   406,   407,   570,   113,    71,
     133,   574,   113,   113,    93,   105,   764,   119,   603,   105,
     109,   119,    80,   616,   617,    91,   113,    25,   113,   113,
     714,   679,   680,   681,   808,   679,   680,   681,   108,   746,
      80,   120,   112,   571,   114,   124,   125,   119,   119,   128,
     129,    85,   700,   123,   113,   640,   704,   127,   706,   707,
     574,    85,   706,   711,   707,    27,   714,    81,   109,   470,
     471,   827,   133,   730,   134,    77,   724,    77,   730,    80,
     724,   113,    77,   120,   768,   120,   793,   771,   616,   617,
      77,   739,   685,   741,   679,   680,   681,   745,   746,    80,
     663,   119,   503,   751,   505,    27,   507,   751,   756,   757,
     758,   769,   774,   757,   758,    81,   774,   120,    81,   105,
     768,   706,   707,   771,   808,   832,   810,   793,   119,   119,
     101,   101,   112,   781,   113,   649,   120,   781,   731,   724,
      84,   109,   790,   120,   109,   793,   119,    78,   120,   663,
     798,   120,   800,    91,   798,   798,    77,   685,    28,   807,
     808,   754,   810,   807,   565,   566,   751,   815,    27,     9,
     113,   815,   757,   758,   822,    79,   105,    81,   118,   827,
      84,   582,    37,   827,   832,   113,    79,    80,    27,    93,
      80,    80,    96,    84,   113,    11,   781,    19,    86,   103,
      93,    53,   730,    96,    91,    79,    86,    91,   123,   119,
     103,   725,    86,   113,   113,    84,   120,    84,   164,    93,
     124,   125,   807,   105,   128,   129,   754,   120,   629,   630,
     815,   124,   125,   105,   577,   128,   129,   123,   574,   113,
     663,   822,   827,   758,    34,   119,   691,   744,   742,   280,
     124,   125,   798,   685,   128,   129,   489,   617,   754,   212,
      15,   700,   550,   272,   815,   201,   667,   668,   669,   516,
       3,     4,     5,     6,     7,     8,   719,    10,   712,    12,
      13,   768,   812,    16,    17,    18,    19,    20,   477,    22,
     651,    24,   725,    26,    34,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   108,    42,
      43,    44,    79,   287,    47,   668,   667,    50,    51,    52,
      53,    54,    55,    56,    57,   671,    93,    79,   237,    96,
      63,   505,   203,   570,   398,   553,   103,   774,    -1,   247,
      -1,    93,   397,    76,    96,    -1,    -1,    -1,   749,    82,
      -1,   103,    -1,   120,    -1,    -1,    89,   124,   125,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,
      -1,    -1,   124,   125,    -1,   108,   128,   129,    -1,   112,
      -1,   114,    -1,    -1,   785,    -1,    -1,    -1,    -1,   122,
     123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,
     133,   134,   135,    -1,    -1,   138,    -1,    -1,    -1,    -1,
     143,   144,     3,     4,     5,    -1,     7,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,    20,
      -1,    22,    -1,    24,    -1,    26,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    79,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    93,    -1,    76,    96,    -1,    -1,    -1,
      -1,    82,    -1,   103,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,    -1,   108,   128,   129,
      -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,
      -1,   132,   133,   134,   135,    -1,    -1,   138,    -1,    -1,
      -1,    -1,   143,   144,     3,     4,     5,    -1,     7,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    24,    -1,    26,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,
      -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,
       3,     4,     5,    -1,   143,   144,    -1,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,    -1,    -1,   126,   127,     4,    -1,    -1,    -1,   132,
     133,   134,   135,    12,    -1,   138,    -1,    -1,    -1,    -1,
     143,   144,    21,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,    21,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,   112,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,   122,   123,    -1,    24,   126,   127,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   142,    42,    43,    44,    -1,    -1,    47,
     112,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
     102,   103,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,   113,   114,    -1,    -1,    -1,    -1,   119,   120,    -1,
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
      -1,    -1,   108,   109,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,   120,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
     120,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
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
      -1,   105,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,   118,    -1,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    -1,    -1,   138,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    46,    47,
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
     112,    -1,   114,    -1,    -1,    -1,   118,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
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
      88,    89,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,
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
      82,    -1,    -1,    -1,    86,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
       4,    -1,    -1,    -1,    -1,    -1,   122,   123,    12,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      24,    -1,   138,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    89,    -1,    -1,    -1,    93,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,     4,    -1,   112,   113,
     114,    -1,    -1,    -1,    12,   119,    -1,    -1,   122,   123,
     124,   125,    -1,   127,   128,   129,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    -1,    -1,    94,    -1,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,     4,    -1,    -1,   112,    -1,   114,    -1,    -1,    12,
      -1,    -1,   120,    -1,   122,   123,    -1,   125,    -1,   127,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,
      -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,   108,     4,    -1,    -1,   112,
      -1,   114,    -1,    -1,    12,    -1,    -1,   120,    -1,   122,
     123,    19,   125,    -1,   127,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    96,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,     4,    -1,    -1,
     108,    -1,    -1,    -1,   112,    12,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,    -1,    24,    -1,   127,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    96,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,     4,    -1,
      -1,   108,   109,    -1,    -1,   112,    12,   114,    -1,    -1,
      -1,    -1,    -1,    19,    -1,   122,   123,    -1,    24,    -1,
     127,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,     4,    -1,    -1,    -1,    94,    -1,
      96,    -1,    12,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,    24,    -1,   112,    -1,   114,    -1,
      30,    31,    32,    33,    34,    35,   122,   123,    -1,    39,
      40,   127,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    89,
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
      -1,    -1,    -1,   112,    12,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,    -1,    24,    -1,   127,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,     4,    -1,    -1,
     112,   113,   114,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,    -1,   127,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,     4,    -1,    -1,    -1,    94,    -1,    96,
      -1,    12,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,    -1,    24,    -1,   112,    -1,   114,    -1,    30,
      31,    32,    33,    34,    35,   122,   123,    -1,    39,    40,
     127,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,     4,
      -1,    -1,    -1,    94,    -1,    96,    -1,    12,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,    24,
      -1,   112,    -1,   114,    -1,    30,    31,    32,    33,    34,
      35,   122,   123,    -1,    39,    40,   127,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,
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
       4,    -1,    -1,   112,    -1,   114,    -1,    -1,    12,    -1,
      -1,    -1,    -1,   122,   123,    -1,    -1,    21,   127,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
      -1,    -1,   126,   127,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,   112,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    21,   126,
     127,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   112,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,    -1,    24,   126,   127,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,   112,    12,    -1,    51,
      52,    53,    54,    55,    56,    -1,   122,   123,    -1,    24,
     126,   127,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    24,    -1,   126,   127,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,   122,   123,    24,
      -1,    -1,   127,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    78,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    24,    -1,    -1,   126,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,   112,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,   122,    24,    -1,
      -1,   126,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,   122,
     123,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,     4,    91,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   103,   104,    -1,    -1,    -1,    -1,
      -1,    24,    -1,   112,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,   122,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,   122,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,   122,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,   103,   104,    -1,    30,    31,    32,
      33,    34,    35,   112,     4,    -1,    39,    40,    -1,    42,
      43,    44,    12,   122,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,    -1,    -1,    24,    -1,   112,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,   122,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   146,   147,   148,   151,   123,   127,   352,
     152,   163,     0,   152,    65,    66,   149,   105,   118,   153,
     164,   165,     1,   107,   351,   108,   133,   219,   219,   112,
     154,    14,   166,   177,   178,   133,   220,    77,    77,     4,
      12,    21,    24,    28,    30,    31,    32,    33,    34,    35,
      37,    38,    39,    40,    42,    43,    44,    47,    51,    52,
      53,    54,    55,    56,   112,   122,   123,   126,   127,   142,
     155,   156,   157,   161,   318,   321,   322,   335,   337,   338,
     344,    27,    24,   167,   118,   162,     3,     5,     6,     7,
       8,    10,    13,    16,    17,    18,    19,    20,    22,    26,
      29,    36,    50,    57,    63,    76,    82,    89,    92,    94,
     108,   112,   114,   123,   127,   132,   133,   134,   135,   138,
     143,   144,   175,   179,   180,   181,   182,   184,   199,   224,
     264,   269,   273,   274,   276,   277,   278,   279,   307,   308,
     311,   312,   334,   335,   338,   346,   347,   350,   109,   119,
     352,    79,    93,    96,   103,   124,   125,   128,   129,   341,
     342,   343,   345,   113,   119,   142,   112,   158,   104,   106,
     150,   352,   118,   112,   272,   273,    33,    34,    35,    89,
      94,    96,   104,   108,   112,   114,   122,   204,   229,   231,
     233,   234,   236,   240,   316,   317,   321,   331,   333,   344,
      37,   112,   104,   106,   299,   272,    72,    73,    74,    75,
     185,   104,   106,   216,   217,    19,    37,   183,   231,   233,
     317,    14,   299,   277,   108,   270,   271,   112,   335,   108,
     112,   309,   310,   312,   347,   277,   297,   298,   277,   276,
     277,    79,   109,   120,   125,   129,   272,   273,   281,   283,
     284,   314,   328,   330,   340,   341,   343,   348,   349,   102,
     113,   119,   280,   281,   282,   341,   342,   343,   348,   353,
     115,   353,    19,   270,   270,   134,   174,   162,    19,    71,
     206,    80,   119,    81,    84,   120,   266,   267,   268,   314,
     327,   329,   339,   341,   342,    88,   277,   101,   104,    87,
     133,   113,   113,   113,   113,   113,   157,    78,   159,   160,
     161,   152,   152,     4,   168,    23,    80,   240,   240,   112,
     224,   257,   258,   259,   338,    28,   109,   226,   227,   229,
     231,    79,    86,    93,   113,   124,   125,   128,   129,   226,
     244,   323,   324,   353,   353,    27,   210,    91,    86,   120,
     238,   319,   323,   332,    88,   237,   240,   231,   226,   243,
     244,    20,    46,   272,   296,   300,   301,   302,   300,   118,
     275,    77,    77,    77,    77,   222,   227,   241,   215,   264,
     265,   273,   215,    15,   194,   231,   231,    80,   119,    81,
      41,    90,   134,   335,    77,   133,   349,    80,   119,   221,
      86,   297,   337,   346,   327,    78,    84,   119,   109,   119,
     273,   336,   338,   101,   113,   113,   119,   113,   119,   113,
     113,   113,   119,   115,   241,   335,   335,   120,   176,   313,
     325,   326,   342,   349,   206,   133,   204,   223,   227,   228,
     334,   272,   286,   287,   302,   337,    27,   218,   268,   274,
     240,   336,    78,   303,   304,   305,   335,   336,   338,   277,
     113,   113,   119,   105,   351,   352,    12,   112,   169,   170,
     104,   106,   288,   222,   342,    80,   105,   119,   245,   109,
      80,    91,   113,   113,   119,   113,   113,   115,   104,   106,
     209,   231,   227,   321,   333,   233,   240,    80,   201,   113,
     119,   217,   299,    85,   105,   118,   351,    25,    27,   214,
     105,   118,   351,   272,    81,    80,    81,   202,   227,   248,
     112,   317,   226,   133,   134,   109,    77,    77,   113,   108,
     112,   114,   315,   316,   309,    77,   272,   120,   120,   272,
     285,   302,   272,   281,   281,   336,   281,   281,    77,    80,
      80,   338,   347,   119,    28,   205,   229,   231,    77,   133,
      80,    81,   200,   252,   218,    81,   119,   120,   217,   105,
     119,    81,   101,   161,   112,    21,   142,   161,   171,   172,
     173,   105,   118,   272,   289,   290,   291,   295,   289,   351,
     113,   227,   259,   103,   104,   112,   246,   247,   331,   248,
     227,   226,     7,    26,   195,   207,   208,   265,   208,   120,
     120,   248,   226,   272,   301,   272,   104,   106,   213,   265,
     227,   248,   246,    84,   188,   334,   109,   113,   115,   119,
      78,   222,   225,   225,   120,   120,   325,   245,   200,   252,
      91,    77,   248,    28,   253,   254,   255,    27,   249,     9,
     260,   261,   262,   272,   302,   304,   281,   306,   336,   171,
     352,   158,   113,   119,   142,   289,   105,   118,    84,    86,
     292,   293,   294,   351,   227,   331,   331,   113,    37,   196,
      19,    37,   194,   231,   105,   118,   351,   275,     7,    26,
     198,   199,   211,   212,   265,   212,   189,   333,    27,   191,
      80,   302,   272,    77,   119,    77,    91,   103,   249,   260,
     231,   245,    84,   230,   235,   239,   240,   256,   104,   106,
     260,    22,    51,    52,   112,   186,   263,   320,   321,   262,
      81,   101,   113,   173,   291,   286,   272,   218,   294,    80,
     105,    80,   231,   194,   231,    80,    81,   203,   207,    11,
      19,   197,   197,   105,   118,   351,    86,   104,   106,   192,
     223,   222,   231,   229,   233,   260,   103,   254,    91,   120,
     239,   313,   250,   251,   275,   250,   113,   231,   232,   242,
     263,    53,   187,   281,   336,    86,   218,   248,   248,   201,
      81,   203,   248,   112,   247,   331,   272,   194,   206,   211,
     190,   333,    78,   193,   194,    78,   193,    91,   235,   256,
     235,   105,   118,   310,   351,   119,   113,   231,   272,   105,
     113,   331,    84,   205,   333,   105,   105,   118,   351,   351,
     231,   251,    80,   242,   189,   200,   252,   194,   222,   249,
     260,   260
  };

  const short
  parser::yyr1_[] =
  {
       0,   145,   146,   147,   147,   148,   149,   149,   149,   150,
     150,   151,   151,   152,   153,   153,   153,   154,   154,   155,
     155,   155,   155,   156,   156,   157,   157,   158,   158,   158,
     159,   159,   160,   160,   161,   161,   162,   162,   163,   163,
     164,   165,   165,   166,   167,   167,   168,   168,   169,   169,
     170,   170,   171,   171,   171,   171,   172,   172,   173,   173,
     174,   174,   175,   175,   175,   176,   176,   177,   178,   178,
     179,   179,   179,   179,   179,   179,   179,   179,   180,   181,
     181,   181,   181,   181,   182,   183,   183,   184,   184,   184,
     184,   185,   185,   185,   185,   185,   186,   186,   186,   187,
     188,   188,   189,   190,   190,   191,   191,   192,   192,   192,
     192,   193,   193,   193,   193,   194,   195,   195,   195,   195,
     195,   196,   196,   197,   197,   198,   198,   198,   199,   199,
     200,   200,   201,   201,   202,   202,   202,   203,   203,   203,
     204,   204,   205,   205,   205,   205,   206,   206,   206,   207,
     207,   208,   208,   208,   208,   209,   209,   210,   210,   211,
     211,   212,   212,   212,   212,   213,   213,   214,   214,   215,
     215,   215,   215,   216,   216,   217,   218,   218,   219,   219,
     220,   220,   220,   221,   221,   222,   223,   224,   224,   225,
     225,   226,   226,   227,   227,   227,   228,   229,   230,   231,
     231,   232,   233,   234,   234,   235,   235,   236,   236,   236,
     237,   238,   238,   239,   240,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   241,   242,   242,   243,   243,   244,
     244,   245,   245,   246,   246,   246,   247,   247,   248,   249,
     249,   249,   250,   250,   251,   252,   253,   253,   254,   254,
     255,   255,   256,   256,   257,   257,   258,   258,   259,   260,
     260,   261,   261,   262,   262,   262,   263,   263,   263,   264,
     264,   265,   266,   266,   267,   267,   268,   269,   269,   269,
     269,   269,   269,   269,   269,   269,   270,   270,   271,   271,
     272,   272,   273,   273,   274,   274,   275,   275,   276,   276,
     276,   276,   277,   277,   277,   277,   277,   277,   277,   277,
     277,   277,   278,   278,   278,   279,   279,   279,   279,   279,
     279,   279,   279,   280,   280,   281,   281,   281,   282,   282,
     283,   283,   283,   283,   283,   283,   283,   284,   284,   285,
     285,   286,   287,   287,   288,   288,   288,   288,   289,   289,
     290,   290,   290,   291,   292,   292,   293,   293,   294,   295,
     296,   297,   298,   298,   299,   299,   300,   300,   300,   300,
     301,   301,   302,   302,   302,   303,   303,   304,   304,   304,
     305,   305,   305,   305,   306,   306,   307,   307,   308,   308,
     309,   309,   309,   310,   310,   311,   311,   311,   311,   312,
     312,   313,   313,   314,   314,   315,   315,   315,   316,   316,
     316,   316,   316,   317,   317,   318,   318,   318,   318,   319,
     319,   320,   321,   321,   322,   323,   323,   323,   324,   324,
     324,   324,   325,   325,   326,   326,   327,   327,   328,   328,
     329,   329,   330,   330,   331,   332,   333,   333,   333,   333,
     333,   334,   334,   335,   335,   335,   336,   337,   337,   338,
     338,   338,   338,   338,   338,   338,   338,   339,   339,   340,
     340,   341,   342,   342,   343,   343,   344,   344,   344,   344,
     344,   344,   344,   344,   344,   344,   344,   344,   344,   344,
     344,   344,   344,   344,   345,   345,   346,   346,   347,   348,
     348,   349,   349,   350,   350,   350,   350,   350,   351,   351,
     352,   352,   353,   353
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       0,     2,     1,     3,     1,     2,     2,     0,     3,     3,
       0,     1,     3,     1,     1,     1,     2,     1,     2,     0,
       2,     3,     0,     5,     1,     0,     2,     0,     1,     0,
       3,     4,     1,     0,     2,     1,     3,     1,     2,     2,
       0,     1,     1,     1,     1,     3,     1,     2,     3,     0,
       1,     1,     1,     1,     4,     7,     1,     1,     3,     4,
       5,     6,     6,     4,     4,     3,     1,     4,     3,     6,
       7,     2,     2,     2,     2,     0,     1,     1,     1,     2,
       0,     2,     3,     2,     1,     0,     2,     3,     3,     3,
       3,     3,     2,     1,     0,     3,     4,     3,     4,     2,
       3,     0,     1,     0,     1,     3,     6,     7,     1,     1,
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
       3,     2,     0,     1,     3,     5,     1,     5,     1,     4,
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
     566,   567,   568,   570,   571,   573,   574,   576,   577,   578,
     580,   581,   583,   584,   586,   587,   591,   592,   594,   595,
     597,   599,   600,   602,   615,   616,   618,   619,   621,   622,
     626,   627,   629,   630,   631,   632,   634,   635,   637,   638,
     643,   644,   646,   647,   648,   650,   651,   655,   657,   658,
     660,   661,   662,   663,   666,   667,   673,   675,   678,   681,
     682,   684,   685,   686,   688,   690,   691,   694,   695,   696,
     702,   709,   710,   711,   712,   713,   715,   716,   717,   719,
     730,   731,   733,   735,   736,   740,   741,   743,   744,   745,
     746,   748,   749,   750,   751,   753,   756,   758,   760,   762,
     763,   765,   765,   767,   767,   771,   773,   780,   787,   788,
     792,   793,   795,   796,   798,   799,   800,   802,   803,   804,
     807,   808,   811,   812,   813,   814,   816,   817,   818,   860,
     861,   863,   864,   865,   866,   868,   869,   871,   872,   874,
     875,   877,   878,   879,   880,   882,   883,   885,   886,   889,
     890,   891,   892,   894,   895,   897,   899,   900,   908,   909,
     911,   912,   913,   926,   927,   936,   938,   940,   941,   943,
     944,   953,   954,   956,   957,   959,   961,   970,   972,   974,
     975,   977,   980,   982,   983,   985,   986,   988,   990,   991,
     993,   995,   996,  1003,  1010,  1011,  1012,  1013,  1014,  1015,
    1016,  1017,  1023,  1024,  1027,  1029,  1030,  1032,  1033,  1035,
    1036,  1043,  1044,  1046,  1047,  1048,  1051,  1052,  1070,  1076,
    1077,  1078,  1080,  1081,  1083,  1085,  1087,  1088,  1090,  1091,
    1093,  1094,  1096,  1097,  1103,  1104,  1106,  1107,  1109,  1111,
    1112,  1114,  1115,  1117,  1118,  1119,  1121,  1122,  1123,  1128,
    1130,  1132,  1136,  1137,  1139,  1140,  1144,  1154,  1155,  1157,
    1158,  1159,  1160,  1161,  1162,  1163,  1166,  1167,  1169,  1170,
    1175,  1176,  1180,  1181,  1183,  1184,  1186,  1187,  1192,  1193,
    1194,  1195,  1198,  1199,  1200,  1201,  1202,  1204,  1206,  1207,
    1208,  1210,  1213,  1214,  1215,  1218,  1219,  1220,  1221,  1222,
    1223,  1228,  1229,  1232,  1233,  1238,  1239,  1240,  1245,  1246,
    1264,  1265,  1266,  1267,  1268,  1269,  1270,  1272,  1273,  1286,
    1288,  1298,  1300,  1301,  1304,  1305,  1306,  1307,  1309,  1310,
    1312,  1313,  1314,  1316,  1318,  1319,  1321,  1322,  1331,  1333,
    1335,  1337,  1339,  1340,  1343,  1344,  1346,  1347,  1348,  1349,
    1354,  1355,  1357,  1358,  1359,  1364,  1365,  1367,  1368,  1369,
    1371,  1372,  1373,  1374,  1377,  1378,  1410,  1411,  1413,  1414,
    1416,  1417,  1418,  1420,  1421,  1423,  1424,  1425,  1426,  1428,
    1429,  1431,  1432,  1434,  1435,  1438,  1439,  1440,  1442,  1443,
    1444,  1445,  1446,  1448,  1449,  1451,  1452,  1453,  1454,  1457,
    1458,  1460,  1462,  1463,  1467,  1469,  1470,  1471,  1473,  1474,
    1475,  1476,  1481,  1482,  1484,  1485,  1487,  1488,  1491,  1492,
    1497,  1498,  1500,  1501,  1505,  1507,  1509,  1510,  1511,  1512,
    1513,  1516,  1517,  1519,  1520,  1521,  1523,  1525,  1526,  1528,
    1529,  1530,  1531,  1532,  1533,  1534,  1535,  1537,  1538,  1540,
    1541,  1543,  1545,  1546,  1548,  1549,  1551,  1552,  1553,  1554,
    1555,  1556,  1557,  1558,  1559,  1560,  1561,  1562,  1563,  1564,
    1565,  1566,  1567,  1568,  1570,  1571,  1575,  1576,  1578,  1580,
    1581,  1583,  1584,  1588,  1589,  1590,  1591,  1592,  1597,  1600,
    1604,  1605,  1607,  1608
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
#line 7384 "parser.cc"

#line 1617 "parser.y"


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

