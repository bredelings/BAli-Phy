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
#line 516 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2526 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 533 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2532 "parser.cc"
    break;

  case 4: // module: body2
#line 534 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2538 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 536 "parser.y"
                                                                 {drv.push_module_context();}
#line 2544 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 544 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2550 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 545 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2556 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 547 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2562 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 548 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2568 "parser.cc"
    break;

  case 13: // top: semis top1
#line 551 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2574 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 553 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2580 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 554 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2586 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 555 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2592 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 563 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2598 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 564 "parser.y"
                                      {}
#line 2604 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 566 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2610 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 567 "parser.y"
                                      {}
#line 2616 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 568 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2622 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 569 "parser.y"
                                      {}
#line 2628 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 571 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2634 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2640 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 574 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2646 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 575 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2652 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 576 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2658 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 578 "parser.y"
                                      {}
#line 2664 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 579 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2670 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 580 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2676 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 582 "parser.y"
                   {}
#line 2682 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 583 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2688 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 585 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2694 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 586 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2700 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 588 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2706 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 589 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2712 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 599 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2718 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 601 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2724 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 602 "parser.y"
                         { }
#line 2730 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 604 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2738 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 617 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2744 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 618 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2750 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 620 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 2756 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 621 "parser.y"
                               { }
#line 2762 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 623 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2768 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 624 "parser.y"
                               { }
#line 2774 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 628 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2780 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 629 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 2786 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 631 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2792 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 632 "parser.y"
                                      {}
#line 2798 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 633 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2804 "parser.cc"
    break;

  case 56: // importlist: ','
#line 634 "parser.y"
                                      {}
#line 2810 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 636 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2816 "parser.cc"
    break;

  case 58: // importlist1: import
#line 637 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2822 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 639 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2828 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 640 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2834 "parser.cc"
    break;

  case 61: // prec: %empty
#line 645 "parser.y"
                   { }
#line 2840 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 646 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2846 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 648 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2852 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 649 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2858 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 650 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2864 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 652 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2870 "parser.cc"
    break;

  case 67: // ops: op
#line 653 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2876 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 657 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2882 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 659 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2888 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 660 "parser.y"
                                            { }
#line 2894 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 662 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2900 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 663 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2906 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 664 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2912 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 665 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2918 "parser.cc"
    break;

  case 75: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 668 "parser.y"
                                                         {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2924 "parser.cc"
    break;

  case 76: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 669 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2930 "parser.cc"
    break;

  case 77: // topdecl: decl_no_th
#line 675 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2936 "parser.cc"
    break;

  case 78: // topdecl: infixexp
#line 677 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2942 "parser.cc"
    break;

  case 79: // cl_decl: "class" tycl_hdr where_cls
#line 680 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2948 "parser.cc"
    break;

  case 80: // ty_decl: "type" type "=" ktype
#line 683 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2954 "parser.cc"
    break;

  case 81: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 684 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2960 "parser.cc"
    break;

  case 82: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 686 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ())};}
#line 2966 "parser.cc"
    break;

  case 83: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 687 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2972 "parser.cc"
    break;

  case 84: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 688 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 2978 "parser.cc"
    break;

  case 85: // standalone_kind_sig: "type" sks_vars "::" kind
#line 690 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 2984 "parser.cc"
    break;

  case 86: // sks_vars: sks_vars "," oqtycon
#line 692 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2990 "parser.cc"
    break;

  case 87: // sks_vars: oqtycon
#line 693 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2996 "parser.cc"
    break;

  case 88: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 696 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3002 "parser.cc"
    break;

  case 89: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 697 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3008 "parser.cc"
    break;

  case 90: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 699 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	   }
#line 3018 "parser.cc"
    break;

  case 91: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 705 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	   }
#line 3028 "parser.cc"
    break;

  case 92: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 711 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3034 "parser.cc"
    break;

  case 93: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 712 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3040 "parser.cc"
    break;

  case 94: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 713 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3046 "parser.cc"
    break;

  case 95: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 714 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3052 "parser.cc"
    break;

  case 96: // overlap_pragma: %empty
#line 715 "parser.y"
                                               {}
#line 3058 "parser.cc"
    break;

  case 106: // where_type_family: %empty
#line 742 "parser.y"
                                                           {}
#line 3064 "parser.cc"
    break;

  case 107: // where_type_family: "where" ty_fam_inst_eqn_list
#line 743 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3070 "parser.cc"
    break;

  case 108: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 745 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3076 "parser.cc"
    break;

  case 109: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 746 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3082 "parser.cc"
    break;

  case 110: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 747 "parser.y"
                                                           {}
#line 3088 "parser.cc"
    break;

  case 111: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 748 "parser.y"
                                                           {}
#line 3094 "parser.cc"
    break;

  case 112: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 750 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3100 "parser.cc"
    break;

  case 113: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 751 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3106 "parser.cc"
    break;

  case 114: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 752 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3112 "parser.cc"
    break;

  case 115: // ty_fam_inst_eqns: %empty
#line 753 "parser.y"
                                                           {}
#line 3118 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn: type "=" ctype
#line 755 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3124 "parser.cc"
    break;

  case 117: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 758 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3130 "parser.cc"
    break;

  case 118: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 760 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3136 "parser.cc"
    break;

  case 119: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 762 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3142 "parser.cc"
    break;

  case 120: // at_decl_cls: "type" ty_fam_inst_eqn
#line 764 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3148 "parser.cc"
    break;

  case 121: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 765 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3154 "parser.cc"
    break;

  case 126: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 773 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3160 "parser.cc"
    break;

  case 127: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 776 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), {}, {}, yystack_[1].value.as < Hs::ConstructorsDecl > ()))};
	      }
#line 3170 "parser.cc"
    break;

  case 128: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 783 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > ()))};
	      }
#line 3180 "parser.cc"
    break;

  case 129: // data_or_newtype: "data"
#line 789 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3186 "parser.cc"
    break;

  case 130: // data_or_newtype: "newtype"
#line 790 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3192 "parser.cc"
    break;

  case 131: // opt_class: %empty
#line 793 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3198 "parser.cc"
    break;

  case 132: // opt_class: qtycon
#line 794 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3204 "parser.cc"
    break;

  case 133: // opt_kind_sig: %empty
#line 798 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3210 "parser.cc"
    break;

  case 134: // opt_kind_sig: "::" kind
#line 799 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3216 "parser.cc"
    break;

  case 135: // opt_datafam_kind_sig: %empty
#line 801 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3222 "parser.cc"
    break;

  case 136: // opt_datafam_kind_sig: "::" kind
#line 802 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3228 "parser.cc"
    break;

  case 137: // opt_tyfam_kind_sig: %empty
#line 804 "parser.y"
                                      {}
#line 3234 "parser.cc"
    break;

  case 138: // opt_tyfam_kind_sig: "::" kind
#line 805 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3240 "parser.cc"
    break;

  case 139: // opt_tyfam_kind_sig: "=" tv_bndr
#line 806 "parser.y"
                                      {}
#line 3246 "parser.cc"
    break;

  case 140: // opt_at_kind_inj_sig: %empty
#line 808 "parser.y"
                                      {}
#line 3252 "parser.cc"
    break;

  case 141: // opt_at_kind_inj_sig: "::" kind
#line 809 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3258 "parser.cc"
    break;

  case 142: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 810 "parser.y"
                                                                  {}
#line 3264 "parser.cc"
    break;

  case 143: // tycl_hdr: context "=>" type
#line 813 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3270 "parser.cc"
    break;

  case 144: // tycl_hdr: type
#line 814 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3276 "parser.cc"
    break;

  case 145: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 817 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3282 "parser.cc"
    break;

  case 146: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 818 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3288 "parser.cc"
    break;

  case 147: // datafam_inst_hdr: context "=>" type
#line 819 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3294 "parser.cc"
    break;

  case 148: // datafam_inst_hdr: type
#line 820 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3300 "parser.cc"
    break;

  case 152: // decl_cls: at_decl_cls
#line 866 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3306 "parser.cc"
    break;

  case 153: // decl_cls: decl
#line 867 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3312 "parser.cc"
    break;

  case 154: // decls_cls: decls_cls ";" decl_cls
#line 869 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3318 "parser.cc"
    break;

  case 155: // decls_cls: decls_cls ";"
#line 870 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3324 "parser.cc"
    break;

  case 156: // decls_cls: decl_cls
#line 871 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3330 "parser.cc"
    break;

  case 157: // decls_cls: %empty
#line 872 "parser.y"
                                           {}
#line 3336 "parser.cc"
    break;

  case 158: // decllist_cls: "{" decls_cls "}"
#line 874 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3342 "parser.cc"
    break;

  case 159: // decllist_cls: "vocurly" decls_cls close
#line 875 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3348 "parser.cc"
    break;

  case 160: // where_cls: "where" decllist_cls
#line 877 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3354 "parser.cc"
    break;

  case 161: // where_cls: %empty
#line 878 "parser.y"
                                           {}
#line 3360 "parser.cc"
    break;

  case 162: // decl_inst: at_decl_inst
#line 880 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3366 "parser.cc"
    break;

  case 163: // decl_inst: decl
#line 881 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3372 "parser.cc"
    break;

  case 164: // decls_inst: decls_inst ";" decl_inst
#line 883 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3378 "parser.cc"
    break;

  case 165: // decls_inst: decls_inst ";"
#line 884 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3384 "parser.cc"
    break;

  case 166: // decls_inst: decl_inst
#line 885 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3390 "parser.cc"
    break;

  case 167: // decls_inst: %empty
#line 886 "parser.y"
                                           {}
#line 3396 "parser.cc"
    break;

  case 168: // decllist_inst: "{" decls_inst "}"
#line 888 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3402 "parser.cc"
    break;

  case 169: // decllist_inst: "vocurly" decls_inst close
#line 889 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3408 "parser.cc"
    break;

  case 170: // where_inst: "where" decllist_inst
#line 891 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3414 "parser.cc"
    break;

  case 171: // where_inst: %empty
#line 892 "parser.y"
                                           {}
#line 3420 "parser.cc"
    break;

  case 172: // decls: decls ";" decl
#line 895 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3426 "parser.cc"
    break;

  case 173: // decls: decls ";"
#line 896 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3432 "parser.cc"
    break;

  case 174: // decls: decl
#line 897 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3438 "parser.cc"
    break;

  case 175: // decls: %empty
#line 898 "parser.y"
                        {}
#line 3444 "parser.cc"
    break;

  case 176: // decllist: "{" decls "}"
#line 900 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3450 "parser.cc"
    break;

  case 177: // decllist: "vocurly" decls close
#line 901 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3456 "parser.cc"
    break;

  case 178: // binds: decllist
#line 903 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3462 "parser.cc"
    break;

  case 179: // wherebinds: "where" binds
#line 905 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3468 "parser.cc"
    break;

  case 180: // wherebinds: %empty
#line 906 "parser.y"
                                 {}
#line 3474 "parser.cc"
    break;

  case 186: // opt_tyconsig: %empty
#line 932 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3480 "parser.cc"
    break;

  case 187: // opt_tyconsig: "::" gtycon
#line 933 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3486 "parser.cc"
    break;

  case 188: // sigtype: ctype
#line 942 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3492 "parser.cc"
    break;

  case 189: // sigtypedoc: ctypedoc
#line 944 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3498 "parser.cc"
    break;

  case 190: // sig_vars: sig_vars "," var
#line 946 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3504 "parser.cc"
    break;

  case 191: // sig_vars: var
#line 947 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3510 "parser.cc"
    break;

  case 192: // sigtypes1: sigtype
#line 949 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3516 "parser.cc"
    break;

  case 193: // sigtypes1: sigtypes1 "," sigtype
#line 950 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3522 "parser.cc"
    break;

  case 194: // ktype: ctype
#line 959 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3528 "parser.cc"
    break;

  case 195: // ktype: ctype "::" kind
#line 960 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3534 "parser.cc"
    break;

  case 196: // ctype: "forall" tv_bndrs "." ctype
#line 962 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3540 "parser.cc"
    break;

  case 197: // ctype: context "=>" ctype
#line 963 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3546 "parser.cc"
    break;

  case 198: // ctype: type
#line 965 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3552 "parser.cc"
    break;

  case 199: // ctypedoc: ctype
#line 967 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3558 "parser.cc"
    break;

  case 200: // context: btype
#line 976 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3564 "parser.cc"
    break;

  case 201: // context_no_ops: btype_no_ops
#line 978 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3570 "parser.cc"
    break;

  case 202: // type: btype
#line 980 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3576 "parser.cc"
    break;

  case 203: // type: btype "->" ctype
#line 981 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3582 "parser.cc"
    break;

  case 204: // typedoc: type
#line 983 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3588 "parser.cc"
    break;

  case 205: // btype: infixtype
#line 986 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3594 "parser.cc"
    break;

  case 206: // infixtype: ftype
#line 988 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3600 "parser.cc"
    break;

  case 207: // infixtype: btype tyop btype
#line 989 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3606 "parser.cc"
    break;

  case 208: // btype_no_ops: atype_docs
#line 991 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3612 "parser.cc"
    break;

  case 209: // btype_no_ops: btype_no_ops atype_docs
#line 992 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3618 "parser.cc"
    break;

  case 210: // ftype: atype
#line 994 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3624 "parser.cc"
    break;

  case 211: // ftype: ftype tyarg
#line 996 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3630 "parser.cc"
    break;

  case 212: // ftype: ftype "@" atype
#line 997 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3636 "parser.cc"
    break;

  case 213: // tyarg: atype
#line 999 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3642 "parser.cc"
    break;

  case 214: // tyop: qtyconop
#line 1001 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3648 "parser.cc"
    break;

  case 215: // tyop: tyvarop
#line 1002 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3654 "parser.cc"
    break;

  case 216: // atype_docs: atype
#line 1009 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3660 "parser.cc"
    break;

  case 217: // atype: ntgtycon
#line 1016 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3666 "parser.cc"
    break;

  case 218: // atype: tyvar
#line 1017 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3672 "parser.cc"
    break;

  case 219: // atype: "*"
#line 1018 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3678 "parser.cc"
    break;

  case 220: // atype: PREFIX_BANG atype
#line 1019 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3684 "parser.cc"
    break;

  case 221: // atype: PREFIX_TILDE atype
#line 1020 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3690 "parser.cc"
    break;

  case 222: // atype: "{" fielddecls "}"
#line 1021 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3696 "parser.cc"
    break;

  case 223: // atype: "(" ")"
#line 1022 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3702 "parser.cc"
    break;

  case 224: // atype: "(" comma_types1 "," ktype ")"
#line 1023 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3708 "parser.cc"
    break;

  case 225: // atype: "[" ktype "]"
#line 1029 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3714 "parser.cc"
    break;

  case 226: // atype: "(" ktype ")"
#line 1030 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3720 "parser.cc"
    break;

  case 227: // inst_type: sigtype
#line 1033 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3726 "parser.cc"
    break;

  case 230: // comma_types0: comma_types1
#line 1038 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3732 "parser.cc"
    break;

  case 231: // comma_types0: %empty
#line 1039 "parser.y"
                                       { /* default construction OK */ }
#line 3738 "parser.cc"
    break;

  case 232: // comma_types1: ktype
#line 1041 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3744 "parser.cc"
    break;

  case 233: // comma_types1: comma_types1 "," ktype
#line 1042 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3750 "parser.cc"
    break;

  case 234: // tv_bndrs: tv_bndrs tv_bndr
#line 1049 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3756 "parser.cc"
    break;

  case 235: // tv_bndrs: %empty
#line 1050 "parser.y"
                               { /* default construction OK */}
#line 3762 "parser.cc"
    break;

  case 236: // tv_bndr: tv_bndr_no_braces
#line 1052 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3768 "parser.cc"
    break;

  case 237: // tv_bndr: "{" tyvar "}"
#line 1053 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3774 "parser.cc"
    break;

  case 238: // tv_bndr: "{" tyvar "::" kind "}"
#line 1054 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3780 "parser.cc"
    break;

  case 239: // tv_bndr_no_braces: tyvar
#line 1057 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3786 "parser.cc"
    break;

  case 240: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1058 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 3792 "parser.cc"
    break;

  case 241: // kind: ctype
#line 1076 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3798 "parser.cc"
    break;

  case 242: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1082 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3804 "parser.cc"
    break;

  case 243: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1083 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3810 "parser.cc"
    break;

  case 244: // gadt_constrlist: %empty
#line 1084 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 3816 "parser.cc"
    break;

  case 245: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1086 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3822 "parser.cc"
    break;

  case 246: // gadt_constrs: gadt_constr
#line 1087 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3828 "parser.cc"
    break;

  case 247: // gadt_constr: optSemi con_list "::" sigtype
#line 1089 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3834 "parser.cc"
    break;

  case 248: // constrs: "=" constrs1
#line 1091 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3840 "parser.cc"
    break;

  case 249: // constrs1: constrs1 "|" constr
#line 1093 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3846 "parser.cc"
    break;

  case 250: // constrs1: constr
#line 1094 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3852 "parser.cc"
    break;

  case 251: // constr: forall context_no_ops "=>" constr_stuff
#line 1096 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3858 "parser.cc"
    break;

  case 252: // constr: forall constr_stuff
#line 1097 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3864 "parser.cc"
    break;

  case 253: // forall: "forall" tv_bndrs "."
#line 1099 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3870 "parser.cc"
    break;

  case 254: // forall: %empty
#line 1100 "parser.y"
                                {}
#line 3876 "parser.cc"
    break;

  case 255: // constr_stuff: btype_no_ops
#line 1102 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3882 "parser.cc"
    break;

  case 256: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1103 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3892 "parser.cc"
    break;

  case 257: // fielddecls: %empty
#line 1109 "parser.y"
                                {}
#line 3898 "parser.cc"
    break;

  case 258: // fielddecls: fielddecls1
#line 1110 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3904 "parser.cc"
    break;

  case 259: // fielddecls1: fielddecls1 "," fielddecl
#line 1112 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3910 "parser.cc"
    break;

  case 260: // fielddecls1: fielddecl
#line 1113 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3916 "parser.cc"
    break;

  case 261: // fielddecl: sig_vars "::" ctype
#line 1115 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3922 "parser.cc"
    break;

  case 272: // decl_no_th: sigdecl
#line 1134 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3928 "parser.cc"
    break;

  case 273: // decl_no_th: infixexp rhs
#line 1136 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3934 "parser.cc"
    break;

  case 274: // decl: decl_no_th
#line 1138 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3940 "parser.cc"
    break;

  case 275: // rhs: "=" exp wherebinds
#line 1142 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3946 "parser.cc"
    break;

  case 276: // rhs: gdrhs wherebinds
#line 1143 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3952 "parser.cc"
    break;

  case 277: // gdrhs: gdrhs gdrh
#line 1145 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3958 "parser.cc"
    break;

  case 278: // gdrhs: gdrh
#line 1146 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3964 "parser.cc"
    break;

  case 279: // gdrh: "|" guardquals "=" exp
#line 1150 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3970 "parser.cc"
    break;

  case 280: // sigdecl: sig_vars "::" sigtypedoc
#line 1160 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3976 "parser.cc"
    break;

  case 281: // sigdecl: infix prec ops
#line 1161 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3982 "parser.cc"
    break;

  case 282: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1163 "parser.y"
                                                    {}
#line 3988 "parser.cc"
    break;

  case 283: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1164 "parser.y"
                                            {}
#line 3994 "parser.cc"
    break;

  case 284: // sigdecl: "{-# SCC" qvar "#-}"
#line 1165 "parser.y"
                              {}
#line 4000 "parser.cc"
    break;

  case 285: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1166 "parser.y"
                                     {}
#line 4006 "parser.cc"
    break;

  case 286: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1167 "parser.y"
                                                               {}
#line 4012 "parser.cc"
    break;

  case 287: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1168 "parser.y"
                                                                      {}
#line 4018 "parser.cc"
    break;

  case 288: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1169 "parser.y"
                                                     {}
#line 4024 "parser.cc"
    break;

  case 293: // exp: infixexp "::" sigtype
#line 1181 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4030 "parser.cc"
    break;

  case 294: // exp: infixexp
#line 1182 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4036 "parser.cc"
    break;

  case 295: // infixexp: exp10
#line 1186 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 4042 "parser.cc"
    break;

  case 296: // infixexp: infixexp qop exp10
#line 1187 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4048 "parser.cc"
    break;

  case 297: // exp10: PREFIX_MINUS fexp
#line 1189 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 4054 "parser.cc"
    break;

  case 298: // exp10: fexp
#line 1190 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4060 "parser.cc"
    break;

  case 301: // fexp: fexp aexp
#line 1198 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4066 "parser.cc"
    break;

  case 302: // fexp: fexp "@" atype
#line 1199 "parser.y"
                                 {}
#line 4072 "parser.cc"
    break;

  case 303: // fexp: "static" aexp
#line 1200 "parser.y"
                                 {}
#line 4078 "parser.cc"
    break;

  case 304: // fexp: aexp
#line 1201 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4084 "parser.cc"
    break;

  case 305: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1204 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4090 "parser.cc"
    break;

  case 306: // aexp: PREFIX_TILDE aexp
#line 1205 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4096 "parser.cc"
    break;

  case 307: // aexp: PREFIX_BANG aexp
#line 1206 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4102 "parser.cc"
    break;

  case 308: // aexp: "\\" apats1 "->" exp
#line 1207 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4108 "parser.cc"
    break;

  case 309: // aexp: "let" binds "in" exp
#line 1208 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4114 "parser.cc"
    break;

  case 310: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1210 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 4120 "parser.cc"
    break;

  case 311: // aexp: "case" exp "of" altslist
#line 1212 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 4126 "parser.cc"
    break;

  case 312: // aexp: "do" stmtlist
#line 1213 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4132 "parser.cc"
    break;

  case 313: // aexp: "mdo" stmtlist
#line 1214 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4138 "parser.cc"
    break;

  case 314: // aexp: aexp1
#line 1216 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4144 "parser.cc"
    break;

  case 315: // aexp1: aexp1 "{" fbinds "}"
#line 1219 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 4150 "parser.cc"
    break;

  case 316: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1220 "parser.y"
                                     { }
#line 4156 "parser.cc"
    break;

  case 317: // aexp1: aexp2
#line 1221 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 4162 "parser.cc"
    break;

  case 318: // aexp2: qvar
#line 1224 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4168 "parser.cc"
    break;

  case 319: // aexp2: qcon
#line 1225 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4174 "parser.cc"
    break;

  case 320: // aexp2: literal
#line 1226 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 4180 "parser.cc"
    break;

  case 321: // aexp2: "(" texp ")"
#line 1227 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 4186 "parser.cc"
    break;

  case 322: // aexp2: "(" tup_exprs ")"
#line 1228 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 4192 "parser.cc"
    break;

  case 323: // aexp2: "(" projection ")"
#line 1229 "parser.y"
                              {}
#line 4198 "parser.cc"
    break;

  case 324: // aexp2: "[" list "]"
#line 1234 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 4204 "parser.cc"
    break;

  case 325: // aexp2: "_"
#line 1235 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 4210 "parser.cc"
    break;

  case 328: // texp: exp
#line 1244 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4216 "parser.cc"
    break;

  case 329: // texp: infixexp qop
#line 1245 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 4222 "parser.cc"
    break;

  case 330: // texp: qopm infixexp
#line 1246 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4228 "parser.cc"
    break;

  case 331: // tup_exprs: tup_exprs "," texp
#line 1251 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4234 "parser.cc"
    break;

  case 332: // tup_exprs: texp "," texp
#line 1252 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4240 "parser.cc"
    break;

  case 333: // list: texp
#line 1270 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 4246 "parser.cc"
    break;

  case 334: // list: lexps
#line 1271 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4252 "parser.cc"
    break;

  case 335: // list: texp ".."
#line 1272 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4258 "parser.cc"
    break;

  case 336: // list: texp "," exp ".."
#line 1273 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 4264 "parser.cc"
    break;

  case 337: // list: texp ".." exp
#line 1274 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4270 "parser.cc"
    break;

  case 338: // list: texp "," exp ".." exp
#line 1275 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4276 "parser.cc"
    break;

  case 339: // list: texp "|" squals
#line 1276 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4282 "parser.cc"
    break;

  case 340: // lexps: lexps "," texp
#line 1278 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4288 "parser.cc"
    break;

  case 341: // lexps: texp "," texp
#line 1279 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4294 "parser.cc"
    break;

  case 342: // squals: squals "," qual
#line 1292 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4300 "parser.cc"
    break;

  case 343: // squals: qual
#line 1294 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4306 "parser.cc"
    break;

  case 344: // guardquals: guardquals1
#line 1304 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4312 "parser.cc"
    break;

  case 345: // guardquals1: guardquals1 "," qual
#line 1306 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4318 "parser.cc"
    break;

  case 346: // guardquals1: qual
#line 1307 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4324 "parser.cc"
    break;

  case 347: // altslist: "{" alts "}"
#line 1310 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4330 "parser.cc"
    break;

  case 348: // altslist: "vocurly" alts close
#line 1311 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4336 "parser.cc"
    break;

  case 349: // altslist: "{" "}"
#line 1312 "parser.y"
                                 {}
#line 4342 "parser.cc"
    break;

  case 350: // altslist: "vocurly" close
#line 1313 "parser.y"
                                 {}
#line 4348 "parser.cc"
    break;

  case 351: // alts: alts1
#line 1315 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4354 "parser.cc"
    break;

  case 352: // alts: ";" alts
#line 1316 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4360 "parser.cc"
    break;

  case 353: // alts1: alts1 ";" alt
#line 1318 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4366 "parser.cc"
    break;

  case 354: // alts1: alts1 ";"
#line 1319 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4372 "parser.cc"
    break;

  case 355: // alts1: alt
#line 1320 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4378 "parser.cc"
    break;

  case 356: // alt: pat alt_rhs
#line 1322 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4384 "parser.cc"
    break;

  case 357: // alt_rhs: "->" exp wherebinds
#line 1324 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4390 "parser.cc"
    break;

  case 358: // alt_rhs: gdpats wherebinds
#line 1325 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4396 "parser.cc"
    break;

  case 359: // gdpats: gdpats gdpat
#line 1327 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4402 "parser.cc"
    break;

  case 360: // gdpats: gdpat
#line 1328 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4408 "parser.cc"
    break;

  case 361: // gdpat: "|" guardquals "->" exp
#line 1337 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4414 "parser.cc"
    break;

  case 362: // pat: exp
#line 1339 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4420 "parser.cc"
    break;

  case 363: // bindpat: exp
#line 1341 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4426 "parser.cc"
    break;

  case 364: // apat: aexp
#line 1343 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4432 "parser.cc"
    break;

  case 365: // apats1: apats1 apat
#line 1345 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4438 "parser.cc"
    break;

  case 366: // apats1: apat
#line 1346 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4444 "parser.cc"
    break;

  case 367: // stmtlist: "{" stmts "}"
#line 1349 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4450 "parser.cc"
    break;

  case 368: // stmtlist: "vocurly" stmts close
#line 1350 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4456 "parser.cc"
    break;

  case 369: // stmts: stmts ";" stmt
#line 1352 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4462 "parser.cc"
    break;

  case 370: // stmts: stmts ";"
#line 1353 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4468 "parser.cc"
    break;

  case 371: // stmts: stmt
#line 1354 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4474 "parser.cc"
    break;

  case 372: // stmts: %empty
#line 1355 "parser.y"
                       {}
#line 4480 "parser.cc"
    break;

  case 373: // stmt: qual
#line 1360 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4486 "parser.cc"
    break;

  case 374: // stmt: "rec" stmtlist
#line 1361 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4492 "parser.cc"
    break;

  case 375: // qual: bindpat "<-" exp
#line 1363 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4498 "parser.cc"
    break;

  case 376: // qual: exp
#line 1364 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4504 "parser.cc"
    break;

  case 377: // qual: "let" binds
#line 1365 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4510 "parser.cc"
    break;

  case 378: // fbinds: fbinds1
#line 1370 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4516 "parser.cc"
    break;

  case 379: // fbinds: %empty
#line 1371 "parser.y"
                        {}
#line 4522 "parser.cc"
    break;

  case 380: // fbinds1: fbind "," fbinds1
#line 1373 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4528 "parser.cc"
    break;

  case 381: // fbinds1: fbind
#line 1374 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4534 "parser.cc"
    break;

  case 382: // fbinds1: ".."
#line 1375 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4540 "parser.cc"
    break;

  case 383: // fbind: qvar "=" texp
#line 1377 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4546 "parser.cc"
    break;

  case 384: // fbind: qvar
#line 1378 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4552 "parser.cc"
    break;

  case 385: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1379 "parser.y"
                                                      {}
#line 4558 "parser.cc"
    break;

  case 386: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1380 "parser.y"
                                                      {}
#line 4564 "parser.cc"
    break;

  case 389: // qcon: gen_qcon
#line 1416 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4570 "parser.cc"
    break;

  case 390: // qcon: sysdcon
#line 1417 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4576 "parser.cc"
    break;

  case 391: // gen_qcon: qconid
#line 1419 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4582 "parser.cc"
    break;

  case 392: // gen_qcon: "(" qconsym ")"
#line 1420 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4588 "parser.cc"
    break;

  case 393: // con: conid
#line 1422 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4594 "parser.cc"
    break;

  case 394: // con: "(" consym ")"
#line 1423 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4600 "parser.cc"
    break;

  case 395: // con: sysdcon
#line 1424 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4606 "parser.cc"
    break;

  case 396: // con_list: con_list "," con
#line 1426 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4612 "parser.cc"
    break;

  case 397: // con_list: con
#line 1427 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4618 "parser.cc"
    break;

  case 398: // sysdcon_no_list: "(" ")"
#line 1429 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4624 "parser.cc"
    break;

  case 399: // sysdcon_no_list: "(" commas ")"
#line 1430 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4630 "parser.cc"
    break;

  case 400: // sysdcon_no_list: "(#" "#)"
#line 1431 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4636 "parser.cc"
    break;

  case 401: // sysdcon_no_list: "(#" commas "#)"
#line 1432 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4642 "parser.cc"
    break;

  case 402: // sysdcon: sysdcon_no_list
#line 1434 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4648 "parser.cc"
    break;

  case 403: // sysdcon: "[" "]"
#line 1435 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4654 "parser.cc"
    break;

  case 404: // conop: consym
#line 1437 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4660 "parser.cc"
    break;

  case 405: // conop: "`" conid "`"
#line 1438 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4666 "parser.cc"
    break;

  case 406: // qconop: qconsym
#line 1440 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4672 "parser.cc"
    break;

  case 407: // qconop: "`" qconid "`"
#line 1441 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4678 "parser.cc"
    break;

  case 408: // gtycon: ntgtycon
#line 1444 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4684 "parser.cc"
    break;

  case 409: // gtycon: "(" ")"
#line 1445 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4690 "parser.cc"
    break;

  case 410: // gtycon: "(#" "#)"
#line 1446 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4696 "parser.cc"
    break;

  case 411: // ntgtycon: oqtycon
#line 1448 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4702 "parser.cc"
    break;

  case 412: // ntgtycon: "(" commas ")"
#line 1449 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4708 "parser.cc"
    break;

  case 413: // ntgtycon: "(#" commas "#)"
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4714 "parser.cc"
    break;

  case 414: // ntgtycon: "(" "->" ")"
#line 1451 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4720 "parser.cc"
    break;

  case 415: // ntgtycon: "[" "]"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4726 "parser.cc"
    break;

  case 416: // oqtycon: qtycon
#line 1454 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4732 "parser.cc"
    break;

  case 417: // oqtycon: "(" qtyconsym ")"
#line 1455 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4738 "parser.cc"
    break;

  case 418: // oqtycon_no_varcon: qtycon
#line 1457 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4744 "parser.cc"
    break;

  case 419: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1458 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4750 "parser.cc"
    break;

  case 420: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1459 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4756 "parser.cc"
    break;

  case 421: // oqtycon_no_varcon: "(" ":" ")"
#line 1460 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4762 "parser.cc"
    break;

  case 422: // qtyconop: qtyconsym
#line 1463 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4768 "parser.cc"
    break;

  case 423: // qtyconop: "`" qtycon "`"
#line 1464 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4774 "parser.cc"
    break;

  case 424: // qtycondoc: qtycon
#line 1466 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4780 "parser.cc"
    break;

  case 425: // qtycon: "QCONID"
#line 1468 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4786 "parser.cc"
    break;

  case 426: // qtycon: tycon
#line 1469 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4792 "parser.cc"
    break;

  case 427: // tycon: "CONID"
#line 1473 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4798 "parser.cc"
    break;

  case 428: // qtyconsym: "QCONSYM"
#line 1475 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4804 "parser.cc"
    break;

  case 429: // qtyconsym: "QVARSYM"
#line 1476 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4810 "parser.cc"
    break;

  case 430: // qtyconsym: tyconsym
#line 1477 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4816 "parser.cc"
    break;

  case 431: // tyconsym: "CONSYM"
#line 1479 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4822 "parser.cc"
    break;

  case 432: // tyconsym: "VARSYM"
#line 1480 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4828 "parser.cc"
    break;

  case 433: // tyconsym: ":"
#line 1481 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4834 "parser.cc"
    break;

  case 434: // tyconsym: "-"
#line 1482 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4840 "parser.cc"
    break;

  case 435: // op: varop
#line 1487 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4846 "parser.cc"
    break;

  case 436: // op: conop
#line 1488 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4852 "parser.cc"
    break;

  case 437: // varop: varsym
#line 1490 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4858 "parser.cc"
    break;

  case 438: // varop: "`" varid "`"
#line 1491 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4864 "parser.cc"
    break;

  case 439: // qop: qvarop
#line 1493 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4870 "parser.cc"
    break;

  case 440: // qop: qconop
#line 1494 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4876 "parser.cc"
    break;

  case 441: // qopm: qvaropm
#line 1497 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4882 "parser.cc"
    break;

  case 442: // qopm: qconop
#line 1498 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4888 "parser.cc"
    break;

  case 443: // qvarop: qvarsym
#line 1503 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4894 "parser.cc"
    break;

  case 444: // qvarop: "`" qvarid "`"
#line 1504 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4900 "parser.cc"
    break;

  case 445: // qvaropm: qvarsym_no_minus
#line 1506 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4906 "parser.cc"
    break;

  case 446: // qvaropm: "`" qvarid "`"
#line 1507 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4912 "parser.cc"
    break;

  case 447: // tyvar: tyvarid
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4918 "parser.cc"
    break;

  case 448: // tyvarop: "`" tyvarid "`"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4924 "parser.cc"
    break;

  case 449: // tyvarid: "VARID"
#line 1515 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4930 "parser.cc"
    break;

  case 450: // tyvarid: special_id
#line 1516 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4936 "parser.cc"
    break;

  case 451: // tyvarid: "unsafe"
#line 1517 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4942 "parser.cc"
    break;

  case 452: // tyvarid: "safe"
#line 1518 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4948 "parser.cc"
    break;

  case 453: // tyvarid: "interruptible"
#line 1519 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4954 "parser.cc"
    break;

  case 454: // var: varid
#line 1522 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4960 "parser.cc"
    break;

  case 455: // var: "(" varsym ")"
#line 1523 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4966 "parser.cc"
    break;

  case 456: // qvar: qvarid
#line 1525 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4972 "parser.cc"
    break;

  case 457: // qvar: "(" varsym ")"
#line 1526 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4978 "parser.cc"
    break;

  case 458: // qvar: "(" qvarsym1 ")"
#line 1527 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4984 "parser.cc"
    break;

  case 459: // field: varid
#line 1529 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4990 "parser.cc"
    break;

  case 460: // qvarid: varid
#line 1531 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4996 "parser.cc"
    break;

  case 461: // qvarid: "QVARID"
#line 1532 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5002 "parser.cc"
    break;

  case 462: // varid: "VARID"
#line 1534 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5008 "parser.cc"
    break;

  case 463: // varid: special_id
#line 1535 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5014 "parser.cc"
    break;

  case 464: // varid: "unsafe"
#line 1536 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5020 "parser.cc"
    break;

  case 465: // varid: "safe"
#line 1537 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5026 "parser.cc"
    break;

  case 466: // varid: "interruptible"
#line 1538 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5032 "parser.cc"
    break;

  case 467: // varid: "forall"
#line 1539 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5038 "parser.cc"
    break;

  case 468: // varid: "family"
#line 1540 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5044 "parser.cc"
    break;

  case 469: // varid: "role"
#line 1541 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5050 "parser.cc"
    break;

  case 470: // qvarsym: varsym
#line 1543 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5056 "parser.cc"
    break;

  case 471: // qvarsym: qvarsym1
#line 1544 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5062 "parser.cc"
    break;

  case 472: // qvarsym_no_minus: varsym_no_minus
#line 1546 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5068 "parser.cc"
    break;

  case 473: // qvarsym_no_minus: qvarsym1
#line 1547 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5074 "parser.cc"
    break;

  case 474: // qvarsym1: "QVARSYM"
#line 1549 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5080 "parser.cc"
    break;

  case 475: // varsym: varsym_no_minus
#line 1551 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5086 "parser.cc"
    break;

  case 476: // varsym: "-"
#line 1552 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5092 "parser.cc"
    break;

  case 477: // varsym_no_minus: "VARSYM"
#line 1554 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5098 "parser.cc"
    break;

  case 478: // varsym_no_minus: special_sym
#line 1555 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5104 "parser.cc"
    break;

  case 479: // special_id: "as"
#line 1557 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5110 "parser.cc"
    break;

  case 480: // special_id: "qualified"
#line 1558 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5116 "parser.cc"
    break;

  case 481: // special_id: "hiding"
#line 1559 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5122 "parser.cc"
    break;

  case 482: // special_id: "export"
#line 1560 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5128 "parser.cc"
    break;

  case 483: // special_id: "label"
#line 1561 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5134 "parser.cc"
    break;

  case 484: // special_id: "dynamic"
#line 1562 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5140 "parser.cc"
    break;

  case 485: // special_id: "stdcall"
#line 1563 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5146 "parser.cc"
    break;

  case 486: // special_id: "ccall"
#line 1564 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5152 "parser.cc"
    break;

  case 487: // special_id: "capi"
#line 1565 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5158 "parser.cc"
    break;

  case 488: // special_id: "prim"
#line 1566 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5164 "parser.cc"
    break;

  case 489: // special_id: "javascript"
#line 1567 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5170 "parser.cc"
    break;

  case 490: // special_id: "group"
#line 1568 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5176 "parser.cc"
    break;

  case 491: // special_id: "stock"
#line 1569 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5182 "parser.cc"
    break;

  case 492: // special_id: "anyclass"
#line 1570 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5188 "parser.cc"
    break;

  case 493: // special_id: "via"
#line 1571 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5194 "parser.cc"
    break;

  case 494: // special_id: "unit"
#line 1572 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5200 "parser.cc"
    break;

  case 495: // special_id: "dependency"
#line 1573 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5206 "parser.cc"
    break;

  case 496: // special_id: "signature"
#line 1574 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5212 "parser.cc"
    break;

  case 497: // special_sym: "."
#line 1576 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5218 "parser.cc"
    break;

  case 498: // special_sym: "*"
#line 1577 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5224 "parser.cc"
    break;

  case 499: // qconid: conid
#line 1581 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5230 "parser.cc"
    break;

  case 500: // qconid: "QCONID"
#line 1582 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5236 "parser.cc"
    break;

  case 501: // conid: "CONID"
#line 1584 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5242 "parser.cc"
    break;

  case 502: // qconsym: consym
#line 1586 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5248 "parser.cc"
    break;

  case 503: // qconsym: "QCONSYM"
#line 1587 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5254 "parser.cc"
    break;

  case 504: // consym: "CONSYM"
#line 1589 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5260 "parser.cc"
    break;

  case 505: // consym: ":"
#line 1590 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5266 "parser.cc"
    break;

  case 506: // literal: "CHAR"
#line 1594 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 5272 "parser.cc"
    break;

  case 507: // literal: "STRING"
#line 1595 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5278 "parser.cc"
    break;

  case 508: // literal: "INTEGER"
#line 1596 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5284 "parser.cc"
    break;

  case 509: // literal: "RATIONAL"
#line 1597 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5290 "parser.cc"
    break;

  case 510: // literal: "PRIMINTEGER"
#line 1598 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5296 "parser.cc"
    break;

  case 512: // close: error
#line 1606 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5302 "parser.cc"
    break;

  case 513: // modid: "CONID"
#line 1610 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5308 "parser.cc"
    break;

  case 514: // modid: "QCONID"
#line 1611 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5314 "parser.cc"
    break;

  case 515: // commas: commas ","
#line 1613 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5320 "parser.cc"
    break;

  case 516: // commas: ","
#line 1614 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5326 "parser.cc"
    break;


#line 5330 "parser.cc"

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


  const short parser::yypact_ninf_ = -672;

  const short parser::yytable_ninf_ = -476;

  const short
  parser::yypact_[] =
  {
      38,   129,  -672,    86,  -672,  -672,  -672,  -672,  -672,   201,
      -5,   -24,  -672,    70,   138,   138,     4,  -672,  -672,  -672,
    -672,   187,  -672,  -672,  -672,    40,  -672,   110,   139,  1383,
     197,   209,   176,  -672,   830,  -672,    96,  -672,  -672,  -672,
     129,  -672,   129,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,  -672,  -672,  -672,   251,  -672,  -672,  -672,  -672,
    -672,   195,   -21,  -672,   205,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,   320,  -672,   129,  -672,   191,  -672,  2436,  4483,
     297,   159,   365,  2436,  -672,  -672,  -672,   428,   383,  -672,
    3544,   334,   365,  3116,   283,  4967,   228,  3116,  3116,  2708,
    3116,  1756,  1620,   234,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,    32,   283,   274,   176,  -672,  -672,  -672,  -672,    68,
      -4,  -672,  -672,   659,  -672,  2844,  -672,   266,  -672,  -672,
    -672,  -672,  -672,  -672,   344,    39,  -672,  -672,  -672,  -672,
     304,  -672,  -672,   355,  -672,  -672,  -672,  -672,   367,  -672,
     375,   379,   414,  -672,  -672,  -672,  4588,  -672,  4625,  -672,
    -672,  -672,  -672,   460,  -672,  1620,   495,   679,  -672,  -672,
    -672,  4483,  4483,  -672,  5066,  3645,  3228,   416,  -672,   513,
     451,  -672,   685,  -672,  3832,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,  4483,   435,  -672,  2164,  2164,  -672,   430,   472,
     473,   475,   476,  3933,  1250,  1250,  -672,   540,  4483,  4483,
     100,   478,   953,   122,   515,  -672,  -672,    -6,  4967,  -672,
     311,   -22,   452,   132,  -672,   140,  -672,  -672,  -672,  -672,
    2980,  -672,  2844,  -672,  -672,  -672,  4830,  -672,  -672,  -672,
     679,   113,   456,   448,  -672,  2436,  -672,  -672,  -672,  -672,
    -672,  -672,  5189,  -672,  -672,   121,   -12,   170,   379,   459,
     461,   462,   275,  -672,   327,  3933,  4967,  4967,  -672,   796,
     191,   506,   445,  4483,  3933,  5066,  2436,  2572,  4830,  -672,
      36,  -672,  -672,  2436,  -672,  -672,  -672,  -672,  4483,  -672,
    5189,  4930,  3116,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
     467,   468,   463,  -672,   479,    70,   129,    67,   391,  3933,
    -672,  -672,   194,   141,   481,   464,  -672,  -672,  -672,  -672,
     482,   507,   499,  -672,  -672,   487,  -672,  -672,  -672,  -672,
    -672,  -672,   491,   488,   496,  -672,   303,   351,   405,  -672,
    4483,  3933,  4867,  4483,  -672,  -672,  -672,  4483,  -672,  -672,
     528,  3933,   383,   365,   526,   527,    88,  -672,  -672,    44,
    -672,   589,  -672,  -672,  -672,  -672,  -672,  -672,   590,   247,
    -672,  -672,   659,    48,  2436,  -672,   535,   292,  3933,   204,
    3933,   485,   486,   514,   547,  -672,   550,   517,   254,   228,
     555,  2436,  -672,   521,   522,  2436,  2436,  2572,  1892,  -672,
    1892,   811,  -672,  -672,  5189,  -672,  -672,  1892,  -672,  1892,
     158,  -672,  -672,  -672,  -672,   557,   564,   568,  5029,   516,
    -672,  -672,  -672,  -672,  -672,  4034,    -7,   376,  -672,  -672,
    -672,  -672,   610,   569,   530,  -672,   538,   383,  -672,  -672,
    -672,  -672,  -672,  -672,   559,  -672,   536,   584,   566,   567,
    -672,  -672,  -672,  4766,  -672,  -672,  -672,   570,  1420,  -672,
    -672,  2028,  1484,  -672,  -672,   574,  3933,  -672,  5066,  5226,
    -672,  3933,  3933,  -672,  -672,  3933,  -672,  -672,  -672,  1114,
    1114,  -672,  -672,  -672,   560,   561,   672,  -672,  3933,  -672,
    -672,   575,   576,   540,  -672,  2436,  -672,  2164,  -672,  2436,
     406,  -672,  -672,  1250,  -672,  -672,  3933,  3933,  5339,   609,
    -672,  -672,   111,  -672,  -672,  5066,   585,  -672,  -672,  -672,
    -672,   588,   400,   366,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,   579,  -672,   624,  -672,  -672,  -672,  -672,  -672,
    -672,  3933,  3933,   583,   586,   796,  -672,   376,   616,  -672,
    -672,   634,  3933,   684,   687,   707,  -672,  2436,  2572,  -672,
    -672,  -672,  4930,  1892,  5189,  -672,  1420,   129,  -672,   205,
     611,   -20,  -672,  -672,  2300,  -672,   623,   614,  -672,   331,
      70,  -672,  -672,  -672,  -672,  3933,  5425,  5425,  -672,  -672,
    -672,  -672,  -672,   620,   692,  3746,  -672,  -672,   277,  -672,
      53,  -672,  -672,  -672,  -672,  3933,  -672,  -672,   430,   972,
     972,  -672,  -672,  -672,  -672,  -672,  5425,   708,   661,  -672,
    -672,  -672,  2572,  2436,  -672,   -16,    37,  -672,  -672,  -672,
    5103,   687,   707,  4483,  -672,  -672,  -672,   655,  -672,  4483,
     426,   707,   349,  -672,   707,  -672,  -672,  -672,  -672,  -672,
      91,  -672,   631,  -672,  -672,  -672,  4729,  -672,  -672,  -672,
    2436,  2572,  2436,  -672,    62,  -672,  -672,  -672,   173,   665,
    -672,  -672,  4483,  4483,  4483,  -672,   424,  -672,  1114,  -672,
    -672,   737,  -672,   730,  -672,   730,  -672,   293,  -672,    57,
    -672,   664,   427,  -672,  3933,  -672,  -672,  -672,  3933,  -672,
    4483,  4483,   707,  -672,  -672,  5263,   684,   662,  3334,  -672,
    -672,  -672,   430,   430,  -672,  -672,  -672,  -672,  4120,   262,
     701,  -672,  -672,  -672,  1892,  5189,  -672,  -672,  -672,   670,
     610,  -672,  -672,  3933,  -672,  3933,   528,  -672,   458,  3933,
    4225,  -672,  -672,  2436,  -672,  4483,   506,  -672,   972,  -672,
    5425,  4311,  4397,  -672,  -672,  -672,  -672,   666,   672,  -672,
    -672,  -672,  4483,   637,  -672,  4483,   300,  -672,   228,    63,
    -672,  -672,   642,   653,  -672,  4483,  -672,  -672,  -672,  2436,
    -672,   663,   660,  -672,  5372,  -672,  -672,  3228,   690,   693,
    -672,  -672,  4034,  -672,  5425,  -672,   675,   305,  -672,    70,
      82,  4483,  3439,  -672,  4483,  -672,   430,   162,  -672,  4483,
    -672,  -672,  -672,  -672,  -672,   665,  5425,   376,  -672,  -672,
    -672,  4483,  -672,  -672,  -672,  -672,  3933,  -672,  -672,   687,
     707,  -672,  -672,   707,  -672,  -672
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   513,   514,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   512,   511,    12,   185,   181,     0,     0,    20,
       0,    46,    41,    15,    14,   184,     0,     6,     7,   479,
       0,   481,     0,   480,   467,   482,   483,   484,   465,   466,
     464,   468,   469,   485,   486,   487,   488,   489,   490,   491,
     492,   493,   494,   496,   495,     0,   462,   427,   461,   425,
      22,     0,    19,    24,    28,    36,   418,   426,    35,   456,
     460,   463,     0,    45,     0,    38,    42,   325,     0,     0,
     129,   131,     0,     0,    63,    64,    65,    96,     0,   130,
       0,     0,     0,     0,   289,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   501,   500,   506,   507,   508,   509,
     510,   289,   289,    61,    68,    71,    72,    73,    74,   151,
       0,    77,   272,    78,   295,   298,   304,   314,   317,   319,
     389,   402,   390,   191,   318,   460,   391,   499,   320,   182,
       0,    27,    26,     0,   476,   498,   497,   477,     0,   474,
       0,     0,     0,   475,   478,    17,     0,    21,    31,    25,
      40,    40,     3,    48,    37,     0,     0,   294,   452,   453,
     451,     0,     0,   219,   257,     0,     0,     0,   449,   161,
       0,   144,   202,   205,   206,   210,   217,   411,   416,   218,
     447,   450,     0,     0,   132,   372,   372,   312,   300,     0,
       0,     0,     0,     0,   175,   175,   178,     0,     0,     0,
       0,     0,   202,   411,     0,   313,   303,     0,     0,   290,
       0,     0,     0,     0,   397,   186,   395,   393,   364,   366,
       0,   306,   297,   307,   505,   403,     0,   504,   503,   328,
     294,   333,     0,   334,   442,     0,   441,   445,   473,   472,
     406,   502,     0,   398,   516,     0,     0,     0,   473,     0,
     472,   406,     0,   400,     0,     0,     0,     0,    62,     0,
      69,   151,     0,     0,     0,     0,     0,     0,     0,   273,
     180,   278,   440,     0,   439,   443,   471,   470,     0,   301,
       0,   379,     0,   183,   421,   420,   419,   458,   457,    23,
       0,     0,    32,    34,     0,     0,     0,    50,     0,     0,
     221,   220,     0,     0,     0,   258,   260,   454,   235,   415,
       0,   194,     0,   198,   433,     0,   434,   223,   432,   431,
     429,   428,   232,     0,     0,   430,     0,     0,     0,    79,
       0,     0,     0,     0,   214,   422,   215,     0,   211,   213,
     135,   231,     0,     0,   376,     0,     0,   371,   373,     0,
     299,     0,    93,    92,    94,    95,   227,   188,   171,     0,
     274,   174,     0,     0,     0,    89,     0,   137,     0,     0,
       0,     0,     0,     0,     0,   284,     0,     0,     0,     0,
       0,     0,   365,     0,     0,   329,   335,     0,     0,   324,
       0,   330,   327,   459,     0,   323,   321,     0,   322,     0,
     457,   392,   399,   515,   401,     0,     0,     0,     0,   281,
     436,    67,   435,   437,   404,     0,     0,   133,   280,   199,
     189,   190,   180,     0,   344,   346,     0,     0,   276,   277,
     296,   302,   316,   382,     0,   378,   381,   384,     0,   460,
     305,    30,    29,     0,     9,    10,    47,     0,    54,    44,
      49,     0,     0,   311,   293,     0,     0,   222,     0,     0,
     225,     0,     0,   414,   226,     0,   417,   412,   413,   157,
     157,   160,   143,   203,     0,     0,   207,   212,     0,    84,
     232,     0,   230,   377,   374,     0,   367,   370,   368,     0,
       0,    88,   176,   173,   177,   309,     0,     0,     0,   101,
     241,    85,     0,    86,    80,     0,     0,   291,   283,   285,
     394,     0,     0,     0,   187,   408,   396,   282,   308,   446,
     407,   337,   339,   343,   328,   341,   340,   326,   332,   331,
     288,     0,     0,     0,     0,     0,   235,   133,     0,   148,
     150,     0,     0,   254,   244,   262,   275,     0,     0,   444,
     179,   315,     0,     0,     0,    33,    54,     0,    56,    28,
       0,    53,    58,   349,     0,   362,     0,   351,   355,     0,
       0,   350,   455,   261,   259,     0,     0,     0,   234,   236,
     239,   195,   197,   233,   122,     0,   152,   156,     0,   153,
       0,   423,   448,   136,    75,     0,   375,   369,   300,   167,
     167,   170,   172,   116,   138,   139,     0,   106,     0,   292,
     409,   410,     0,   336,   192,     0,     0,   438,   405,    66,
       0,   244,   262,     0,   149,   134,   235,   248,   250,     0,
       0,   262,     0,    81,   263,   265,   279,   345,   380,   383,
     386,   388,     0,    60,    59,    51,     0,    55,   352,   347,
     354,     0,     0,   356,   180,   360,   348,   196,     0,     0,
     224,   123,     0,     0,     0,   120,   140,   158,   155,   159,
     233,     0,   129,   124,   162,   124,   166,     0,   163,     0,
     102,     0,     0,    83,     0,   342,   338,   286,     0,   287,
       0,     0,   262,    90,   147,     0,   254,     0,   255,   208,
     216,   252,   300,   300,    82,    99,    97,    98,     0,     0,
     266,   269,   424,   264,     0,     0,    52,    57,   353,     0,
     180,   358,   359,     0,   237,     0,   135,   121,   140,     0,
       0,   118,   154,     0,   125,     0,   151,   168,   165,   169,
       0,   115,   115,   107,    76,   193,   146,     0,   200,    91,
     253,   249,     0,     0,   209,     0,     0,   246,     0,     0,
     270,   204,   228,     0,   267,     0,   268,   385,   387,     0,
     357,     0,     0,   117,     0,   119,   141,     0,     0,   218,
     310,   126,     0,   164,   103,   105,     0,     0,   114,     0,
       0,     0,   255,   251,   256,   242,   300,     0,   243,     0,
     271,   100,   361,   238,   240,   218,     0,   133,   104,   110,
     108,   113,   111,   109,   145,   245,     0,   229,   142,   244,
     262,   112,   247,   262,   127,   128
  };

  const short
  parser::yypgoto_[] =
  {
    -672,  -672,  -672,  -672,  -672,  -672,  -672,    43,  -672,  -672,
    -672,  -672,   615,   206,  -672,  -672,    -9,   671,  -672,  -672,
    -672,  -672,  -672,  -672,  -672,  -672,   210,  -672,   123,  -672,
    -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,  -672,
    -672,  -672,  -672,  -672,   -36,  -672,  -672,  -672,    49,  -193,
    -672,  -672,    99,  -672,   772,  -672,  -536,    66,  -672,    69,
     532,    14,  -273,   130,   330,  -672,  -672,    64,   203,  -672,
    -672,   606,  -672,  -309,  -399,   809,  -672,  -672,  -302,   124,
    -172,   273,  -162,    94,  -672,   -71,  -672,   -85,  -672,     8,
    -672,  -324,  -672,  -672,  -672,  -671,  -113,   551,    10,  -672,
     469,  -468,   313,  -659,  -386,  -602,   109,    25,  -524,  -672,
     135,  -672,    73,  -672,  -672,   377,  -585,  -672,   199,   128,
     837,  -187,  -672,  -672,   598,  -672,   163,  -672,   -69,    33,
    -251,  -199,   767,   -73,  -672,  -672,  -672,  -106,  -672,  -672,
    -672,  -672,   207,  -672,  -672,  -416,  -672,   224,  -672,  -672,
     221,  -672,  -672,   639,  -672,   -70,   691,   389,  -246,  -672,
     326,  -672,  -672,  -672,  -672,   501,   125,  -672,   -99,  -638,
     -68,  -672,   503,   -60,  -672,  -672,  -672,   -19,  -672,  -176,
    -672,   347,  -672,   658,  -672,  -672,  -672,  -119,  -672,  -330,
    -249,    -3,  -218,  -156,   -31,  -672,  -672,   -27,    13,   -98,
     -89,  -672,  -136,  -104,   -52,  -227,  -672,  -265,   -11,  -112
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   172,     6,    10,    19,    30,
      71,    72,    73,   169,   311,   312,   579,    86,    11,    20,
      21,    32,    84,   317,   469,   470,   580,   581,   582,   279,
     123,   429,    33,    34,   124,   125,   126,   127,   220,   128,
     213,   729,   786,   627,   700,   804,   703,   763,   807,   808,
     606,   682,   755,   694,   695,   203,   564,   499,   519,   751,
     189,   557,   283,   607,   608,   491,   349,   696,   697,   621,
     511,   379,   216,   217,   448,    27,    36,   400,   376,   438,
     130,   635,   342,   520,   440,   332,   717,   333,   782,   192,
     193,   718,   194,   358,   353,   719,   195,   378,   783,   501,
     343,   479,   598,   599,   521,   651,   776,   777,   565,   647,
     648,   649,   721,   324,   325,   326,   653,   654,   655,   730,
     380,   609,   289,   290,   291,   132,   228,   229,   249,   177,
     134,   778,   135,   136,   137,   138,   265,   266,   267,   252,
     253,   542,   443,   444,   473,   586,   587,   588,   673,   674,
     675,   589,   365,   239,   240,   207,   366,   367,   368,   454,
     455,   456,   660,   139,   140,   234,   235,   141,   142,   430,
     254,   534,   196,   197,    75,   354,   731,   198,    77,   344,
     345,   431,   432,   293,   255,   294,   256,   199,   356,   200,
     143,   144,   458,    79,    80,   295,   257,   258,   297,   163,
      81,   164,   146,   147,   260,   261,   148,    24,     9,   272
  };

  const short
  parser::yytable_[] =
  {
     201,   274,   237,   145,   191,   251,   397,   236,   435,   371,
      76,   201,   323,   259,   270,   221,   355,   474,   190,   176,
      74,   641,   495,   330,   208,   385,    78,   381,   381,   151,
     226,   152,   225,   642,   238,   241,   441,   243,   161,   712,
     223,   445,   450,   566,   412,    22,   355,   774,    13,    22,
     465,   275,   434,   503,    22,   395,   590,   713,    22,     1,
     271,   707,   299,   447,    22,   292,   724,   133,   320,   321,
     560,    22,   204,   173,   346,   347,   284,   270,   162,   467,
     775,   359,   452,    22,   392,   268,    12,   281,   640,   447,
     403,   798,   201,   201,    18,   601,   201,   201,   166,   666,
      17,   416,   231,   708,   508,   201,   296,   417,   222,   292,
     404,   396,   613,   201,   709,   285,    29,   360,   514,  -454,
     287,   167,   667,   271,   201,   269,   561,   769,   393,   201,
     201,   624,   446,   386,   387,   798,   364,   364,   570,   282,
     227,   774,     2,   774,   250,   250,   671,    76,   268,    76,
     296,    23,   404,   327,   450,    23,   708,    74,  -454,   313,
      23,   543,   507,    78,    23,    78,   513,   238,   668,   299,
      23,   688,   734,    35,   775,   758,   645,    23,   715,   468,
     388,   816,   292,   145,   145,   451,   201,    37,   162,    23,
     334,   406,   735,   506,   201,   201,   547,   407,   191,   500,
     831,    31,   -87,   161,   336,   149,   507,   591,   250,   201,
     222,   244,   190,   314,   315,   150,    38,   442,   364,   389,
     398,   476,   414,   296,    82,   394,   222,   222,   524,   460,
     201,   413,   408,    83,   415,   338,   339,   843,  -455,   340,
     341,   -87,   836,   162,   497,   263,    25,   382,   382,   634,
     634,   264,     7,   743,   327,   844,     8,   247,   845,   399,
     285,   201,   201,   201,   201,   492,    14,    15,   201,   413,
     459,    26,   201,   426,   427,   741,   628,  -455,   744,   331,
     331,   399,    67,   418,   276,   277,    69,   154,   411,   419,
     155,   839,   433,   504,    85,   237,   701,   156,   457,   201,
     236,   201,   545,   840,   546,   466,   323,   377,   165,   174,
     259,   548,   259,   549,   292,   515,   522,   168,   157,   259,
     355,   259,   657,   603,   554,   676,   622,    67,   434,   523,
     153,    69,   538,   494,   202,   475,   232,   541,   364,   544,
     233,   790,   113,   292,   154,   689,   201,   155,   224,   273,
     559,   114,   512,   264,   156,   296,   661,   791,   222,   792,
     600,   496,   531,   796,   558,   513,   532,   300,   533,   377,
     301,   725,   517,   518,   728,   157,   158,    67,   439,   159,
     160,    69,   687,   413,   296,    67,   705,   201,   422,    69,
     201,   227,   201,   201,   423,   688,   201,   553,   757,   600,
     726,   727,   585,   585,   154,   815,   765,   155,   278,   201,
     830,   758,   685,   377,   156,   671,   487,   672,   816,   691,
     346,   347,   423,   831,   170,   445,   171,   201,   201,   201,
     805,   302,   698,   698,   759,   157,   616,   303,   364,   159,
     618,   250,   424,   250,    76,   493,   423,   327,   812,    76,
     250,   814,   250,   690,   575,   331,   562,   563,   145,   145,
      78,   728,   201,   201,   316,    78,   488,   659,   304,   205,
     423,   206,    67,   201,   828,   259,    69,   678,   679,   334,
     305,   631,   145,   802,   331,   264,   335,   214,   306,   215,
     747,   434,   307,   336,   327,   471,   701,   472,   656,   364,
     209,   210,   211,   212,   749,   750,   201,   201,   201,   489,
     619,   490,   620,   630,   818,   585,   201,   788,   318,   264,
     686,   600,   382,   382,   338,   339,   201,   308,   340,   341,
     722,   761,   723,   762,   842,   264,   720,   201,   749,   794,
     348,   459,   350,   413,   832,   833,   382,   361,   370,   372,
     373,   201,   374,   375,   201,   384,   391,    76,   714,   390,
     201,   245,   801,   364,   706,   409,   663,   410,   433,   457,
     593,   698,   420,    78,  -475,   421,   602,   282,   436,   331,
     461,   462,   463,   478,   464,   434,   477,   481,   145,   145,
     482,   480,   355,   201,   201,   201,   600,   746,   386,   748,
     483,   585,   364,   740,   484,   720,   250,   485,   498,   486,
     623,  -363,   505,   222,   509,   201,   516,   510,   525,   201,
     526,   201,   201,   527,   528,   766,   201,   529,   787,   201,
     530,   799,   537,   732,   550,   555,   259,   447,   841,   201,
     767,   539,   540,   781,   551,   377,   377,    76,   552,   568,
     567,   222,   382,   382,   201,   572,   201,   145,   569,   720,
     201,   201,   720,    78,   571,   573,   201,   574,  -459,   554,
     386,   201,   201,   201,   237,   600,   386,   386,   825,   236,
     611,   612,   576,   201,   800,   346,   201,   592,   614,   677,
     222,   222,   222,   626,   629,   615,   201,   329,   632,   720,
     821,   720,   633,   637,   413,   201,   638,   643,   201,   331,
     732,   644,   646,   201,   650,   201,   652,   559,   222,   768,
     822,   382,   201,   201,   665,   201,   834,   145,   669,   681,
     201,   558,   670,   680,   781,   702,   222,   201,   244,   716,
     286,   704,   201,   287,   736,   745,   386,   201,   753,   754,
     760,   334,   154,   772,   785,   155,   789,   811,   244,   319,
     114,   819,   156,   222,   334,   336,   820,   250,   823,   222,
     222,   351,   154,   824,   826,   155,  -200,  -239,   336,   288,
     829,   309,   156,   157,   247,   664,   662,   159,   248,   737,
     838,   382,   352,   222,   756,   280,   338,   339,   439,   288,
     340,   341,   377,   157,   247,   352,   129,   159,   248,   338,
     339,   810,   793,   340,   341,   437,   827,   795,   752,   222,
     610,   383,   803,   699,    28,   636,   425,   222,   764,   837,
     502,   625,   779,    87,    39,    88,    89,    90,    91,   222,
      92,   835,    41,    93,   623,   813,    94,    95,    96,    97,
      98,   771,    99,   733,    43,   594,   100,   784,    44,   101,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,   131,    55,    56,    57,   244,   242,    58,   739,   402,
     103,    59,    60,    61,    62,    63,    64,   104,   449,   154,
     244,   331,   155,   105,   738,   742,   617,   369,   658,   156,
     536,   535,   639,   817,   154,     0,   106,   155,   405,     0,
       0,     0,   107,     0,   156,     0,   428,     0,     0,   108,
     157,   247,   109,     0,   110,     0,     0,     0,     0,     0,
     377,   288,     0,     0,     0,   157,   247,     0,   111,   159,
     248,     0,   112,     0,   113,     0,     0,     0,     0,     0,
       0,     0,    66,   114,     0,     0,    68,   115,     0,     0,
       0,     0,   116,   117,   118,   119,     0,     0,   120,     0,
       0,     0,     0,   121,   122,    87,    39,    88,     0,   692,
       0,     0,    92,     0,    41,    93,     0,     0,    94,    95,
      96,     0,    98,     0,    99,     0,    43,     0,   693,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,   104,
       0,     0,   334,     0,     0,   105,     0,     0,     0,   351,
       0,     0,     0,     0,     0,     0,   336,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,     0,     0,
       0,     0,     0,   352,     0,     0,     0,   338,   339,     0,
     111,   340,   341,     0,   112,     0,   113,     0,     0,     0,
       0,     0,     0,     0,    66,   114,     0,     0,    68,   115,
       0,     0,     0,     0,   116,   117,   118,   119,     0,     0,
     120,     0,     0,     0,     0,   121,   122,    87,    39,    88,
       0,   604,     0,     0,    92,     0,    41,    93,     0,     0,
      94,    95,    96,     0,    98,     0,     0,     0,    43,     0,
     605,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,   104,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   112,     0,   113,     0,
       0,     0,     0,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,   121,   122,     0,
      92,     0,    41,    93,     0,     0,    94,    95,    96,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
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
       0,     0,   116,   117,   118,   119,     0,    39,   120,     0,
       0,    40,     0,   121,   122,    41,     0,     0,     0,     0,
       0,     0,     0,     0,    42,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    39,    55,    56,    57,     0,     0,
      58,     0,    41,     0,    59,    60,    61,    62,    63,    64,
       0,   577,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,    22,     0,    87,    39,    88,
       0,     0,     0,     0,    92,    65,    41,    93,     0,     0,
       0,     0,     0,     0,    98,    66,    67,     0,    43,    68,
      69,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    70,    55,    56,    57,     0,
       0,    58,    65,     0,   103,    59,    60,    61,    62,    63,
      64,     0,    66,    67,     0,     0,    68,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   578,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    23,   111,     0,     0,     0,   175,     0,   113,     0,
       0,     0,   584,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   244,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,   154,   110,     0,   155,     0,     0,     0,
       0,     0,   262,   156,     0,     0,     0,     0,   111,     0,
       0,     0,   175,   263,   113,     0,     0,     0,     0,   264,
     246,     0,    66,   114,   157,   247,    68,   115,   159,   248,
       0,     0,   116,   117,   118,   119,     0,     0,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    41,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   244,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,   155,     0,     0,     0,     0,     0,     0,   156,
       0,     0,     0,     0,   111,   245,     0,     0,   175,     0,
     113,     0,     0,     0,     0,     0,   246,     0,    66,   114,
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
     111,     0,     0,     0,   175,     0,   113,     0,     0,     0,
       0,     0,   246,     0,    66,   114,   157,   247,    68,   115,
     159,   248,     0,     0,   116,   117,   118,   119,     0,     0,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      41,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   583,     0,     0,   111,     0,     0,     0,
     175,     0,   113,     0,     0,     0,   584,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
     363,    58,     0,     0,   103,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   175,     0,   113,     0,
       0,     0,     0,     0,     0,     0,    66,   114,     0,     0,
      68,   115,     0,     0,     0,     0,   116,   117,   118,   119,
       0,     0,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    41,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   175,     0,
     113,     0,     0,     0,     0,     0,     0,     0,    66,   114,
       0,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,     0,     0,   120,    87,    39,    88,     0,     0,
       0,     0,    92,     0,    41,    93,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,    43,     0,     0,     0,
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
      41,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     175,     0,   113,     0,     0,     0,     0,     0,     0,     0,
      66,   114,     0,     0,    68,   115,     0,     0,     0,     0,
     116,   117,   118,   119,     0,     0,   120,    87,    39,    88,
       0,     0,     0,     0,    92,     0,    41,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,   298,   108,     0,     0,     0,     0,   110,     0,
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
       0,     0,   107,     0,     0,     0,   401,     0,     0,   108,
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
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   175,     0,
     113,     0,    39,     0,     0,     0,     0,     0,    66,   114,
      41,     0,    68,   115,     0,     0,     0,     0,   116,   117,
     118,   119,    43,     0,   120,     0,   328,     0,    45,    46,
      47,   178,   179,   180,     0,     0,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   334,     0,     0,
       0,     0,     0,     0,   335,     0,     0,   181,     0,     0,
       0,   336,   182,     0,   183,     0,     0,     0,     0,     0,
       0,     0,   184,     0,     0,     0,   185,     0,    39,     0,
     186,   337,   187,     0,     0,     0,    41,   264,     0,     0,
     188,    67,   338,   339,     0,    69,   340,   341,    43,     0,
       0,     0,     0,     0,    45,    46,    47,   178,   179,   180,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   244,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,     0,  -201,     0,     0,   182,     0,
     183,     0,     0,     0,     0,     0,     0,     0,   184,     0,
       0,     0,   185,    39,     0,     0,   186,     0,   187,     0,
       0,    41,     0,     0,   773,     0,   188,    67,     0,   247,
       0,    69,     0,    43,     0,     0,     0,     0,     0,    45,
      46,    47,   178,   179,   180,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   244,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
       0,     0,     0,   182,     0,   183,     0,     0,     0,     0,
       0,     0,     0,   184,     0,     0,     0,   185,    39,     0,
       0,   186,     0,   187,     0,     0,    41,     0,     0,   773,
       0,   188,    67,   218,   247,     0,    69,     0,    43,     0,
       0,     0,     0,     0,    45,    46,    47,   178,   179,   180,
       0,   219,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,     0,     0,     0,     0,   182,     0,
     183,     0,     0,     0,     0,     0,     0,     0,   184,    39,
       0,     0,   185,     0,     0,     0,   186,    41,   187,     0,
       0,     0,     0,     0,     0,     0,   188,    67,     0,    43,
       0,    69,     0,   328,     0,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,     0,     0,     0,     0,   182,
       0,   183,     0,     0,     0,     0,     0,     0,     0,   184,
      39,     0,     0,   185,   329,     0,     0,   186,    41,   187,
       0,     0,     0,     0,     0,   683,     0,   188,    67,     0,
      43,     0,    69,     0,     0,     0,    45,    46,    47,   178,
     179,   180,     0,   684,     0,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   181,    39,     0,     0,     0,
     182,     0,   183,     0,    41,     0,     0,     0,     0,     0,
     184,     0,     0,     0,   185,     0,    43,     0,   186,     0,
     187,     0,    45,    46,    47,   178,   179,   180,   188,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     357,   181,     0,     0,     0,     0,   182,     0,   183,     0,
       0,     0,     0,     0,     0,     0,   184,    39,     0,     0,
     185,     0,     0,     0,   186,    41,   187,     0,     0,     0,
       0,     0,     0,     0,   188,    67,     0,    43,     0,    69,
       0,   328,     0,    45,    46,    47,   178,   179,   180,     0,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,     0,     0,     0,   182,     0,   183,
       0,     0,     0,     0,     0,     0,     0,   184,    39,     0,
       0,   185,     0,     0,     0,   186,    41,   187,     0,     0,
       0,     0,     0,     0,     0,   188,    67,     0,    43,     0,
      69,     0,   556,     0,    45,    46,    47,   178,   179,   180,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,    39,     0,     0,     0,   182,     0,
     183,     0,    41,     0,     0,     0,     0,     0,   184,     0,
       0,     0,   185,     0,    43,     0,   186,     0,   187,     0,
      45,    46,    47,   178,   179,   180,   188,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,   182,     0,   183,     0,     0,     0,
       0,     0,     0,     0,   184,     0,     0,     0,   185,    39,
       0,     0,   186,   780,   187,     0,     0,    41,     0,     0,
       0,     0,   188,    67,     0,     0,     0,    69,     0,    43,
       0,     0,     0,   328,     0,    45,    46,    47,   178,   179,
     180,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   181,    39,     0,     0,     0,   182,
       0,   183,     0,    41,     0,     0,     0,     0,     0,   184,
       0,     0,     0,   185,     0,    43,     0,   797,     0,   187,
       0,    45,    46,    47,   178,   179,   180,   188,    67,     0,
      53,    54,    69,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   806,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,    39,     0,     0,     0,   182,     0,   183,     0,    41,
       0,     0,     0,     0,     0,   184,     0,     0,     0,   185,
       0,    43,     0,   186,     0,   187,     0,    45,    46,    47,
     178,   179,   180,   188,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   809,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,    39,     0,     0,
       0,   182,     0,   183,     0,    41,     0,     0,     0,     0,
       0,   184,     0,     0,     0,   185,     0,    43,     0,   186,
       0,   187,     0,    45,    46,    47,   178,   179,   180,   188,
      67,     0,    53,    54,    69,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   181,     0,     0,     0,     0,   182,     0,   183,
       0,     0,     0,     0,     0,     0,     0,   184,     0,     0,
       0,   185,    39,     0,     0,   186,    40,   187,     0,     0,
      41,     0,     0,     0,     0,   188,    67,     0,     0,    42,
      69,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,     0,     0,   310,     0,     0,     0,     0,     0,     0,
      66,    67,     0,     0,    68,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,    65,     0,     0,
       0,    41,     0,     0,     0,     0,     0,    66,    67,     0,
     577,    68,    69,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,     0,    41,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,    65,    41,     0,     0,     0,     0,     0,     0,     0,
       0,    66,    67,     0,    43,    68,    69,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,    65,    41,
       0,    59,    60,    61,    62,    63,    64,     0,    66,    67,
       0,    43,    68,    69,     0,     0,     0,    45,    46,    47,
     178,   179,   180,     0,     0,     0,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    41,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    66,   114,    43,     0,    68,   115,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,     0,    41,
       0,    59,    60,    61,    62,    63,    64,     0,     0,   188,
      67,    43,     0,     0,    69,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,   453,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,    41,   230,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    66,    43,     0,     0,    68,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,     0,    41,   230,
      59,    60,    61,    62,    63,    64,     0,     0,     0,    66,
      43,     0,     0,    68,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    39,    55,    56,
      57,     0,     0,    58,     0,    41,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,    43,     0,     0,
       0,     0,     0,    45,    46,    47,   178,   179,   180,     0,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,    66,   114,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   322,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    66,     0,
       0,     0,     0,    39,   710,     0,     0,     0,     0,     0,
       0,    41,     0,     0,     0,     0,   711,   596,     0,     0,
       0,     0,     0,    43,     0,   597,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   188,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,     0,    41,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
      43,     0,     0,     0,     0,     0,    45,    46,    47,   178,
     179,   180,     0,     0,     0,    53,    54,    39,    55,    56,
      57,     0,     0,    58,     0,    41,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,    43,     0,     0,
       0,     0,     0,    45,    46,    47,   178,   179,   180,     0,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,    66,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   595,
     596,     0,     0,     0,     0,     0,     0,     0,   597,     0,
       0,     0,     0,    39,     0,     0,     0,     0,   188,     0,
       0,    41,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    43,     0,     0,   770,   596,     0,    45,
      46,    47,   178,   179,   180,   597,    39,     0,    53,    54,
       0,    55,    56,    57,    41,   188,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   178,   179,   180,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,    39,
       0,     0,     0,     0,     0,     0,     0,    41,     0,     0,
       0,     0,     0,   596,     0,     0,     0,     0,     0,    43,
       0,   597,     0,     0,     0,    45,    46,    47,   178,   179,
     180,   188,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,     0,     0,   597,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188
  };

  const short
  parser::yycheck_[] =
  {
      89,   113,   106,    34,    89,   111,   233,   106,   281,   208,
      29,   100,   184,   111,   112,   100,   192,   319,    89,    88,
      29,   557,   352,   185,    93,   218,    29,   214,   215,    40,
     103,    42,   102,   557,   107,   108,   285,   110,    65,   641,
     100,   287,   293,   442,   262,     1,   222,   718,     5,     1,
     315,    19,   279,   362,     1,    77,   472,   642,     1,    21,
     112,    77,   135,    27,     1,   133,   651,    34,   181,   182,
      77,     1,    91,    84,   186,   187,    80,   175,    65,    12,
     718,   194,   300,     1,    90,   112,     0,    19,   556,    27,
     246,   750,   181,   182,   118,   481,   185,   186,   119,   119,
     105,   113,   105,   119,   369,   194,   133,   119,   100,   177,
     246,   133,   498,   202,    77,   119,   112,   202,   383,    80,
      84,   142,   142,   175,   213,   112,   133,   712,   134,   218,
     219,   517,   288,   218,   219,   794,   205,   206,   447,    71,
     108,   812,   104,   814,   111,   112,    84,   166,   175,   168,
     177,   107,   288,   184,   405,   107,   119,   166,   119,   168,
     107,   407,   118,   166,   107,   168,   118,   240,   584,   242,
     107,   118,    81,   133,   812,   118,   562,   107,   646,   112,
      80,   118,   250,   214,   215,   298,   275,    77,   175,   107,
      79,    78,   101,   105,   283,   284,   414,    84,   283,   361,
     118,    14,    80,   230,    93,   109,   118,   472,   175,   298,
     202,    79,   283,   170,   171,   119,    77,   286,   287,   119,
      80,    80,   101,   250,    27,   228,   218,   219,   390,   302,
     319,   262,   119,    24,   113,   124,   125,   839,    80,   128,
     129,   119,    80,   230,   357,   113,   108,   214,   215,   551,
     552,   119,   123,    80,   285,   840,   127,   125,   843,   119,
     119,   350,   351,   352,   353,   350,    65,    66,   357,   300,
     301,   133,   361,   276,   277,   674,   525,   119,   105,   185,
     186,   119,   123,   113,   121,   122,   127,    93,   255,   119,
      96,   827,   279,   363,   118,   399,   626,   103,   301,   388,
     399,   390,   408,   827,   410,   316,   478,   213,   113,   118,
     408,   417,   410,   419,   382,   384,   112,   112,   124,   417,
     496,   419,   568,   485,   428,   590,   513,   123,   555,   389,
      79,   127,   401,   352,    37,   322,   108,   406,   407,   408,
     112,   740,   114,   411,    93,   610,   435,    96,    14,   115,
     435,   123,   105,   119,   103,   382,   574,   743,   350,   745,
     479,   353,   108,   749,   435,   118,   112,   101,   114,   275,
     104,    22,    80,    81,   112,   124,   125,   123,   284,   128,
     129,   127,   105,   414,   411,   123,   632,   476,   113,   127,
     479,   108,   481,   482,   119,   118,   485,   428,   105,   518,
      51,    52,   471,   472,    93,   105,   708,    96,   134,   498,
     105,   118,   605,   319,   103,    84,   113,    86,   118,   618,
     532,   533,   119,   118,   104,   671,   106,   516,   517,   518,
     760,    87,   619,   620,   699,   124,   505,   133,   507,   128,
     509,   408,   115,   410,   463,   351,   119,   478,   772,   468,
     417,   775,   419,   615,   463,   361,    80,    81,   489,   490,
     463,   112,   551,   552,     4,   468,   115,   573,   113,   104,
     119,   106,   123,   562,   804,   573,   127,   596,   597,    79,
     113,   115,   513,   756,   390,   119,    86,   104,   113,   106,
     683,   718,   113,    93,   525,   104,   826,   106,   567,   568,
      72,    73,    74,    75,    80,    81,   595,   596,   597,   104,
     104,   106,   106,   113,   779,   584,   605,   735,    23,   119,
     605,   640,   489,   490,   124,   125,   615,   113,   128,   129,
     104,   104,   106,   106,   836,   119,   649,   626,    80,    81,
      27,   572,    91,   574,   809,   810,   513,   112,   118,    77,
      77,   640,    77,    77,   643,    15,    41,   576,   643,    81,
     649,   109,   755,   632,   633,   109,   577,   119,   555,   572,
     476,   758,   113,   576,   113,   113,   482,    71,   133,   485,
     113,   113,   119,   119,   105,   812,   105,    80,   619,   620,
      91,   109,   768,   682,   683,   684,   715,   682,   683,   684,
     113,   670,   671,   672,   113,   718,   573,   119,    80,   113,
     516,    85,    85,   605,    25,   704,    81,    27,   133,   708,
     134,   710,   711,   109,    77,   710,   715,    77,   734,   718,
     113,   750,    77,   652,    77,   119,   734,    27,   831,   728,
     711,   120,   120,   728,    80,   551,   552,   666,    80,   119,
      81,   643,   619,   620,   743,   119,   745,   688,   120,   772,
     749,   750,   775,   666,   105,    81,   755,   101,   101,   773,
     755,   760,   761,   762,   778,   794,   761,   762,   797,   778,
     120,   120,   112,   772,   753,   797,   775,   113,   113,   595,
     682,   683,   684,    84,   109,   119,   785,   109,   119,   812,
     785,   814,    78,   120,   735,   794,   120,    91,   797,   615,
     729,    77,    28,   802,    27,   804,     9,   802,   710,   711,
     789,   688,   811,   812,   113,   814,   811,   758,   105,    37,
     819,   802,   118,   113,   819,    27,   728,   826,    79,    84,
      81,    80,   831,    84,   113,    80,   831,   836,    11,    19,
      86,    79,    93,    91,    53,    96,    86,    91,    79,    80,
     123,   119,   103,   755,    79,    93,   113,   734,   105,   761,
     762,    86,    93,   113,    84,    96,    91,    84,    93,   120,
     105,   166,   103,   124,   125,   579,   576,   128,   129,   666,
     826,   758,   120,   785,   695,   124,   124,   125,   704,   120,
     128,   129,   708,   124,   125,   120,    34,   128,   129,   124,
     125,   762,   746,   128,   129,   283,   802,   748,   688,   811,
     490,   215,   758,   620,    15,   552,   275,   819,   704,   819,
     361,   518,   723,     3,     4,     5,     6,     7,     8,   831,
      10,   816,    12,    13,   750,   772,    16,    17,    18,    19,
      20,   716,    22,   654,    24,   478,    26,   729,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    34,    42,    43,    44,    79,   109,    47,   671,   240,
      50,    51,    52,    53,    54,    55,    56,    57,   290,    93,
      79,   797,    96,    63,   670,   674,   507,   206,   572,   103,
     399,   398,   555,   778,    93,    -1,    76,    96,   250,    -1,
      -1,    -1,    82,    -1,   103,    -1,   120,    -1,    -1,    89,
     124,   125,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
     836,   120,    -1,    -1,    -1,   124,   125,    -1,   108,   128,
     129,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,    -1,
      -1,    -1,    -1,   143,   144,     3,     4,     5,    -1,     7,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,
      18,    -1,    20,    -1,    22,    -1,    24,    -1,    26,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    79,    -1,    -1,    63,    -1,    -1,    -1,    86,
      -1,    -1,    -1,    -1,    -1,    -1,    93,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    -1,    94,    -1,    -1,    -1,
      -1,    -1,    -1,   120,    -1,    -1,    -1,   124,   125,    -1,
     108,   128,   129,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,    -1,    -1,   126,   127,
      -1,    -1,    -1,    -1,   132,   133,   134,   135,    -1,    -1,
     138,    -1,    -1,    -1,    -1,   143,   144,     3,     4,     5,
      -1,     7,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
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
      -1,    -1,   138,     3,     4,     5,    -1,   143,   144,    -1,
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
      -1,    -1,   132,   133,   134,   135,    -1,     4,   138,    -1,
      -1,     8,    -1,   143,   144,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    21,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,    -1,    12,    -1,    51,    52,    53,    54,    55,    56,
      -1,    21,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,   112,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,   122,   123,    -1,    24,   126,
     127,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,   142,    42,    43,    44,    -1,
      -1,    47,   112,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,   122,   123,    -1,    -1,   126,   127,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,   108,    -1,    -1,    -1,   112,    -1,   114,    -1,
      -1,    -1,   118,    -1,    -1,    -1,   122,   123,    -1,    -1,
     126,   127,    -1,    -1,    -1,    -1,   132,   133,   134,   135,
      -1,    -1,   138,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,   102,   103,    -1,    -1,    -1,    -1,   108,    -1,
      -1,    -1,   112,   113,   114,    -1,    -1,    -1,    -1,   119,
     120,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
      -1,    -1,   132,   133,   134,   135,    -1,    -1,   138,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    -1,
      94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,   123,
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
     108,    -1,    -1,    -1,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,   120,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,    -1,    -1,   132,   133,   134,   135,    -1,    -1,
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
      -1,    -1,    -1,   105,    -1,    -1,   108,    -1,    -1,    -1,
     112,    -1,   114,    -1,    -1,    -1,   118,    -1,    -1,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      46,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
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
      -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    94,    -1,
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
      -1,    -1,    82,    -1,    -1,    -1,    86,    -1,    -1,    89,
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
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,   112,    -1,
     114,    -1,     4,    -1,    -1,    -1,    -1,    -1,   122,   123,
      12,    -1,   126,   127,    -1,    -1,    -1,    -1,   132,   133,
     134,   135,    24,    -1,   138,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    -1,    -1,
      -1,    93,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,     4,    -1,
     112,   113,   114,    -1,    -1,    -1,    12,   119,    -1,    -1,
     122,   123,   124,   125,    -1,   127,   128,   129,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    -1,    -1,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,     4,    -1,    -1,   112,    -1,   114,    -1,
      -1,    12,    -1,    -1,   120,    -1,   122,   123,    -1,   125,
      -1,   127,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,   108,     4,    -1,
      -1,   112,    -1,   114,    -1,    -1,    12,    -1,    -1,   120,
      -1,   122,   123,    19,   125,    -1,   127,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,
      96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,     4,
      -1,    -1,   108,    -1,    -1,    -1,   112,    12,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    24,
      -1,   127,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    94,
      -1,    96,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
       4,    -1,    -1,   108,   109,    -1,    -1,   112,    12,   114,
      -1,    -1,    -1,    -1,    -1,    19,    -1,   122,   123,    -1,
      24,    -1,   127,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    -1,    39,    40,    -1,    42,    43,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    94,    -1,    96,    -1,
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
      -1,   108,    -1,    -1,    -1,   112,    12,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,    -1,    24,    -1,
     127,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    -1,    -1,    94,    -1,    96,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,     4,
      -1,    -1,   112,   113,   114,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   122,   123,    -1,    -1,    -1,   127,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    -1,    -1,    94,    -1,    96,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,
      -1,   108,     4,    -1,    -1,   112,     8,   114,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   122,   123,    -1,    -1,    21,
     127,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     112,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,   112,    -1,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,   122,   123,    -1,
      21,   126,   127,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,   112,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,    -1,    24,   126,   127,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,   112,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,   122,   123,
      -1,    24,   126,   127,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    24,    -1,   126,   127,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,   122,
     123,    24,    -1,    -1,   127,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    78,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    24,    -1,    -1,   126,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,   112,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,   122,
      24,    -1,    -1,   126,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,   122,   123,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
      -1,    -1,    -1,     4,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   103,   104,    -1,    -1,
      -1,    -1,    -1,    24,    -1,   112,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,   122,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,    -1,    12,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,   122,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   112,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,   122,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,   103,   104,    -1,    30,
      31,    32,    33,    34,    35,   112,     4,    -1,    39,    40,
      -1,    42,    43,    44,    12,   122,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,    24,
      -1,   112,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,   122,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   104,   146,   147,   148,   151,   123,   127,   353,
     152,   163,     0,   152,    65,    66,   149,   105,   118,   153,
     164,   165,     1,   107,   352,   108,   133,   220,   220,   112,
     154,    14,   166,   177,   178,   133,   221,    77,    77,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   112,   122,   123,   126,   127,
     142,   155,   156,   157,   161,   319,   322,   323,   336,   338,
     339,   345,    27,    24,   167,   118,   162,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    29,    36,    50,    57,    63,    76,    82,    89,    92,
      94,   108,   112,   114,   123,   127,   132,   133,   134,   135,
     138,   143,   144,   175,   179,   180,   181,   182,   184,   199,
     225,   265,   270,   274,   275,   277,   278,   279,   280,   308,
     309,   312,   313,   335,   336,   339,   347,   348,   351,   109,
     119,   353,   353,    79,    93,    96,   103,   124,   125,   128,
     129,   342,   343,   344,   346,   113,   119,   142,   112,   158,
     104,   106,   150,   353,   118,   112,   273,   274,    33,    34,
      35,    89,    94,    96,   104,   108,   112,   114,   122,   205,
     230,   232,   234,   235,   237,   241,   317,   318,   322,   332,
     334,   345,    37,   200,   322,   104,   106,   300,   273,    72,
      73,    74,    75,   185,   104,   106,   217,   218,    19,    37,
     183,   232,   234,   318,    14,   300,   278,   108,   271,   272,
     112,   336,   108,   112,   310,   311,   313,   348,   278,   298,
     299,   278,   277,   278,    79,   109,   120,   125,   129,   273,
     274,   282,   284,   285,   315,   329,   331,   341,   342,   344,
     349,   350,   102,   113,   119,   281,   282,   283,   342,   343,
     344,   349,   354,   115,   354,    19,   271,   271,   134,   174,
     162,    19,    71,   207,    80,   119,    81,    84,   120,   267,
     268,   269,   315,   328,   330,   340,   342,   343,    88,   278,
     101,   104,    87,   133,   113,   113,   113,   113,   113,   157,
      78,   159,   160,   161,   152,   152,     4,   168,    23,    80,
     241,   241,   112,   225,   258,   259,   260,   339,    28,   109,
     227,   228,   230,   232,    79,    86,    93,   113,   124,   125,
     128,   129,   227,   245,   324,   325,   354,   354,    27,   211,
      91,    86,   120,   239,   320,   324,   333,    88,   238,   241,
     232,   112,    20,    46,   273,   297,   301,   302,   303,   301,
     118,   276,    77,    77,    77,    77,   223,   228,   242,   216,
     265,   266,   274,   216,    15,   194,   232,   232,    80,   119,
      81,    41,    90,   134,   336,    77,   133,   350,    80,   119,
     222,    86,   298,   338,   347,   328,    78,    84,   119,   109,
     119,   274,   337,   339,   101,   113,   113,   119,   113,   119,
     113,   113,   113,   119,   115,   242,   336,   336,   120,   176,
     314,   326,   327,   343,   350,   207,   133,   205,   224,   228,
     229,   335,   273,   287,   288,   303,   338,    27,   219,   269,
     275,   241,   337,    78,   304,   305,   306,   336,   337,   339,
     278,   113,   113,   119,   105,   352,   353,    12,   112,   169,
     170,   104,   106,   289,   223,   343,    80,   105,   119,   246,
     109,    80,    91,   113,   113,   119,   113,   113,   115,   104,
     106,   210,   232,   228,   322,   334,   234,   241,    80,   202,
     227,   244,   245,   218,   300,    85,   105,   118,   352,    25,
      27,   215,   105,   118,   352,   273,    81,    80,    81,   203,
     228,   249,   112,   318,   227,   133,   134,   109,    77,    77,
     113,   108,   112,   114,   316,   317,   310,    77,   273,   120,
     120,   273,   286,   303,   273,   282,   282,   337,   282,   282,
      77,    80,    80,   339,   348,   119,    28,   206,   230,   232,
      77,   133,    80,    81,   201,   253,   219,    81,   119,   120,
     218,   105,   119,    81,   101,   161,   112,    21,   142,   161,
     171,   172,   173,   105,   118,   273,   290,   291,   292,   296,
     290,   352,   113,   228,   260,   103,   104,   112,   247,   248,
     332,   249,   228,   227,     7,    26,   195,   208,   209,   266,
     209,   120,   120,   249,   113,   119,   273,   302,   273,   104,
     106,   214,   266,   228,   249,   247,    84,   188,   335,   109,
     113,   115,   119,    78,   223,   226,   226,   120,   120,   326,
     246,   201,   253,    91,    77,   249,    28,   254,   255,   256,
      27,   250,     9,   261,   262,   263,   273,   303,   305,   282,
     307,   337,   171,   353,   158,   113,   119,   142,   290,   105,
     118,    84,    86,   293,   294,   295,   352,   228,   332,   332,
     113,    37,   196,    19,    37,   194,   232,   105,   118,   352,
     227,   276,     7,    26,   198,   199,   212,   213,   266,   213,
     189,   334,    27,   191,    80,   303,   273,    77,   119,    77,
      91,   103,   250,   261,   232,   246,    84,   231,   236,   240,
     241,   257,   104,   106,   261,    22,    51,    52,   112,   186,
     264,   321,   322,   263,    81,   101,   113,   173,   292,   287,
     273,   219,   295,    80,   105,    80,   232,   194,   232,    80,
      81,   204,   208,    11,    19,   197,   197,   105,   118,   352,
      86,   104,   106,   192,   224,   223,   232,   230,   234,   261,
     103,   255,    91,   120,   240,   314,   251,   252,   276,   251,
     113,   232,   233,   243,   264,    53,   187,   282,   337,    86,
     219,   249,   249,   202,    81,   204,   249,   112,   248,   332,
     273,   194,   207,   212,   190,   334,    78,   193,   194,    78,
     193,    91,   236,   257,   236,   105,   118,   311,   352,   119,
     113,   232,   273,   105,   113,   332,    84,   206,   334,   105,
     105,   118,   352,   352,   232,   252,    80,   243,   189,   201,
     253,   194,   223,   250,   261,   261
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
     248,   249,   250,   250,   250,   251,   251,   252,   253,   254,
     254,   255,   255,   256,   256,   257,   257,   258,   258,   259,
     259,   260,   261,   261,   262,   262,   263,   263,   263,   264,
     264,   264,   265,   265,   266,   267,   267,   268,   268,   269,
     270,   270,   270,   270,   270,   270,   270,   270,   270,   271,
     271,   272,   272,   273,   273,   274,   274,   275,   275,   276,
     276,   277,   277,   277,   277,   278,   278,   278,   278,   278,
     278,   278,   278,   278,   278,   279,   279,   279,   280,   280,
     280,   280,   280,   280,   280,   280,   281,   281,   282,   282,
     282,   283,   283,   284,   284,   284,   284,   284,   284,   284,
     285,   285,   286,   286,   287,   288,   288,   289,   289,   289,
     289,   290,   290,   291,   291,   291,   292,   293,   293,   294,
     294,   295,   296,   297,   298,   299,   299,   300,   300,   301,
     301,   301,   301,   302,   302,   303,   303,   303,   304,   304,
     305,   305,   305,   306,   306,   306,   306,   307,   307,   308,
     308,   309,   309,   310,   310,   310,   311,   311,   312,   312,
     312,   312,   313,   313,   314,   314,   315,   315,   316,   316,
     316,   317,   317,   317,   317,   317,   318,   318,   319,   319,
     319,   319,   320,   320,   321,   322,   322,   323,   324,   324,
     324,   325,   325,   325,   325,   326,   326,   327,   327,   328,
     328,   329,   329,   330,   330,   331,   331,   332,   333,   334,
     334,   334,   334,   334,   335,   335,   336,   336,   336,   337,
     338,   338,   339,   339,   339,   339,   339,   339,   339,   339,
     340,   340,   341,   341,   342,   343,   343,   344,   344,   345,
     345,   345,   345,   345,   345,   345,   345,   345,   345,   345,
     345,   345,   345,   345,   345,   345,   345,   346,   346,   347,
     347,   348,   349,   349,   350,   350,   351,   351,   351,   351,
     351,   352,   352,   353,   353,   354,   354
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
       0,     1,     1,     1,     1,     5,     7,     1,     1,     3,
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
       5,     1,     4,     4,     0,     3,     1,     4,     2,     3,
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
       0,   516,   516,   533,   534,   536,   540,   541,   542,   544,
     545,   547,   548,   551,   553,   554,   555,   563,   564,   566,
     567,   568,   569,   571,   572,   574,   575,   576,   578,   579,
     580,   582,   583,   585,   586,   588,   589,   593,   594,   596,
     597,   599,   601,   602,   604,   617,   618,   620,   621,   623,
     624,   628,   629,   631,   632,   633,   634,   636,   637,   639,
     640,   645,   646,   648,   649,   650,   652,   653,   657,   659,
     660,   662,   663,   664,   665,   668,   669,   675,   677,   680,
     683,   684,   686,   687,   688,   690,   692,   693,   696,   697,
     698,   704,   711,   712,   713,   714,   715,   717,   718,   719,
     721,   732,   733,   735,   737,   738,   742,   743,   745,   746,
     747,   748,   750,   751,   752,   753,   755,   758,   760,   762,
     764,   765,   767,   767,   769,   769,   773,   775,   782,   789,
     790,   793,   794,   798,   799,   801,   802,   804,   805,   806,
     808,   809,   810,   813,   814,   817,   818,   819,   820,   822,
     823,   824,   866,   867,   869,   870,   871,   872,   874,   875,
     877,   878,   880,   881,   883,   884,   885,   886,   888,   889,
     891,   892,   895,   896,   897,   898,   900,   901,   903,   905,
     906,   914,   915,   917,   918,   919,   932,   933,   942,   944,
     946,   947,   949,   950,   959,   960,   962,   963,   965,   967,
     976,   978,   980,   981,   983,   986,   988,   989,   991,   992,
     994,   996,   997,   999,  1001,  1002,  1009,  1016,  1017,  1018,
    1019,  1020,  1021,  1022,  1023,  1029,  1030,  1033,  1035,  1036,
    1038,  1039,  1041,  1042,  1049,  1050,  1052,  1053,  1054,  1057,
    1058,  1076,  1082,  1083,  1084,  1086,  1087,  1089,  1091,  1093,
    1094,  1096,  1097,  1099,  1100,  1102,  1103,  1109,  1110,  1112,
    1113,  1115,  1117,  1118,  1120,  1121,  1123,  1124,  1125,  1127,
    1128,  1129,  1134,  1136,  1138,  1142,  1143,  1145,  1146,  1150,
    1160,  1161,  1163,  1164,  1165,  1166,  1167,  1168,  1169,  1172,
    1173,  1175,  1176,  1181,  1182,  1186,  1187,  1189,  1190,  1192,
    1193,  1198,  1199,  1200,  1201,  1204,  1205,  1206,  1207,  1208,
    1210,  1212,  1213,  1214,  1216,  1219,  1220,  1221,  1224,  1225,
    1226,  1227,  1228,  1229,  1234,  1235,  1238,  1239,  1244,  1245,
    1246,  1251,  1252,  1270,  1271,  1272,  1273,  1274,  1275,  1276,
    1278,  1279,  1292,  1294,  1304,  1306,  1307,  1310,  1311,  1312,
    1313,  1315,  1316,  1318,  1319,  1320,  1322,  1324,  1325,  1327,
    1328,  1337,  1339,  1341,  1343,  1345,  1346,  1349,  1350,  1352,
    1353,  1354,  1355,  1360,  1361,  1363,  1364,  1365,  1370,  1371,
    1373,  1374,  1375,  1377,  1378,  1379,  1380,  1383,  1384,  1416,
    1417,  1419,  1420,  1422,  1423,  1424,  1426,  1427,  1429,  1430,
    1431,  1432,  1434,  1435,  1437,  1438,  1440,  1441,  1444,  1445,
    1446,  1448,  1449,  1450,  1451,  1452,  1454,  1455,  1457,  1458,
    1459,  1460,  1463,  1464,  1466,  1468,  1469,  1473,  1475,  1476,
    1477,  1479,  1480,  1481,  1482,  1487,  1488,  1490,  1491,  1493,
    1494,  1497,  1498,  1503,  1504,  1506,  1507,  1511,  1513,  1515,
    1516,  1517,  1518,  1519,  1522,  1523,  1525,  1526,  1527,  1529,
    1531,  1532,  1534,  1535,  1536,  1537,  1538,  1539,  1540,  1541,
    1543,  1544,  1546,  1547,  1549,  1551,  1552,  1554,  1555,  1557,
    1558,  1559,  1560,  1561,  1562,  1563,  1564,  1565,  1566,  1567,
    1568,  1569,  1570,  1571,  1572,  1573,  1574,  1576,  1577,  1581,
    1582,  1584,  1586,  1587,  1589,  1590,  1594,  1595,  1596,  1597,
    1598,  1603,  1606,  1610,  1611,  1613,  1614
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
#line 7409 "parser.cc"

#line 1623 "parser.y"


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

