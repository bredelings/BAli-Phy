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
#line 55 "parser.y"

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

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Hs::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.YY_MOVE_OR_COPY< Hs::ImpSpec > (YY_MOVE (that.value));
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

      case symbol_kind::S_export: // export
        value.YY_MOVE_OR_COPY< Located<Hs::Export> > (YY_MOVE (that.value));
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

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.YY_MOVE_OR_COPY< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.YY_MOVE_OR_COPY< std::optional<std::vector<Located<Hs::Export>>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Hs::Context,Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.YY_MOVE_OR_COPY< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_modid: // modid
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.YY_MOVE_OR_COPY< std::vector<Hs::ImpDecl> > (YY_MOVE (that.value));
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.YY_MOVE_OR_COPY< std::vector<Located<Hs::Export>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (YY_MOVE (that.value));
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

      case symbol_kind::S_export: // export
        value.move< Located<Hs::Export> > (YY_MOVE (that.value));
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

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Located<Hs::Export>>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::LType> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_modid: // modid
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::ImpDecl> > (YY_MOVE (that.value));
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Located<Hs::Export>> > (YY_MOVE (that.value));
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

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Hs::ImpDecl > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.copy< Hs::ImpSpec > (that.value);
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

      case symbol_kind::S_export: // export
        value.copy< Located<Hs::Export> > (that.value);
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

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.copy< std::optional<Located<Hs::Kind>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.copy< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.copy< std::optional<std::vector<Located<Hs::Export>>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Hs::Context,Hs::LType> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.copy< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (that.value);
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
      case symbol_kind::S_modid: // modid
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.copy< std::vector<Hs::ImpDecl> > (that.value);
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.copy< std::vector<Located<Hs::Export>> > (that.value);
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

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::ImpDecl > (that.value);
        break;

      case symbol_kind::S_impspec: // impspec
        value.move< Hs::ImpSpec > (that.value);
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

      case symbol_kind::S_export: // export
        value.move< Located<Hs::Export> > (that.value);
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

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        value.move< std::optional<Located<Hs::Kind>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        value.move< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > (that.value);
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        value.move< std::optional<std::vector<Located<Hs::Export>>> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Hs::Context,Hs::LType> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > (that.value);
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
      case symbol_kind::S_modid: // modid
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Hs::ImpDecl> > (that.value);
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        value.move< std::vector<Located<Hs::Export>> > (that.value);
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

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Hs::ImpDecl > ();
        break;

      case symbol_kind::S_impspec: // impspec
        yylhs.value.emplace< Hs::ImpSpec > ();
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

      case symbol_kind::S_export: // export
        yylhs.value.emplace< Located<Hs::Export> > ();
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

      case symbol_kind::S_opt_tyfam_kind_sig: // opt_tyfam_kind_sig
      case symbol_kind::S_opt_at_kind_inj_sig: // opt_at_kind_inj_sig
        yylhs.value.emplace< std::optional<Located<Hs::Kind>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_where_type_family: // where_type_family
        yylhs.value.emplace< std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ();
        break;

      case symbol_kind::S_maybeexports: // maybeexports
        yylhs.value.emplace< std::optional<std::vector<Located<Hs::Export>>> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Hs::Context,Hs::LType> > ();
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        yylhs.value.emplace< std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();
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
      case symbol_kind::S_modid: // modid
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

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        yylhs.value.emplace< std::vector<Hs::ImpDecl> > ();
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

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
        yylhs.value.emplace< std::vector<Located<Hs::Export>> > ();
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
#line 509 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2446 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 526 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < std::string > (),yystack_[2].value.as < std::optional<std::vector<Located<Hs::Export>>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2452 "parser.cc"
    break;

  case 4: // module: body2
#line 527 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{"Main",{},yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2458 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 529 "parser.y"
                                                                 {drv.push_module_context();}
#line 2464 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 537 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2470 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 538 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2476 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 540 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2482 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 541 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2488 "parser.cc"
    break;

  case 13: // top: semis top1
#line 544 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2494 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 546 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2500 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 547 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::ImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2506 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 548 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::ImpDecl> > (),{});}
#line 2512 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 556 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Located<Hs::Export>>> > () = yystack_[1].value.as < std::vector<Located<Hs::Export>> > ();}
#line 2518 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 557 "parser.y"
                                      {}
#line 2524 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 559 "parser.y"
                                      {yylhs.value.as < std::vector<Located<Hs::Export>> > () = yystack_[0].value.as < std::vector<Located<Hs::Export>> > ();}
#line 2530 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 561 "parser.y"
                                      {yylhs.value.as < std::vector<Located<Hs::Export>> > () = yystack_[2].value.as < std::vector<Located<Hs::Export>> > (); yylhs.value.as < std::vector<Located<Hs::Export>> > ().push_back(yystack_[0].value.as < Located<Hs::Export> > ());}
#line 2536 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 562 "parser.y"
                                      {yylhs.value.as < std::vector<Located<Hs::Export>> > ().push_back(yystack_[0].value.as < Located<Hs::Export> > ());}
#line 2542 "parser.cc"
    break;

  case 22: // export: qcname export_subspec
#line 564 "parser.y"
                                      {yylhs.value.as < Located<Hs::Export> > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2548 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 565 "parser.y"
                                      {yylhs.value.as < Located<Hs::Export> > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, {yystack_[0].location,yystack_[0].value.as < std::string > ()}}}; }
#line 2554 "parser.cc"
    break;

  case 24: // export_subspec: %empty
#line 567 "parser.y"
                                      {}
#line 2560 "parser.cc"
    break;

  case 25: // export_subspec: "(" qcnames ")"
#line 568 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2566 "parser.cc"
    break;

  case 26: // export_subspec: "(" ".." ")"
#line 569 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2572 "parser.cc"
    break;

  case 27: // qcnames: %empty
#line 571 "parser.y"
                   {}
#line 2578 "parser.cc"
    break;

  case 28: // qcnames: qcnames1
#line 572 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2584 "parser.cc"
    break;

  case 29: // qcnames1: qcnames1 "," qcname
#line 574 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2590 "parser.cc"
    break;

  case 30: // qcnames1: qcname
#line 575 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2596 "parser.cc"
    break;

  case 31: // qcname: qvar
#line 577 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2602 "parser.cc"
    break;

  case 32: // qcname: oqtycon_no_varcon
#line 578 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2608 "parser.cc"
    break;

  case 37: // importdecls: importdecls_semi importdecl
#line 588 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[1].value.as < std::vector<Hs::ImpDecl> > (), yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[0].value.as < Hs::ImpDecl > ()); }
#line 2614 "parser.cc"
    break;

  case 38: // importdecls_semi: importdecls_semi importdecl semis1
#line 590 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::ImpDecl> > () = yystack_[2].value.as < std::vector<Hs::ImpDecl> > (); yylhs.value.as < std::vector<Hs::ImpDecl> > ().push_back(yystack_[1].value.as < Hs::ImpDecl > ()); }
#line 2620 "parser.cc"
    break;

  case 39: // importdecls_semi: %empty
#line 591 "parser.y"
                         { }
#line 2626 "parser.cc"
    break;

  case 40: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 593 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::ImpDecl > () = Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ());
}
#line 2634 "parser.cc"
    break;

  case 41: // optqualified: "qualified"
#line 606 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2640 "parser.cc"
    break;

  case 42: // optqualified: %empty
#line 607 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2646 "parser.cc"
    break;

  case 43: // maybeas: "as" modid
#line 609 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2652 "parser.cc"
    break;

  case 44: // maybeas: %empty
#line 610 "parser.y"
                               { }
#line 2658 "parser.cc"
    break;

  case 45: // maybeimpspec: impspec
#line 612 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 2664 "parser.cc"
    break;

  case 46: // maybeimpspec: %empty
#line 613 "parser.y"
                               { }
#line 2670 "parser.cc"
    break;

  case 47: // impspec: "(" exportlist ")"
#line 617 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Located<Hs::Export>> > ()}; }
#line 2676 "parser.cc"
    break;

  case 48: // impspec: "hiding" "(" exportlist ")"
#line 618 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Located<Hs::Export>> > ()}; }
#line 2682 "parser.cc"
    break;

  case 49: // prec: %empty
#line 623 "parser.y"
                   { }
#line 2688 "parser.cc"
    break;

  case 50: // prec: "INTEGER"
#line 624 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 2694 "parser.cc"
    break;

  case 51: // infix: "infix"
#line 626 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 2700 "parser.cc"
    break;

  case 52: // infix: "infixl"
#line 627 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 2706 "parser.cc"
    break;

  case 53: // infix: "infixr"
#line 628 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 2712 "parser.cc"
    break;

  case 54: // ops: ops "," op
#line 630 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 2718 "parser.cc"
    break;

  case 55: // ops: op
#line 631 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 2724 "parser.cc"
    break;

  case 56: // topdecls: topdecls_semi topdecl
#line 635 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ()); }
#line 2730 "parser.cc"
    break;

  case 57: // topdecls_semi: topdecls_semi topdecl semis1
#line 637 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 2736 "parser.cc"
    break;

  case 58: // topdecls_semi: %empty
#line 638 "parser.y"
                                            { }
#line 2742 "parser.cc"
    break;

  case 59: // topdecl: cl_decl
#line 640 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2748 "parser.cc"
    break;

  case 60: // topdecl: ty_decl
#line 641 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2754 "parser.cc"
    break;

  case 61: // topdecl: standalone_kind_sig
#line 642 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2760 "parser.cc"
    break;

  case 62: // topdecl: inst_decl
#line 643 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2766 "parser.cc"
    break;

  case 63: // topdecl: "default" "(" comma_types0 ")"
#line 646 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::DefaultDecl(yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 2772 "parser.cc"
    break;

  case 64: // topdecl: "foreign" "import" "bpcall" "STRING" var "::" sigtypedoc
#line 647 "parser.y"
                                                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ForeignDecl(yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 2778 "parser.cc"
    break;

  case 65: // topdecl: decl_no_th
#line 654 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 2784 "parser.cc"
    break;

  case 66: // topdecl: infixexp
#line 656 "parser.y"
                                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<Hs::InfixExp> > ();}
#line 2790 "parser.cc"
    break;

  case 67: // cl_decl: "class" tycl_hdr where_cls
#line 658 "parser.y"
                                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_class_decl(yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[1].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2796 "parser.cc"
    break;

  case 68: // ty_decl: "type" type "=" ktype
#line 660 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 2802 "parser.cc"
    break;

  case 69: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 661 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > ())};}
#line 2808 "parser.cc"
    break;

  case 70: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 663 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < std::optional<Hs::GADTConstructorsDecl> > ())};}
#line 2814 "parser.cc"
    break;

  case 71: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 664 "parser.y"
                                                                                          {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_type_family(yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 2820 "parser.cc"
    break;

  case 72: // standalone_kind_sig: "type" sks_vars "::" kind
#line 667 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < expression_ref > ())};}
#line 2826 "parser.cc"
    break;

  case 73: // sks_vars: sks_vars "," oqtycon
#line 669 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 2832 "parser.cc"
    break;

  case 74: // sks_vars: oqtycon
#line 670 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 2838 "parser.cc"
    break;

  case 75: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 673 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_instance_decl(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 2844 "parser.cc"
    break;

  case 76: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 674 "parser.y"
                                                                           {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 2850 "parser.cc"
    break;

  case 91: // where_type_family: %empty
#line 709 "parser.y"
                                                           {}
#line 2856 "parser.cc"
    break;

  case 92: // where_type_family: "where" ty_fam_inst_eqn_list
#line 710 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2862 "parser.cc"
    break;

  case 93: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 712 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2868 "parser.cc"
    break;

  case 94: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 713 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2874 "parser.cc"
    break;

  case 95: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 714 "parser.y"
                                                           {}
#line 2880 "parser.cc"
    break;

  case 96: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 715 "parser.y"
                                                           {}
#line 2886 "parser.cc"
    break;

  case 97: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 717 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 2892 "parser.cc"
    break;

  case 98: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 718 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 2898 "parser.cc"
    break;

  case 99: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 719 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 2904 "parser.cc"
    break;

  case 100: // ty_fam_inst_eqns: %empty
#line 720 "parser.y"
                                                           {}
#line 2910 "parser.cc"
    break;

  case 101: // ty_fam_inst_eqn: type "=" ctype
#line 722 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 2916 "parser.cc"
    break;

  case 102: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 725 "parser.y"
                                                               {}
#line 2922 "parser.cc"
    break;

  case 103: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 727 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_type_family(yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 2928 "parser.cc"
    break;

  case 104: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 729 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,make_type_family(yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 2934 "parser.cc"
    break;

  case 105: // at_decl_cls: "type" ty_fam_inst_eqn
#line 731 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 2940 "parser.cc"
    break;

  case 106: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 732 "parser.y"
                                                               { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 2946 "parser.cc"
    break;

  case 111: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 740 "parser.y"
                                                              { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 2952 "parser.cc"
    break;

  case 112: // data_or_newtype: "data"
#line 742 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 2958 "parser.cc"
    break;

  case 113: // data_or_newtype: "newtype"
#line 743 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 2964 "parser.cc"
    break;

  case 114: // opt_kind_sig: %empty
#line 747 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 2970 "parser.cc"
    break;

  case 115: // opt_kind_sig: "::" kind
#line 748 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < expression_ref > ();}
#line 2976 "parser.cc"
    break;

  case 118: // opt_tyfam_kind_sig: %empty
#line 753 "parser.y"
                                      {}
#line 2982 "parser.cc"
    break;

  case 119: // opt_tyfam_kind_sig: "::" kind
#line 754 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 2988 "parser.cc"
    break;

  case 120: // opt_tyfam_kind_sig: "=" tv_bndr
#line 755 "parser.y"
                                      {}
#line 2994 "parser.cc"
    break;

  case 121: // opt_at_kind_inj_sig: %empty
#line 757 "parser.y"
                                      {}
#line 3000 "parser.cc"
    break;

  case 122: // opt_at_kind_inj_sig: "::" kind
#line 758 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < expression_ref > ()}};}
#line 3006 "parser.cc"
    break;

  case 123: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 759 "parser.y"
                                                                  {}
#line 3012 "parser.cc"
    break;

  case 124: // tycl_hdr: context "=>" type
#line 763 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3018 "parser.cc"
    break;

  case 125: // tycl_hdr: type
#line 764 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3024 "parser.cc"
    break;

  case 129: // decl_cls: at_decl_cls
#line 810 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3030 "parser.cc"
    break;

  case 130: // decl_cls: decl
#line 811 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3036 "parser.cc"
    break;

  case 131: // decls_cls: decls_cls ";" decl_cls
#line 813 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3042 "parser.cc"
    break;

  case 132: // decls_cls: decls_cls ";"
#line 814 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3048 "parser.cc"
    break;

  case 133: // decls_cls: decl_cls
#line 815 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3054 "parser.cc"
    break;

  case 134: // decls_cls: %empty
#line 816 "parser.y"
                                           {}
#line 3060 "parser.cc"
    break;

  case 135: // decllist_cls: "{" decls_cls "}"
#line 818 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3066 "parser.cc"
    break;

  case 136: // decllist_cls: "vocurly" decls_cls close
#line 819 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3072 "parser.cc"
    break;

  case 137: // where_cls: "where" decllist_cls
#line 821 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3078 "parser.cc"
    break;

  case 138: // where_cls: %empty
#line 822 "parser.y"
                                           {}
#line 3084 "parser.cc"
    break;

  case 139: // decl_inst: at_decl_inst
#line 824 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3090 "parser.cc"
    break;

  case 140: // decl_inst: decl
#line 825 "parser.y"
                                           {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3096 "parser.cc"
    break;

  case 141: // decls_inst: decls_inst ";" decl_inst
#line 827 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3102 "parser.cc"
    break;

  case 142: // decls_inst: decls_inst ";"
#line 828 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3108 "parser.cc"
    break;

  case 143: // decls_inst: decl_inst
#line 829 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3114 "parser.cc"
    break;

  case 144: // decls_inst: %empty
#line 830 "parser.y"
                                           {}
#line 3120 "parser.cc"
    break;

  case 145: // decllist_inst: "{" decls_inst "}"
#line 832 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3126 "parser.cc"
    break;

  case 146: // decllist_inst: "vocurly" decls_inst close
#line 833 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3132 "parser.cc"
    break;

  case 147: // where_inst: "where" decllist_inst
#line 835 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3138 "parser.cc"
    break;

  case 148: // where_inst: %empty
#line 836 "parser.y"
                                           {}
#line 3144 "parser.cc"
    break;

  case 149: // decls: decls ";" decl
#line 839 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3150 "parser.cc"
    break;

  case 150: // decls: decls ";"
#line 840 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3156 "parser.cc"
    break;

  case 151: // decls: decl
#line 841 "parser.y"
                        {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3162 "parser.cc"
    break;

  case 152: // decls: %empty
#line 842 "parser.y"
                        {}
#line 3168 "parser.cc"
    break;

  case 153: // decllist: "{" decls "}"
#line 844 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3174 "parser.cc"
    break;

  case 154: // decllist: "vocurly" decls close
#line 845 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 3180 "parser.cc"
    break;

  case 155: // binds: decllist
#line 847 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3186 "parser.cc"
    break;

  case 156: // wherebinds: "where" binds
#line 849 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3192 "parser.cc"
    break;

  case 157: // wherebinds: %empty
#line 850 "parser.y"
                                 {}
#line 3198 "parser.cc"
    break;

  case 163: // opt_tyconsig: %empty
#line 876 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3204 "parser.cc"
    break;

  case 164: // opt_tyconsig: "::" gtycon
#line 877 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3210 "parser.cc"
    break;

  case 165: // sigtype: ctype
#line 886 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3216 "parser.cc"
    break;

  case 166: // sigtypedoc: ctypedoc
#line 888 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3222 "parser.cc"
    break;

  case 167: // sig_vars: sig_vars "," var
#line 890 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3228 "parser.cc"
    break;

  case 168: // sig_vars: var
#line 891 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3234 "parser.cc"
    break;

  case 169: // sigtypes1: sigtype
#line 893 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3240 "parser.cc"
    break;

  case 170: // sigtypes1: sigtypes1 "," sigtype
#line 894 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3246 "parser.cc"
    break;

  case 171: // ktype: ctype
#line 903 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3252 "parser.cc"
    break;

  case 172: // ktype: ctype "::" kind
#line 904 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < expression_ref > ())};}
#line 3258 "parser.cc"
    break;

  case 173: // ctype: "forall" tv_bndrs "." ctype
#line 906 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3264 "parser.cc"
    break;

  case 174: // ctype: context "=>" ctype
#line 907 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3270 "parser.cc"
    break;

  case 175: // ctype: type
#line 909 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3276 "parser.cc"
    break;

  case 176: // ctypedoc: ctype
#line 911 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3282 "parser.cc"
    break;

  case 177: // context: btype
#line 920 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3288 "parser.cc"
    break;

  case 178: // context_no_ops: btype_no_ops
#line 922 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3294 "parser.cc"
    break;

  case 179: // type: btype
#line 924 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3300 "parser.cc"
    break;

  case 180: // type: btype "->" ctype
#line 925 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3306 "parser.cc"
    break;

  case 181: // typedoc: type
#line 927 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3312 "parser.cc"
    break;

  case 182: // btype: infixtype
#line 930 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3318 "parser.cc"
    break;

  case 183: // infixtype: ftype
#line 932 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3324 "parser.cc"
    break;

  case 184: // infixtype: btype tyop btype
#line 933 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3330 "parser.cc"
    break;

  case 185: // btype_no_ops: atype_docs
#line 935 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3336 "parser.cc"
    break;

  case 186: // btype_no_ops: btype_no_ops atype_docs
#line 936 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3342 "parser.cc"
    break;

  case 187: // ftype: atype
#line 938 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3348 "parser.cc"
    break;

  case 188: // ftype: ftype tyarg
#line 940 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 3354 "parser.cc"
    break;

  case 189: // ftype: ftype "@" atype
#line 941 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 3360 "parser.cc"
    break;

  case 190: // tyarg: atype
#line 943 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3366 "parser.cc"
    break;

  case 191: // tyop: qtyconop
#line 945 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3372 "parser.cc"
    break;

  case 192: // tyop: tyvarop
#line 946 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3378 "parser.cc"
    break;

  case 193: // atype_docs: atype
#line 953 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3384 "parser.cc"
    break;

  case 194: // atype: ntgtycon
#line 960 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 3390 "parser.cc"
    break;

  case 195: // atype: tyvar
#line 961 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3396 "parser.cc"
    break;

  case 196: // atype: "*"
#line 962 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 3402 "parser.cc"
    break;

  case 197: // atype: PREFIX_BANG atype
#line 963 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 3408 "parser.cc"
    break;

  case 198: // atype: PREFIX_TILDE atype
#line 964 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 3414 "parser.cc"
    break;

  case 199: // atype: "{" fielddecls "}"
#line 965 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 3420 "parser.cc"
    break;

  case 200: // atype: "(" ")"
#line 966 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 3426 "parser.cc"
    break;

  case 201: // atype: "(" comma_types1 "," ktype ")"
#line 967 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 3432 "parser.cc"
    break;

  case 202: // atype: "[" ktype "]"
#line 973 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 3438 "parser.cc"
    break;

  case 203: // atype: "(" ktype ")"
#line 974 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 3444 "parser.cc"
    break;

  case 204: // inst_type: sigtype
#line 977 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3450 "parser.cc"
    break;

  case 207: // comma_types0: comma_types1
#line 982 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 3456 "parser.cc"
    break;

  case 208: // comma_types0: %empty
#line 983 "parser.y"
                                       { /* default construction OK */ }
#line 3462 "parser.cc"
    break;

  case 209: // comma_types1: ktype
#line 985 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3468 "parser.cc"
    break;

  case 210: // comma_types1: comma_types1 "," ktype
#line 986 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3474 "parser.cc"
    break;

  case 211: // tv_bndrs: tv_bndrs tv_bndr
#line 993 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 3480 "parser.cc"
    break;

  case 212: // tv_bndrs: %empty
#line 994 "parser.y"
                               { /* default construction OK */}
#line 3486 "parser.cc"
    break;

  case 213: // tv_bndr: tv_bndr_no_braces
#line 996 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 3492 "parser.cc"
    break;

  case 214: // tv_bndr: "{" tyvar "}"
#line 997 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 3498 "parser.cc"
    break;

  case 215: // tv_bndr: "{" tyvar "::" kind "}"
#line 998 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 3504 "parser.cc"
    break;

  case 216: // tv_bndr_no_braces: tyvar
#line 1001 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 3510 "parser.cc"
    break;

  case 217: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1002 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ())};}
#line 3516 "parser.cc"
    break;

  case 218: // kind: ctype
#line 1020 "parser.y"
             {yylhs.value.as < expression_ref > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 3522 "parser.cc"
    break;

  case 219: // gadt_constrlist: "where" "{" gadt_constrs "}"
#line 1026 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3528 "parser.cc"
    break;

  case 220: // gadt_constrlist: "where" "vocurly" gadt_constrs close
#line 1027 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 3534 "parser.cc"
    break;

  case 221: // gadt_constrlist: %empty
#line 1028 "parser.y"
                                                     {yylhs.value.as < std::optional<Hs::GADTConstructorsDecl> > () = {};}
#line 3540 "parser.cc"
    break;

  case 222: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1030 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3546 "parser.cc"
    break;

  case 223: // gadt_constrs: gadt_constr
#line 1031 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 3552 "parser.cc"
    break;

  case 224: // gadt_constr: optSemi con_list "::" sigtype
#line 1033 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3558 "parser.cc"
    break;

  case 225: // constrs: "=" constrs1
#line 1035 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 3564 "parser.cc"
    break;

  case 226: // constrs1: constrs1 "|" constr
#line 1037 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3570 "parser.cc"
    break;

  case 227: // constrs1: constr
#line 1038 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 3576 "parser.cc"
    break;

  case 228: // constr: forall context_no_ops "=>" constr_stuff
#line 1040 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 3582 "parser.cc"
    break;

  case 229: // constr: forall constr_stuff
#line 1041 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 3588 "parser.cc"
    break;

  case 230: // forall: "forall" tv_bndrs "."
#line 1043 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 3594 "parser.cc"
    break;

  case 231: // forall: %empty
#line 1044 "parser.y"
                                {}
#line 3600 "parser.cc"
    break;

  case 232: // constr_stuff: btype_no_ops
#line 1046 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 3606 "parser.cc"
    break;

  case 233: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1047 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::make_tyapps({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::make_tyapps(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::make_tyapps(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 3616 "parser.cc"
    break;

  case 234: // fielddecls: %empty
#line 1053 "parser.y"
                                {}
#line 3622 "parser.cc"
    break;

  case 235: // fielddecls: fielddecls1
#line 1054 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 3628 "parser.cc"
    break;

  case 236: // fielddecls1: fielddecls1 "," fielddecl
#line 1056 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3634 "parser.cc"
    break;

  case 237: // fielddecls1: fielddecl
#line 1057 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 3640 "parser.cc"
    break;

  case 238: // fielddecl: sig_vars "::" ctype
#line 1059 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 3646 "parser.cc"
    break;

  case 249: // decl_no_th: sigdecl
#line 1078 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3652 "parser.cc"
    break;

  case 250: // decl_no_th: infixexp rhs
#line 1080 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 3658 "parser.cc"
    break;

  case 251: // decl: decl_no_th
#line 1082 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3664 "parser.cc"
    break;

  case 252: // rhs: "=" exp wherebinds
#line 1086 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 3670 "parser.cc"
    break;

  case 253: // rhs: gdrhs wherebinds
#line 1087 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 3676 "parser.cc"
    break;

  case 254: // gdrhs: gdrhs gdrh
#line 1089 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3682 "parser.cc"
    break;

  case 255: // gdrhs: gdrh
#line 1090 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 3688 "parser.cc"
    break;

  case 256: // gdrh: "|" guardquals "=" exp
#line 1094 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 3694 "parser.cc"
    break;

  case 257: // sigdecl: sig_vars "::" sigtypedoc
#line 1104 "parser.y"
                                  { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::SignatureDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 3700 "parser.cc"
    break;

  case 258: // sigdecl: infix prec ops
#line 1105 "parser.y"
                         { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 3706 "parser.cc"
    break;

  case 259: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1107 "parser.y"
                                                    {}
#line 3712 "parser.cc"
    break;

  case 260: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1108 "parser.y"
                                            {}
#line 3718 "parser.cc"
    break;

  case 261: // sigdecl: "{-# SCC" qvar "#-}"
#line 1109 "parser.y"
                              {}
#line 3724 "parser.cc"
    break;

  case 262: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1110 "parser.y"
                                     {}
#line 3730 "parser.cc"
    break;

  case 263: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1111 "parser.y"
                                                               {}
#line 3736 "parser.cc"
    break;

  case 264: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1112 "parser.y"
                                                                      {}
#line 3742 "parser.cc"
    break;

  case 265: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1113 "parser.y"
                                                     {}
#line 3748 "parser.cc"
    break;

  case 270: // exp: infixexp "::" sigtype
#line 1125 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 3754 "parser.cc"
    break;

  case 271: // exp: infixexp
#line 1126 "parser.y"
                           { yylhs.value.as < Located<expression_ref> > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 3760 "parser.cc"
    break;

  case 272: // infixexp: exp10
#line 1130 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Located<expression_ref> > ()})};}
#line 3766 "parser.cc"
    break;

  case 273: // infixexp: infixexp qop exp10
#line 1131 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < expression_ref > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3772 "parser.cc"
    break;

  case 274: // exp10: PREFIX_MINUS fexp
#line 1133 "parser.y"
                                        {yylhs.value.as < Located<expression_ref> > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Located<expression_ref> > ()} )};}
#line 3778 "parser.cc"
    break;

  case 275: // exp10: fexp
#line 1134 "parser.y"
                               {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3784 "parser.cc"
    break;

  case 278: // fexp: fexp aexp
#line 1142 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::ApplyExp(yystack_[1].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ())};}
#line 3790 "parser.cc"
    break;

  case 279: // fexp: fexp "@" atype
#line 1143 "parser.y"
                                 {}
#line 3796 "parser.cc"
    break;

  case 280: // fexp: "static" aexp
#line 1144 "parser.y"
                                 {}
#line 3802 "parser.cc"
    break;

  case 281: // fexp: aexp
#line 1145 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3808 "parser.cc"
    break;

  case 282: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1148 "parser.y"
                                            {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::AsPattern(Hs::Var(yystack_[2].value.as < std::string > ()),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3814 "parser.cc"
    break;

  case 283: // aexp: PREFIX_TILDE aexp
#line 1149 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LazyPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3820 "parser.cc"
    break;

  case 284: // aexp: PREFIX_BANG aexp
#line 1150 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::StrictPattern(yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3826 "parser.cc"
    break;

  case 285: // aexp: "\\" apats1 "->" exp
#line 1151 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LambdaExp(yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3832 "parser.cc"
    break;

  case 286: // aexp: "let" binds "in" exp
#line 1152 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetExp(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3838 "parser.cc"
    break;

  case 287: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1154 "parser.y"
                                                       {yylhs.value.as < Located<expression_ref> > () = {yystack_[7].location+yystack_[0].location,Hs::IfExp(yystack_[6].value.as < Located<expression_ref> > (),yystack_[3].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())}; }
#line 3844 "parser.cc"
    break;

  case 288: // aexp: "case" exp "of" altslist
#line 1156 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::CaseExp(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::Alts > ())}; }
#line 3850 "parser.cc"
    break;

  case 289: // aexp: "do" stmtlist
#line 1157 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3856 "parser.cc"
    break;

  case 290: // aexp: "mdo" stmtlist
#line 1158 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 3862 "parser.cc"
    break;

  case 291: // aexp: aexp1
#line 1160 "parser.y"
                                 {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3868 "parser.cc"
    break;

  case 292: // aexp1: aexp1 "{" fbinds "}"
#line 1163 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = {yylhs.location,Hs::RecordExp{yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<Hs::FieldBindings> > ()}}; }
#line 3874 "parser.cc"
    break;

  case 293: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1164 "parser.y"
                                     { }
#line 3880 "parser.cc"
    break;

  case 294: // aexp1: aexp2
#line 1165 "parser.y"
                                     { yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > (); }
#line 3886 "parser.cc"
    break;

  case 295: // aexp2: qvar
#line 1168 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 3892 "parser.cc"
    break;

  case 296: // aexp2: qcon
#line 1169 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 3898 "parser.cc"
    break;

  case 297: // aexp2: literal
#line 1170 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[0].value.as < expression_ref > ()};}
#line 3904 "parser.cc"
    break;

  case 298: // aexp2: "(" texp ")"
#line 1171 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, unloc(yystack_[1].value.as < Located<expression_ref> > ())};}
#line 3910 "parser.cc"
    break;

  case 299: // aexp2: "(" tup_exprs ")"
#line 1172 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Located<expression_ref>> > ())};}
#line 3916 "parser.cc"
    break;

  case 300: // aexp2: "(" projection ")"
#line 1173 "parser.y"
                              {}
#line 3922 "parser.cc"
    break;

  case 301: // aexp2: "[" list "]"
#line 1178 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, yystack_[1].value.as < expression_ref > ()};}
#line 3928 "parser.cc"
    break;

  case 302: // aexp2: "_"
#line 1179 "parser.y"
                              {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::WildcardPattern()};}
#line 3934 "parser.cc"
    break;

  case 305: // texp: exp
#line 1188 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 3940 "parser.cc"
    break;

  case 306: // texp: infixexp qop
#line 1189 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < expression_ref > ()} )}; }
#line 3946 "parser.cc"
    break;

  case 307: // texp: qopm infixexp
#line 1190 "parser.y"
                      {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < expression_ref > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 3952 "parser.cc"
    break;

  case 308: // tup_exprs: tup_exprs "," texp
#line 1195 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3958 "parser.cc"
    break;

  case 309: // tup_exprs: texp "," texp
#line 1196 "parser.y"
                                 {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 3964 "parser.cc"
    break;

  case 310: // list: texp
#line 1214 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List({yystack_[0].value.as < Located<expression_ref> > ()}); }
#line 3970 "parser.cc"
    break;

  case 311: // list: lexps
#line 1215 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::List(yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 3976 "parser.cc"
    break;

  case 312: // list: texp ".."
#line 1216 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFrom(yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3982 "parser.cc"
    break;

  case 313: // list: texp "," exp ".."
#line 1217 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThen(yystack_[3].value.as < Located<expression_ref> > (),yystack_[1].value.as < Located<expression_ref> > ()); }
#line 3988 "parser.cc"
    break;

  case 314: // list: texp ".." exp
#line 1218 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromTo(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ()); }
#line 3994 "parser.cc"
    break;

  case 315: // list: texp "," exp ".." exp
#line 1219 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListFromThenTo(yystack_[4].value.as < Located<expression_ref> > (), yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < Located<expression_ref> > ()); }
#line 4000 "parser.cc"
    break;

  case 316: // list: texp "|" squals
#line 1220 "parser.y"
                                 { yylhs.value.as < expression_ref > () = Hs::ListComprehension(yystack_[2].value.as < Located<expression_ref> > (), yystack_[0].value.as < std::vector<Located<expression_ref>> > ()); }
#line 4006 "parser.cc"
    break;

  case 317: // lexps: lexps "," texp
#line 1222 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4012 "parser.cc"
    break;

  case 318: // lexps: texp "," texp
#line 1223 "parser.y"
                                 { yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[2].value.as < Located<expression_ref> > ()); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4018 "parser.cc"
    break;

  case 319: // squals: squals "," qual
#line 1236 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4024 "parser.cc"
    break;

  case 320: // squals: qual
#line 1238 "parser.y"
                                          {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4030 "parser.cc"
    break;

  case 321: // guardquals: guardquals1
#line 1248 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[0].value.as < std::vector<Located<expression_ref>> > ();}
#line 4036 "parser.cc"
    break;

  case 322: // guardquals1: guardquals1 "," qual
#line 1250 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > ();yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4042 "parser.cc"
    break;

  case 323: // guardquals1: qual
#line 1251 "parser.y"
                                   {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4048 "parser.cc"
    break;

  case 324: // altslist: "{" alts "}"
#line 1254 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4054 "parser.cc"
    break;

  case 325: // altslist: "vocurly" alts close
#line 1255 "parser.y"
                                 {yylhs.value.as < Hs::Alts > () = Hs::Alts{yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ()};}
#line 4060 "parser.cc"
    break;

  case 326: // altslist: "{" "}"
#line 1256 "parser.y"
                                 {}
#line 4066 "parser.cc"
    break;

  case 327: // altslist: "vocurly" close
#line 1257 "parser.y"
                                 {}
#line 4072 "parser.cc"
    break;

  case 328: // alts: alts1
#line 1259 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4078 "parser.cc"
    break;

  case 329: // alts: ";" alts
#line 1260 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[0].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4084 "parser.cc"
    break;

  case 330: // alts1: alts1 ";" alt
#line 1262 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[2].value.as < std::vector<Located<Hs::Alt>> > (); yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4090 "parser.cc"
    break;

  case 331: // alts1: alts1 ";"
#line 1263 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > () = yystack_[1].value.as < std::vector<Located<Hs::Alt>> > ();}
#line 4096 "parser.cc"
    break;

  case 332: // alts1: alt
#line 1264 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::Alt>> > ().push_back(yystack_[0].value.as < Located<Hs::Alt> > ());}
#line 4102 "parser.cc"
    break;

  case 333: // alt: pat alt_rhs
#line 1266 "parser.y"
                                 {yylhs.value.as < Located<Hs::Alt> > () = Located<Hs::Alt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4108 "parser.cc"
    break;

  case 334: // alt_rhs: "->" exp wherebinds
#line 1268 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Located<expression_ref> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4114 "parser.cc"
    break;

  case 335: // alt_rhs: gdpats wherebinds
#line 1269 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4120 "parser.cc"
    break;

  case 336: // gdpats: gdpats gdpat
#line 1271 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4126 "parser.cc"
    break;

  case 337: // gdpats: gdpat
#line 1272 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4132 "parser.cc"
    break;

  case 338: // gdpat: "|" guardquals "->" exp
#line 1281 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Located<expression_ref>> > (),yystack_[0].value.as < Located<expression_ref> > ()};}
#line 4138 "parser.cc"
    break;

  case 339: // pat: exp
#line 1283 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4144 "parser.cc"
    break;

  case 340: // bindpat: exp
#line 1285 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4150 "parser.cc"
    break;

  case 341: // apat: aexp
#line 1287 "parser.y"
              {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4156 "parser.cc"
    break;

  case 342: // apats1: apats1 apat
#line 1289 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4162 "parser.cc"
    break;

  case 343: // apats1: apat
#line 1290 "parser.y"
                    {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4168 "parser.cc"
    break;

  case 344: // stmtlist: "{" stmts "}"
#line 1293 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4174 "parser.cc"
    break;

  case 345: // stmtlist: "vocurly" stmts close
#line 1294 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Located<expression_ref>> > ()};}
#line 4180 "parser.cc"
    break;

  case 346: // stmts: stmts ";" stmt
#line 1296 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[2].value.as < std::vector<Located<expression_ref>> > (); yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4186 "parser.cc"
    break;

  case 347: // stmts: stmts ";"
#line 1297 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > () = yystack_[1].value.as < std::vector<Located<expression_ref>> > ();}
#line 4192 "parser.cc"
    break;

  case 348: // stmts: stmt
#line 1298 "parser.y"
                       {yylhs.value.as < std::vector<Located<expression_ref>> > ().push_back(yystack_[0].value.as < Located<expression_ref> > ());}
#line 4198 "parser.cc"
    break;

  case 349: // stmts: %empty
#line 1299 "parser.y"
                       {}
#line 4204 "parser.cc"
    break;

  case 350: // stmt: qual
#line 1304 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = yystack_[0].value.as < Located<expression_ref> > ();}
#line 4210 "parser.cc"
    break;

  case 351: // stmt: "rec" stmtlist
#line 1305 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 4216 "parser.cc"
    break;

  case 352: // qual: bindpat "<-" exp
#line 1307 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::PatQual(yystack_[2].value.as < Located<expression_ref> > (),yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4222 "parser.cc"
    break;

  case 353: // qual: exp
#line 1308 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Located<expression_ref> > ())};}
#line 4228 "parser.cc"
    break;

  case 354: // qual: "let" binds
#line 1309 "parser.y"
                        {yylhs.value.as < Located<expression_ref> > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 4234 "parser.cc"
    break;

  case 355: // fbinds: fbinds1
#line 1314 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 4240 "parser.cc"
    break;

  case 356: // fbinds: %empty
#line 1315 "parser.y"
                        {}
#line 4246 "parser.cc"
    break;

  case 357: // fbinds1: fbind "," fbinds1
#line 1317 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4252 "parser.cc"
    break;

  case 358: // fbinds1: fbind
#line 1318 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4258 "parser.cc"
    break;

  case 359: // fbinds1: ".."
#line 1319 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = true; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 4264 "parser.cc"
    break;

  case 360: // fbind: qvar "=" texp
#line 1321 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Located<expression_ref> > ())}};}
#line 4270 "parser.cc"
    break;

  case 361: // fbind: qvar
#line 1322 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 4276 "parser.cc"
    break;

  case 362: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1323 "parser.y"
                                                      {}
#line 4282 "parser.cc"
    break;

  case 363: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1324 "parser.y"
                                                      {}
#line 4288 "parser.cc"
    break;

  case 366: // qcon: gen_qcon
#line 1360 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4294 "parser.cc"
    break;

  case 367: // qcon: sysdcon
#line 1361 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4300 "parser.cc"
    break;

  case 368: // gen_qcon: qconid
#line 1363 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4306 "parser.cc"
    break;

  case 369: // gen_qcon: "(" qconsym ")"
#line 1364 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4312 "parser.cc"
    break;

  case 370: // con: conid
#line 1366 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4318 "parser.cc"
    break;

  case 371: // con: "(" consym ")"
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4324 "parser.cc"
    break;

  case 372: // con: sysdcon
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4330 "parser.cc"
    break;

  case 373: // con_list: con_list "," con
#line 1370 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4336 "parser.cc"
    break;

  case 374: // con_list: con
#line 1371 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4342 "parser.cc"
    break;

  case 375: // sysdcon_no_list: "(" ")"
#line 1373 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 4348 "parser.cc"
    break;

  case 376: // sysdcon_no_list: "(" commas ")"
#line 1374 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4354 "parser.cc"
    break;

  case 377: // sysdcon_no_list: "(#" "#)"
#line 1375 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 4360 "parser.cc"
    break;

  case 378: // sysdcon_no_list: "(#" commas "#)"
#line 1376 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4366 "parser.cc"
    break;

  case 379: // sysdcon: sysdcon_no_list
#line 1378 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4372 "parser.cc"
    break;

  case 380: // sysdcon: "[" "]"
#line 1379 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 4378 "parser.cc"
    break;

  case 381: // conop: consym
#line 1381 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4384 "parser.cc"
    break;

  case 382: // conop: "`" conid "`"
#line 1382 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4390 "parser.cc"
    break;

  case 383: // qconop: qconsym
#line 1384 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4396 "parser.cc"
    break;

  case 384: // qconop: "`" qconid "`"
#line 1385 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4402 "parser.cc"
    break;

  case 385: // gtycon: ntgtycon
#line 1388 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4408 "parser.cc"
    break;

  case 386: // gtycon: "(" ")"
#line 1389 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 4414 "parser.cc"
    break;

  case 387: // gtycon: "(#" "#)"
#line 1390 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 4420 "parser.cc"
    break;

  case 388: // ntgtycon: oqtycon
#line 1392 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4426 "parser.cc"
    break;

  case 389: // ntgtycon: "(" commas ")"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 4432 "parser.cc"
    break;

  case 390: // ntgtycon: "(#" commas "#)"
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 4438 "parser.cc"
    break;

  case 391: // ntgtycon: "(" "->" ")"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 4444 "parser.cc"
    break;

  case 392: // ntgtycon: "[" "]"
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 4450 "parser.cc"
    break;

  case 393: // oqtycon: qtycon
#line 1398 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4456 "parser.cc"
    break;

  case 394: // oqtycon: "(" qtyconsym ")"
#line 1399 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4462 "parser.cc"
    break;

  case 395: // oqtycon_no_varcon: qtycon
#line 1401 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4468 "parser.cc"
    break;

  case 396: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1402 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4474 "parser.cc"
    break;

  case 397: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1403 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4480 "parser.cc"
    break;

  case 398: // oqtycon_no_varcon: "(" ":" ")"
#line 1404 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 4486 "parser.cc"
    break;

  case 399: // qtyconop: qtyconsym
#line 1407 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4492 "parser.cc"
    break;

  case 400: // qtyconop: "`" qtycon "`"
#line 1408 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4498 "parser.cc"
    break;

  case 401: // qtycondoc: qtycon
#line 1410 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4504 "parser.cc"
    break;

  case 402: // qtycon: "QCONID"
#line 1412 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4510 "parser.cc"
    break;

  case 403: // qtycon: tycon
#line 1413 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4516 "parser.cc"
    break;

  case 404: // tycon: "CONID"
#line 1417 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4522 "parser.cc"
    break;

  case 405: // qtyconsym: "QCONSYM"
#line 1419 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4528 "parser.cc"
    break;

  case 406: // qtyconsym: "QVARSYM"
#line 1420 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4534 "parser.cc"
    break;

  case 407: // qtyconsym: tyconsym
#line 1421 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4540 "parser.cc"
    break;

  case 408: // tyconsym: "CONSYM"
#line 1423 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4546 "parser.cc"
    break;

  case 409: // tyconsym: "VARSYM"
#line 1424 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4552 "parser.cc"
    break;

  case 410: // tyconsym: ":"
#line 1425 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4558 "parser.cc"
    break;

  case 411: // tyconsym: "-"
#line 1426 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 4564 "parser.cc"
    break;

  case 412: // op: varop
#line 1431 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4570 "parser.cc"
    break;

  case 413: // op: conop
#line 1432 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4576 "parser.cc"
    break;

  case 414: // varop: varsym
#line 1434 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4582 "parser.cc"
    break;

  case 415: // varop: "`" varid "`"
#line 1435 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4588 "parser.cc"
    break;

  case 416: // qop: qvarop
#line 1437 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4594 "parser.cc"
    break;

  case 417: // qop: qconop
#line 1438 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4600 "parser.cc"
    break;

  case 418: // qopm: qvaropm
#line 1441 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 4606 "parser.cc"
    break;

  case 419: // qopm: qconop
#line 1442 "parser.y"
                { yylhs.value.as < expression_ref > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 4612 "parser.cc"
    break;

  case 420: // qvarop: qvarsym
#line 1447 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4618 "parser.cc"
    break;

  case 421: // qvarop: "`" qvarid "`"
#line 1448 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4624 "parser.cc"
    break;

  case 422: // qvaropm: qvarsym_no_minus
#line 1450 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 4630 "parser.cc"
    break;

  case 423: // qvaropm: "`" qvarid "`"
#line 1451 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4636 "parser.cc"
    break;

  case 424: // tyvar: tyvarid
#line 1455 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4642 "parser.cc"
    break;

  case 425: // tyvarop: "`" tyvarid "`"
#line 1457 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4648 "parser.cc"
    break;

  case 426: // tyvarid: "VARID"
#line 1459 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4654 "parser.cc"
    break;

  case 427: // tyvarid: special_id
#line 1460 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4660 "parser.cc"
    break;

  case 428: // tyvarid: "unsafe"
#line 1461 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 4666 "parser.cc"
    break;

  case 429: // tyvarid: "safe"
#line 1462 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 4672 "parser.cc"
    break;

  case 430: // tyvarid: "interruptible"
#line 1463 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 4678 "parser.cc"
    break;

  case 431: // var: varid
#line 1466 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4684 "parser.cc"
    break;

  case 432: // var: "(" varsym ")"
#line 1467 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4690 "parser.cc"
    break;

  case 433: // qvar: qvarid
#line 1469 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4696 "parser.cc"
    break;

  case 434: // qvar: "(" varsym ")"
#line 1470 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4702 "parser.cc"
    break;

  case 435: // qvar: "(" qvarsym1 ")"
#line 1471 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 4708 "parser.cc"
    break;

  case 436: // field: varid
#line 1473 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4714 "parser.cc"
    break;

  case 437: // qvarid: varid
#line 1475 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4720 "parser.cc"
    break;

  case 438: // qvarid: "QVARID"
#line 1476 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4726 "parser.cc"
    break;

  case 439: // varid: "VARID"
#line 1478 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4732 "parser.cc"
    break;

  case 440: // varid: special_id
#line 1479 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4738 "parser.cc"
    break;

  case 441: // varid: "unsafe"
#line 1480 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 4744 "parser.cc"
    break;

  case 442: // varid: "safe"
#line 1481 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 4750 "parser.cc"
    break;

  case 443: // varid: "interruptible"
#line 1482 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 4756 "parser.cc"
    break;

  case 444: // varid: "forall"
#line 1483 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 4762 "parser.cc"
    break;

  case 445: // varid: "family"
#line 1484 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 4768 "parser.cc"
    break;

  case 446: // varid: "role"
#line 1485 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 4774 "parser.cc"
    break;

  case 447: // qvarsym: varsym
#line 1487 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4780 "parser.cc"
    break;

  case 448: // qvarsym: qvarsym1
#line 1488 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4786 "parser.cc"
    break;

  case 449: // qvarsym_no_minus: varsym_no_minus
#line 1490 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4792 "parser.cc"
    break;

  case 450: // qvarsym_no_minus: qvarsym1
#line 1491 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4798 "parser.cc"
    break;

  case 451: // qvarsym1: "QVARSYM"
#line 1493 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4804 "parser.cc"
    break;

  case 452: // varsym: varsym_no_minus
#line 1495 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4810 "parser.cc"
    break;

  case 453: // varsym: "-"
#line 1496 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 4816 "parser.cc"
    break;

  case 454: // varsym_no_minus: "VARSYM"
#line 1498 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4822 "parser.cc"
    break;

  case 455: // varsym_no_minus: special_sym
#line 1499 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4828 "parser.cc"
    break;

  case 456: // special_id: "as"
#line 1501 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 4834 "parser.cc"
    break;

  case 457: // special_id: "qualified"
#line 1502 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 4840 "parser.cc"
    break;

  case 458: // special_id: "hiding"
#line 1503 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 4846 "parser.cc"
    break;

  case 459: // special_id: "export"
#line 1504 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 4852 "parser.cc"
    break;

  case 460: // special_id: "label"
#line 1505 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 4858 "parser.cc"
    break;

  case 461: // special_id: "dynamic"
#line 1506 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 4864 "parser.cc"
    break;

  case 462: // special_id: "stdcall"
#line 1507 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 4870 "parser.cc"
    break;

  case 463: // special_id: "ccall"
#line 1508 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 4876 "parser.cc"
    break;

  case 464: // special_id: "capi"
#line 1509 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 4882 "parser.cc"
    break;

  case 465: // special_id: "prim"
#line 1510 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 4888 "parser.cc"
    break;

  case 466: // special_id: "javascript"
#line 1511 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 4894 "parser.cc"
    break;

  case 467: // special_id: "group"
#line 1512 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 4900 "parser.cc"
    break;

  case 468: // special_id: "stock"
#line 1513 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 4906 "parser.cc"
    break;

  case 469: // special_id: "anyclass"
#line 1514 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4912 "parser.cc"
    break;

  case 470: // special_id: "via"
#line 1515 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4918 "parser.cc"
    break;

  case 471: // special_id: "unit"
#line 1516 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4924 "parser.cc"
    break;

  case 472: // special_id: "dependency"
#line 1517 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4930 "parser.cc"
    break;

  case 473: // special_id: "signature"
#line 1518 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4936 "parser.cc"
    break;

  case 474: // special_sym: "."
#line 1520 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4942 "parser.cc"
    break;

  case 475: // special_sym: "*"
#line 1521 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4948 "parser.cc"
    break;

  case 476: // qconid: conid
#line 1525 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4954 "parser.cc"
    break;

  case 477: // qconid: "QCONID"
#line 1526 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4960 "parser.cc"
    break;

  case 478: // conid: "CONID"
#line 1528 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4966 "parser.cc"
    break;

  case 479: // qconsym: consym
#line 1530 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4972 "parser.cc"
    break;

  case 480: // qconsym: "QCONSYM"
#line 1531 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4978 "parser.cc"
    break;

  case 481: // consym: "CONSYM"
#line 1533 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4984 "parser.cc"
    break;

  case 482: // consym: ":"
#line 1534 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4990 "parser.cc"
    break;

  case 483: // literal: "CHAR"
#line 1538 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char > ()});}
#line 4996 "parser.cc"
    break;

  case 484: // literal: "STRING"
#line 1539 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5002 "parser.cc"
    break;

  case 485: // literal: "INTEGER"
#line 1540 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5008 "parser.cc"
    break;

  case 486: // literal: "RATIONAL"
#line 1541 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::Double{yystack_[0].value.as < double > ()});}
#line 5014 "parser.cc"
    break;

  case 487: // literal: "PRIMINTEGER"
#line 1542 "parser.y"
                     {yylhs.value.as < expression_ref > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5020 "parser.cc"
    break;

  case 489: // close: error
#line 1550 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5026 "parser.cc"
    break;

  case 490: // modid: "CONID"
#line 1554 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5032 "parser.cc"
    break;

  case 491: // modid: "QCONID"
#line 1555 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5038 "parser.cc"
    break;

  case 492: // commas: commas ","
#line 1557 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5044 "parser.cc"
    break;

  case 493: // commas: ","
#line 1558 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5050 "parser.cc"
    break;


#line 5054 "parser.cc"

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


  const short parser::yypact_ninf_ = -639;

  const short parser::yytable_ninf_ = -453;

  const short
  parser::yypact_[] =
  {
      52,   277,  -639,   105,  -639,  -639,  -639,  -639,  -639,   409,
      13,    17,  -639,    70,    45,    45,    53,  -639,  -639,  -639,
    -639,    98,  -639,  -639,  -639,    21,  -639,   114,   168,  4339,
     208,   176,   155,  -639,   770,  -639,   -22,  -639,  -639,  -639,
    -639,   277,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,  1056,  -639,  -639,  -639,  -639,   178,
     162,  -639,   191,  -639,  -639,  -639,  -639,  -639,  -639,  -639,
     158,  -639,   277,  -639,   197,  -639,  2288,  4234,  -639,   223,
     272,  2288,  -639,  -639,  -639,   441,   280,  -639,  3396,   332,
     272,  2968,   248,  4578,   226,  2968,  2968,  2560,  2968,  1608,
    1472,   -17,  -639,  -639,  -639,  -639,  -639,  -639,  -639,    36,
     248,   217,   155,  -639,  -639,  -639,  -639,   294,   -11,  -639,
    -639,  1045,  -639,  2696,  -639,   219,  -639,  -639,  -639,  -639,
    -639,  -639,   283,     0,  -639,  -639,  -639,  -639,   290,  -639,
     313,  -639,  -639,  -639,  -639,   318,  -639,   321,   346,   370,
    -639,  -639,  -639,  4339,  4376,  -639,  -639,  -639,  -639,   474,
    -639,  1472,   422,   751,  -639,  -639,  -639,  4234,  4234,  -639,
    4677,  3497,  3080,   369,  -639,   465,   420,  -639,   269,  -639,
    3684,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  3785,  2016,
    2016,  -639,   399,   442,   444,   445,   447,  3785,  1200,  1200,
    -639,   511,  4234,  4234,   117,   459,   151,   119,   503,  -639,
    -639,   -25,  4578,  -639,   268,    -5,   437,   234,  -639,   130,
    -639,  -639,  -639,  -639,  2832,  -639,  2696,  -639,  -639,  -639,
     713,  -639,  -639,  -639,   751,     5,   438,   430,  -639,  2288,
    -639,  -639,  -639,  -639,  -639,  -639,  4714,  -639,  -639,   118,
     170,   186,   346,   440,   448,   449,   250,  -639,   314,  3785,
    4578,  4578,  -639,   405,   197,   417,  4234,  3785,  4677,  2288,
    2424,   713,  -639,    61,  -639,  -639,  2288,  -639,  -639,  -639,
    -639,  4234,  -639,  4714,  4541,  2968,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,   450,   457,   432,  -639,   467,    70,   277,
      18,   308,  3785,  -639,  -639,   249,   141,   468,   456,  -639,
    -639,  -639,  -639,   471,   480,   473,  -639,  -639,   469,  -639,
    -639,  -639,  -639,  -639,  -639,   470,   462,   475,  -639,   255,
     317,   333,  -639,  4234,  3785,  4477,  4234,  -639,  -639,  -639,
    4234,  -639,  -639,  -639,   477,   478,   280,   272,   499,   501,
      93,  -639,  -639,    32,  -639,   573,  -639,  -639,  -639,  -639,
    -639,  -639,   572,   122,  -639,  -639,  1045,    40,  2288,  -639,
     522,   406,  3785,    95,  3785,   476,   479,   498,   531,  -639,
     534,   504,   330,   226,   537,  2288,  -639,   496,   502,  2288,
    2288,  2424,  1744,  -639,  1744,   798,  -639,  -639,  4714,  -639,
    -639,  1744,  -639,  1744,   163,  -639,  -639,  -639,  -639,   542,
     541,   546,  4640,   509,  -639,  -639,  -639,  -639,  -639,    -1,
     424,  -639,  -639,  -639,  -639,   602,   551,   514,  -639,   517,
     280,  -639,  -639,  -639,  -639,  -639,  -639,   538,  -639,   529,
     568,   550,   553,  -639,  -639,  -639,  4440,  -639,  -639,  -639,
     540,  4339,  -639,  -639,  1880,  1336,  -639,  -639,   544,  3785,
    -639,  4677,  4770,  -639,  3785,  3785,  -639,  -639,  3785,  -639,
    -639,  -639,   928,   928,  -639,  -639,  -639,   539,   547,   414,
    -639,  -639,  3785,   511,  -639,  2288,  -639,  2016,  -639,  2288,
     342,  -639,  -639,  1200,  -639,  -639,  3785,  3785,  4883,   574,
    -639,  -639,    88,  -639,  -639,  4677,   552,  -639,  -639,  -639,
    -639,   563,   920,   341,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,  -639,   554,  -639,   582,  -639,  -639,  -639,  -639,  -639,
    -639,  3785,  3785,   555,   556,   405,  -639,   600,  3785,   650,
     652,   671,  -639,  2288,  2424,  -639,  -639,  -639,  4541,  1744,
    4714,  -639,  4339,   569,  -639,  2152,  -639,   579,   567,  -639,
     365,    70,  -639,  -639,  -639,  -639,  3785,  4969,  4969,  -639,
    -639,  -639,  -639,  -639,   577,   649,  3598,  -639,  -639,   123,
    -639,    48,  -639,  -639,  -639,  -639,  -639,   399,  1064,  1064,
    -639,  -639,  -639,  -639,  -639,  4969,   660,   611,  -639,  -639,
    -639,  2424,  2288,  -639,    -2,     4,  -639,  -639,  -639,  -639,
    -639,  -639,   609,  -639,  4234,   358,   671,   142,  -639,   671,
    -639,  -639,  -639,  -639,  -639,     3,  -639,   584,  -639,  -639,
    -639,  2288,  2424,  2288,  -639,    65,  -639,  -639,  -639,   146,
     615,  -639,  -639,  4234,  4234,  4234,  -639,   451,  -639,   928,
    -639,   687,   682,  -639,  -639,   143,  -639,    62,  -639,   616,
     367,  -639,  3785,  -639,  -639,  -639,  3785,  -639,  4807,   650,
     612,  3186,  -639,  -639,  -639,   399,   399,  -639,  -639,  -639,
    -639,  3871,   231,   653,  -639,  -639,  -639,  1744,  4714,  -639,
    -639,   619,   602,  -639,  -639,  3785,  -639,  3785,   627,  -639,
     455,  3785,  3976,  -639,  -639,  2288,  -639,  4234,  -639,  1064,
    -639,  4969,  4062,  4148,  -639,  -639,  -639,  -639,  -639,  4234,
     585,  -639,  4234,   150,  -639,   226,    66,  -639,  -639,   590,
     597,  -639,  4234,  -639,  -639,  -639,  2288,  -639,   610,   601,
    3785,  -639,  4916,  -639,  -639,  3080,   635,   636,  -639,  -639,
    -639,  4969,  -639,   618,   239,  -639,    70,    69,  3291,  -639,
    4234,  -639,   399,   173,  -639,  4234,  -639,  -639,  -639,  -639,
    -639,  -639,   615,  4969,  -639,  -639,  -639,  4234,  -639,  -639,
    -639,  3785,  -639,  -639,  -639,  -639
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    36,     0,     2,    36,     4,   490,   491,     8,
       0,    39,     1,     0,     0,     0,    18,    11,    35,    13,
      16,    58,   489,   488,    12,   162,   158,     0,     0,     0,
       0,    42,    37,    15,    14,   161,     0,     6,     7,   456,
     458,     0,   457,   444,   459,   460,   461,   442,   443,   441,
     445,   446,   462,   463,   464,   465,   466,   467,   468,   469,
     470,   471,   473,   472,     0,   439,   404,   438,   402,     0,
      19,    21,    24,    32,   395,   403,    31,   433,   437,   440,
       0,    41,     0,    34,    38,   302,     0,     0,   112,     0,
       0,     0,    51,    52,    53,    81,     0,   113,     0,     0,
       0,     0,   266,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   478,   477,   483,   484,   485,   486,   487,   266,
     266,    49,    56,    59,    60,    61,    62,   128,     0,    65,
     249,    66,   272,   275,   281,   291,   294,   296,   366,   379,
     367,   168,   295,   437,   368,   476,   297,   159,     0,    23,
       0,   453,   475,   474,   454,     0,   451,     0,     0,     0,
     452,   455,    17,     0,    27,    22,    36,    36,     3,    44,
      33,     0,     0,   271,   429,   430,   428,     0,     0,   196,
     234,     0,     0,     0,   426,   138,     0,   125,   179,   182,
     183,   187,   194,   388,   393,   195,   424,   427,   208,   349,
     349,   289,   277,     0,     0,     0,     0,     0,   152,   152,
     155,     0,     0,     0,     0,     0,   179,   388,     0,   290,
     280,     0,     0,   267,     0,     0,     0,     0,   374,   163,
     372,   370,   341,   343,     0,   283,   274,   284,   482,   380,
       0,   481,   480,   305,   271,   310,     0,   311,   419,     0,
     418,   422,   450,   449,   383,   479,     0,   375,   493,     0,
       0,     0,   450,     0,   449,   383,     0,   377,     0,     0,
       0,     0,    50,     0,    57,     0,     0,     0,     0,     0,
       0,     0,   250,   157,   255,   417,     0,   416,   420,   448,
     447,     0,   278,     0,   356,     0,   160,   398,   397,   396,
     435,   434,    20,     0,     0,    28,    30,     0,     0,     0,
      46,     0,     0,   198,   197,     0,     0,     0,   235,   237,
     431,   212,   392,     0,   171,     0,   175,   410,     0,   411,
     200,   409,   408,   406,   405,   209,     0,     0,   407,     0,
       0,     0,    67,     0,     0,     0,     0,   191,   399,   192,
       0,   188,   190,   209,     0,   207,     0,     0,   353,     0,
       0,   348,   350,     0,   276,     0,    78,    77,    79,    80,
     204,   165,   148,     0,   251,   151,     0,     0,     0,    76,
       0,   118,     0,     0,     0,     0,     0,     0,     0,   261,
       0,     0,     0,     0,     0,     0,   342,     0,     0,   306,
     312,     0,     0,   301,     0,   307,   304,   436,     0,   300,
     298,     0,   299,     0,   434,   369,   376,   492,   378,     0,
       0,     0,     0,   258,   413,    55,   412,   414,   381,     0,
     114,   257,   176,   166,   167,   157,     0,   321,   323,     0,
       0,   253,   254,   273,   279,   293,   359,     0,   355,   358,
     361,     0,   437,   282,    26,    25,     0,     9,    10,    43,
       0,     0,    40,    45,     0,     0,   288,   270,     0,     0,
     199,     0,     0,   202,     0,     0,   391,   203,     0,   394,
     389,   390,   134,   134,   137,   124,   180,     0,     0,   184,
     189,    63,     0,   354,   351,     0,   344,   347,   345,     0,
       0,    75,   153,   150,   154,   286,     0,     0,     0,    86,
     218,    72,     0,    73,    68,     0,     0,   268,   260,   262,
     371,     0,     0,     0,   164,   385,   373,   259,   285,   423,
     384,   314,   316,   320,   305,   318,   317,   303,   309,   308,
     265,     0,     0,     0,     0,     0,   127,     0,     0,   231,
     221,   239,   252,     0,     0,   421,   156,   292,     0,     0,
       0,    29,     0,     0,   326,     0,   339,     0,   328,   332,
       0,     0,   327,   432,   238,   236,     0,     0,     0,   211,
     213,   216,   172,   174,   210,   107,     0,   129,   133,     0,
     130,     0,   400,   425,   210,   352,   346,   277,   144,   144,
     147,   149,   101,   119,   120,     0,    91,     0,   269,   386,
     387,     0,   313,   169,     0,     0,   415,   382,    54,   126,
     115,   212,   225,   227,     0,     0,   239,     0,    69,   240,
     242,   256,   322,   357,   360,   363,   365,     0,    47,   329,
     324,   331,     0,     0,   333,   157,   337,   325,   173,     0,
       0,   201,   108,     0,     0,     0,   105,   121,   135,   132,
     136,     0,   109,   139,   143,     0,   140,     0,    87,     0,
       0,    71,     0,   319,   315,   263,     0,   264,     0,   231,
       0,   232,   185,   193,   229,   277,   277,    70,    84,    82,
      83,     0,     0,   243,   246,   401,   241,     0,     0,    48,
     330,     0,   157,   335,   336,     0,   214,     0,   116,   106,
     121,     0,     0,   103,   131,     0,   110,     0,   145,   142,
     146,     0,   100,   100,    92,    64,   170,   230,   226,     0,
       0,   186,     0,     0,   223,     0,     0,   247,   181,   205,
       0,   244,     0,   245,   362,   364,     0,   334,     0,     0,
       0,   102,     0,   104,   122,     0,     0,   195,   287,   111,
     141,    88,    90,     0,     0,    99,     0,     0,   232,   228,
     233,   219,   277,     0,   220,     0,   248,    85,   338,   215,
     217,   117,   195,     0,    89,    95,    93,    98,    96,    94,
     222,     0,   206,   123,    97,   224
  };

  const short
  parser::yypgoto_[] =
  {
    -639,  -639,  -639,  -639,  -639,  -639,  -639,    42,  -639,  -639,
    -421,  -639,   561,  -639,  -639,  -639,  -148,   604,  -639,  -639,
    -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,   -56,  -639,  -639,  -639,     6,  -211,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,  -639,    20,   452,  -639,    72,   253,
    -639,  -639,    14,   133,  -639,  -639,   530,  -639,  -292,  -406,
     723,  -639,  -639,  -309,    68,  -157,   200,  -142,   243,  -639,
     -61,  -639,   -53,  -639,   -76,  -639,  -323,  -639,  -639,  -639,
    -585,  -158,   485,   -14,  -639,   560,   138,   254,  -638,  -328,
    -639,    77,     9,  -639,  -639,    91,  -639,    43,  -639,  -639,
     300,   153,  -639,   164,    99,   761,  -171,  -639,  -639,   528,
    -639,   375,  -639,   -84,    -3,  -259,  -190,   690,   -48,  -639,
    -639,  -639,   -74,  -639,  -639,  -639,  -639,   174,  -639,  -639,
    -403,  -639,   177,  -639,  -639,   183,  -639,  -639,   581,  -639,
     -57,   629,   335,  -270,  -639,   276,  -639,  -639,  -639,  -639,
     458,   102,  -639,   -95,  -630,   -72,  -639,   446,   -77,  -639,
    -639,  -639,   -21,  -639,  -164,  -639,   296,  -639,   598,  -639,
    -639,  -639,  -426,  -639,  -331,  -230,     7,  -251,  -154,    -6,
    -639,  -639,   -10,   -49,   -92,   -87,  -639,  -147,   -91,   -42,
    -223,  -639,  -264,   -16,  -105
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   168,     6,    10,    19,    30,
      69,    70,    71,   165,   304,   305,    72,    84,    11,    20,
      21,    32,    82,   310,   462,   463,   273,   121,   423,    33,
      34,   122,   123,   124,   125,   214,   126,   207,   692,   743,
     606,   668,   761,   671,   724,   764,   765,   587,   653,   717,
     663,   127,   550,   751,   509,   713,   185,   276,   588,   589,
     484,   342,   664,   665,   600,   501,   373,   210,   211,   441,
      27,    36,   394,   370,   431,   128,   614,   335,   510,   433,
     325,   680,   326,   739,   188,   189,   681,   190,   351,   346,
     682,   191,   372,   740,   354,   336,   472,   579,   580,   511,
     626,   733,   734,   551,   622,   623,   624,   684,   317,   318,
     319,   628,   629,   630,   693,   374,   590,   282,   283,   284,
     130,   222,   223,   243,   173,   132,   735,   133,   134,   135,
     136,   259,   260,   261,   246,   247,   532,   436,   437,   466,
     567,   568,   569,   644,   645,   646,   570,   359,   233,   234,
     201,   360,   361,   362,   447,   448,   449,   635,   137,   138,
     228,   229,   139,   140,   424,   248,   524,   192,   193,    73,
     347,   694,   194,    75,   337,   338,   425,   426,   286,   249,
     287,   250,   195,   349,   196,   141,   142,   451,    77,    78,
     288,   251,   252,   290,   160,    79,   161,   144,   145,   254,
     255,   146,    24,     9,   266
  };

  const short
  parser::yytable_[] =
  {
     197,   379,   172,   467,   391,   406,   268,   202,    74,   230,
     438,   197,   365,   231,   488,   159,   306,   253,   264,   313,
     314,   217,   216,   316,   348,   149,   186,   443,   143,   552,
     460,   131,   352,    22,   187,   245,    76,   375,   375,   323,
     563,    22,   445,   219,   458,   215,   581,    13,   434,    22,
     428,   732,   348,   220,   158,   269,   353,   232,   235,   285,
     237,   263,   571,    22,   493,   386,   169,    22,   265,   277,
      22,    22,   389,     1,   756,   675,   546,   339,   340,   264,
    -431,   677,   581,   400,   697,   292,   397,   147,   440,   401,
     197,   197,   440,   398,   197,   197,   731,   148,   267,   498,
     262,   285,   258,   197,   698,    12,   244,   244,   278,   387,
     225,   197,    31,   504,   756,   358,   358,   676,    17,  -431,
     197,   289,   159,   676,   402,   197,   197,   439,   390,   265,
     461,   533,   547,   444,   398,    18,   216,   216,   732,    23,
     443,   637,    74,    74,   221,   280,   582,    23,   556,   642,
     497,   649,   650,    25,    35,    23,     2,   537,   503,   380,
     381,   262,   639,   289,   688,    29,   659,   327,   244,    23,
      76,    76,   285,    23,   320,   159,    23,    23,    26,   603,
     719,   329,   197,   731,   772,   731,   232,   787,   292,   197,
     197,    37,   490,   689,   690,   435,   358,   382,   496,   -74,
      81,   572,   143,   143,   197,   376,   376,   512,   307,   308,
     392,   497,   331,   332,   158,   186,   333,   334,    66,   408,
     620,   469,    68,   187,   427,   197,   705,   502,   658,   388,
     327,   409,   613,   613,   289,    80,   383,   344,   -74,   703,
     503,   659,   514,  -432,   329,    38,   405,   453,   718,   393,
     407,   706,   581,   791,   691,   771,   197,   197,   197,   197,
     278,   719,   166,   197,   167,    66,   468,   216,   772,    68,
     489,   345,   320,    83,   669,   331,   332,   420,   421,   333,
     334,   163,  -432,   410,   632,   607,   757,   407,   452,   411,
     485,   162,   393,   459,   505,   197,   747,   197,   230,   412,
     494,   450,   231,   164,   285,   413,   513,   647,   561,   636,
     253,   528,   253,   238,   316,   170,   531,   358,   534,   253,
     293,   253,   428,   294,   487,   348,   581,   660,   535,   782,
     536,   544,   601,   285,   226,   198,   584,   538,   227,   539,
     111,   673,   151,   691,   786,   152,   218,   257,   327,   112,
     594,   272,   153,   258,    66,   344,   221,   787,    68,   241,
    -177,   151,   329,   416,   152,   275,   289,   726,   480,   417,
     295,   153,   438,   154,   417,   656,   199,   748,   200,   749,
     566,   566,   197,   754,   208,   197,   209,   197,   197,   345,
     762,   197,   154,   331,   332,   289,   156,   333,   334,   244,
       7,   244,   407,   720,     8,   197,   768,   661,   244,   770,
     244,   595,   464,   358,   465,   597,   543,   339,   340,   197,
     197,   197,   781,   296,   324,   324,   297,   666,   666,   418,
     784,   298,   481,   417,   299,    74,   417,   482,   521,   483,
      74,   324,   522,   709,   523,   311,   598,   745,   599,   642,
     371,   643,   669,    66,   197,   197,   610,    68,   428,   300,
     258,   197,   685,    76,   686,   320,   683,   253,    76,   631,
     358,   722,   774,   723,    14,    15,   143,   143,   309,   376,
     376,   566,   795,   301,   238,   634,   507,   508,   258,   197,
     197,   197,   341,   327,   270,   271,   427,   143,   151,   197,
     376,   152,   788,   789,   548,   549,   759,   329,   153,   320,
     216,   343,   371,   203,   204,   205,   206,   364,   197,   366,
     432,   367,   368,   683,   369,   422,   378,   358,   674,   154,
     241,   711,   712,   657,   345,   711,   752,   197,   331,   332,
     384,    74,   333,   334,   385,   428,   239,   403,   666,   404,
     429,   456,   452,   414,   407,   371,   244,   566,   358,   702,
     474,  -452,   415,   454,   475,   450,   197,   197,   197,    76,
     455,   683,   457,   470,   683,   471,   794,   216,   216,   216,
     473,   478,   476,   477,  -340,   197,   495,   486,   479,   197,
     491,   197,   143,   143,   197,   376,   376,   492,   499,   500,
     708,   380,   710,   506,   197,   253,   695,   517,   518,   515,
     683,   519,   683,   516,   527,   216,   529,   520,   197,   540,
     197,   541,   530,   744,   197,   197,   542,   324,   545,   440,
     197,   758,   553,   554,   197,   197,   197,   555,   738,   544,
     230,   216,   197,   557,   231,   197,   216,   216,   558,   559,
     339,   560,   562,   143,  -436,   197,   376,   573,   605,   592,
     612,   608,   778,   197,   380,   197,   216,   593,   197,   380,
     380,   695,   322,   611,   197,   616,   617,   619,   621,   625,
     627,   197,   638,   197,   640,   641,   652,   670,   197,   777,
     651,   672,   407,   679,   244,   707,   197,   699,   715,   216,
     197,   716,   721,   729,   197,   746,   742,   750,   112,   775,
     776,   216,   574,   143,   780,   779,   376,    39,   583,   783,
    -216,   324,   738,   785,   302,    40,   274,   793,   430,   767,
     753,   714,   667,   760,   380,   324,   591,    42,    28,   377,
     725,    43,   615,    44,    45,    46,    47,    48,    49,   602,
      50,    51,    52,    53,   419,    54,    55,    56,   355,   678,
      57,   792,   604,   736,    58,    59,    60,    61,    62,    63,
     728,   575,   769,    85,    39,    86,    87,    88,    89,   687,
      90,   790,    40,    91,   371,   371,    92,    93,    94,    95,
      96,   741,    97,   696,    42,   129,    98,   236,    43,    99,
      44,    45,    46,    47,    48,    49,   100,    50,    51,    52,
      53,   442,    54,    55,    56,   396,   701,    57,   700,   648,
     101,    58,    59,    60,    61,    62,    63,   102,   704,   363,
     238,   312,   596,   103,   633,    65,   112,   773,   525,    67,
     113,   618,   399,     0,   151,     0,   104,   152,     0,     0,
       0,   526,   105,     0,   153,     0,     0,     0,     0,   106,
       0,     0,   107,     0,   108,     0,     0,     0,     0,     0,
       0,   281,     0,     0,     0,   154,   241,   238,   109,   156,
     242,     0,   110,     0,   111,     0,     0,     0,     0,     0,
       0,   151,    65,   112,   152,     0,    67,   113,     0,     0,
       0,   153,   114,   115,   116,   117,     0,     0,   118,     0,
       0,     0,   119,   120,     0,   432,     0,     0,   281,   371,
       0,     0,   154,   241,     0,     0,   156,   242,     0,     0,
       0,    85,    39,    86,     0,   585,     0,     0,    90,     0,
      40,    91,     0,     0,    92,    93,    94,     0,    96,     0,
       0,     0,    42,     0,   586,   602,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,   102,     0,     0,     0,     0,
       0,   103,     0,     0,     0,     0,     0,     0,   324,   327,
       0,     0,     0,     0,   104,     0,   328,     0,     0,     0,
     105,     0,     0,   329,     0,     0,     0,   106,     0,     0,
     107,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   609,   371,     0,   109,     0,     0,   258,
     110,     0,   111,     0,   331,   332,     0,     0,   333,   334,
      65,   112,     0,     0,    67,   113,     0,     0,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
     119,   120,     0,     0,    90,     0,    40,    91,     0,     0,
      92,    93,    94,     0,    96,     0,     0,     0,    42,     0,
     662,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,   102,     0,     0,   238,     0,   279,   103,     0,   280,
       0,     0,     0,     0,     0,   150,     0,     0,   151,     0,
     104,   152,     0,     0,     0,     0,   105,     0,   153,   151,
       0,     0,   152,   106,     0,     0,   107,     0,   108,   153,
       0,     0,     0,     0,     0,   281,     0,     0,     0,   154,
     241,     0,   109,   156,   242,     0,   110,     0,   111,     0,
     154,   155,     0,     0,   156,   157,    65,   112,     0,     0,
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
     111,     0,     0,     0,   565,     0,     0,     0,    65,   112,
       0,     0,    67,   113,     0,     0,     0,     0,   114,   115,
     116,   117,     0,     0,   118,    85,    39,    86,     0,     0,
       0,     0,    90,     0,    40,    91,     0,     0,     0,     0,
       0,     0,    96,     0,     0,     0,    42,     0,     0,     0,
      43,     0,    44,    45,    46,    47,    48,    49,   100,    50,
      51,    52,    53,     0,    54,    55,    56,     0,     0,    57,
       0,     0,   101,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   238,     0,     0,   105,     0,     0,     0,     0,     0,
       0,   106,     0,     0,   107,   151,   108,     0,   152,     0,
       0,     0,     0,     0,   256,   153,     0,     0,     0,     0,
     109,     0,     0,     0,   171,   257,   111,     0,     0,     0,
       0,   258,   240,     0,    65,   112,   154,   241,    67,   113,
     156,   242,     0,     0,   114,   115,   116,   117,     0,     0,
     118,    85,    39,    86,     0,     0,     0,     0,    90,     0,
      40,    91,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,    42,     0,     0,     0,    43,     0,    44,    45,
      46,    47,    48,    49,   100,    50,    51,    52,    53,     0,
      54,    55,    56,     0,     0,    57,     0,     0,   101,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   238,     0,     0,
     105,     0,     0,     0,     0,     0,     0,   106,     0,     0,
     107,     0,   108,     0,   152,     0,     0,     0,     0,     0,
       0,   153,     0,     0,     0,     0,   109,   239,     0,     0,
     171,     0,   111,     0,     0,     0,     0,     0,   240,     0,
      65,   112,   154,   241,    67,   113,   156,   242,     0,     0,
     114,   115,   116,   117,     0,     0,   118,    85,    39,    86,
       0,     0,     0,     0,    90,     0,    40,    91,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
     100,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,     0,     0,   101,    58,    59,    60,    61,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   238,     0,     0,   105,     0,     0,     0,
       0,     0,     0,   106,     0,     0,   107,     0,   108,     0,
     152,     0,     0,     0,     0,     0,     0,   153,     0,     0,
       0,     0,   109,     0,     0,     0,   171,     0,   111,     0,
       0,     0,     0,     0,   240,     0,    65,   112,   154,   241,
      67,   113,   156,   242,     0,     0,   114,   115,   116,   117,
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
       0,     0,     0,     0,     0,   564,     0,     0,   109,     0,
       0,     0,   171,     0,   111,     0,     0,     0,   565,     0,
       0,     0,    65,   112,     0,     0,    67,   113,     0,     0,
       0,     0,   114,   115,   116,   117,     0,     0,   118,    85,
      39,    86,     0,     0,     0,     0,    90,     0,    40,    91,
       0,     0,     0,     0,     0,     0,   356,     0,     0,     0,
      42,     0,     0,     0,    43,     0,    44,    45,    46,    47,
      48,    49,   100,    50,    51,    52,    53,     0,    54,    55,
      56,     0,   357,    57,     0,     0,   101,    58,    59,    60,
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
     565,     0,     0,     0,    65,   112,     0,     0,    67,   113,
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
       0,     0,     0,     0,   356,     0,     0,     0,    42,     0,
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
       0,     0,     0,     0,   291,   106,     0,     0,     0,     0,
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
       0,     0,     0,     0,   105,     0,     0,     0,   395,     0,
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
     114,   115,   116,   117,    42,     0,   118,     0,   321,     0,
      44,    45,    46,   174,   175,   176,     0,     0,     0,    52,
      53,     0,    54,    55,    56,     0,     0,    57,     0,     0,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   327,
       0,     0,     0,     0,     0,     0,   328,     0,     0,   177,
       0,     0,     0,   329,   178,     0,   179,     0,     0,     0,
       0,     0,     0,     0,   180,     0,     0,     0,   181,     0,
      39,     0,   182,   330,   183,     0,     0,     0,    40,   258,
       0,     0,   184,    66,   331,   332,     0,    68,   333,   334,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   238,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,  -178,     0,     0,
     178,     0,   179,     0,     0,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   181,    39,     0,     0,   182,     0,
     183,     0,     0,    40,     0,     0,   730,     0,   184,    66,
       0,   241,     0,    68,     0,    42,     0,     0,     0,     0,
       0,    44,    45,    46,   174,   175,   176,     0,     0,     0,
      52,    53,     0,    54,    55,    56,     0,     0,    57,     0,
       0,     0,    58,    59,    60,    61,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     238,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,   178,     0,   179,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,   181,
      39,     0,     0,   182,     0,   183,     0,     0,    40,     0,
       0,   730,     0,   184,    66,   212,   241,     0,    68,     0,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,   213,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,     0,     0,     0,     0,
     178,     0,   179,     0,     0,     0,     0,     0,     0,     0,
     180,    39,     0,     0,   181,     0,     0,     0,   182,    40,
     183,     0,     0,     0,     0,     0,     0,     0,   184,    66,
       0,    42,     0,    68,     0,   321,     0,    44,    45,    46,
     174,   175,   176,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,     0,     0,     0,
       0,   178,     0,   179,     0,     0,     0,     0,     0,     0,
       0,   180,    39,     0,     0,   181,   322,     0,     0,   182,
      40,   183,     0,     0,     0,     0,     0,   654,     0,   184,
      66,     0,    42,     0,    68,     0,     0,     0,    44,    45,
      46,   174,   175,   176,     0,   655,     0,    52,    53,     0,
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
       0,     0,   350,   177,     0,     0,     0,     0,   178,     0,
     179,     0,     0,     0,     0,     0,     0,     0,   180,    39,
       0,     0,   181,     0,     0,     0,   182,    40,   183,     0,
       0,     0,     0,     0,     0,     0,   184,    66,     0,    42,
       0,    68,     0,   321,     0,    44,    45,    46,   174,   175,
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
      39,     0,     0,   182,   737,   183,     0,     0,    40,     0,
       0,     0,     0,   184,    66,     0,     0,     0,    68,     0,
      42,     0,     0,     0,   321,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   177,    39,     0,     0,     0,
     178,     0,   179,     0,    40,     0,     0,     0,     0,     0,
     180,     0,     0,     0,   181,     0,    42,     0,   755,     0,
     183,     0,    44,    45,    46,   174,   175,   176,   184,    66,
       0,    52,    53,    68,    54,    55,    56,     0,     0,    57,
       0,     0,     0,    58,    59,    60,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     763,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   177,    39,     0,     0,     0,   178,     0,   179,     0,
      40,     0,     0,     0,     0,     0,   180,     0,     0,     0,
     181,     0,    42,     0,   182,     0,   183,     0,    44,    45,
      46,   174,   175,   176,   184,    66,     0,    52,    53,    68,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   766,     0,     0,     0,
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
       0,    64,    40,     0,   303,     0,     0,     0,     0,     0,
       0,    65,    66,     0,    42,    67,    68,     0,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    39,    54,    55,    56,     0,     0,    57,    64,    40,
       0,    58,    59,    60,    61,    62,    63,     0,    65,    66,
       0,    42,    67,    68,     0,     0,     0,    44,    45,    46,
     174,   175,   176,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,    64,    40,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,     0,    42,    67,    68,     0,    43,
       0,    44,    45,    46,    47,    48,    49,     0,    50,    51,
      52,    53,    39,    54,    55,    56,     0,     0,    57,     0,
      40,     0,    58,    59,    60,    61,    62,    63,     0,   184,
      66,     0,    42,     0,    68,     0,    43,     0,    44,    45,
      46,    47,    48,    49,     0,    50,    51,    52,    53,   446,
      54,    55,    56,     0,     0,    57,     0,     0,     0,    58,
      59,    60,    61,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    40,   224,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    42,     0,     0,    67,    43,     0,
      44,    45,    46,    47,    48,    49,     0,    50,    51,    52,
      53,    39,    54,    55,    56,     0,     0,    57,     0,    40,
     224,    58,    59,    60,    61,    62,    63,     0,     0,     0,
      65,    42,     0,     0,    67,    43,     0,    44,    45,    46,
      47,    48,    49,     0,    50,    51,    52,    53,    39,    54,
      55,    56,     0,     0,    57,     0,    40,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,    42,     0,
       0,     0,    43,     0,    44,    45,    46,    47,    48,    49,
       0,    50,    51,    52,    53,     0,    54,    55,    56,     0,
       0,    57,    65,   112,     0,    58,    59,    60,    61,    62,
      63,     0,     0,     0,    39,     0,     0,     0,     0,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,   315,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    65,
      44,    45,    46,   174,   175,   176,     0,     0,     0,    52,
      53,    39,    54,    55,    56,     0,     0,    57,     0,    40,
       0,    58,    59,    60,    61,    62,    63,     0,     0,     0,
       0,    42,     0,     0,     0,     0,    65,    44,    45,    46,
     174,   175,   176,     0,     0,     0,    52,    53,     0,    54,
      55,    56,     0,     0,    57,     0,     0,     0,    58,    59,
      60,    61,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   576,   577,     0,     0,     0,     0,     0,
       0,     0,   578,     0,     0,     0,     0,    39,     0,     0,
       0,     0,   184,     0,     0,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,     0,
     727,   577,     0,    44,    45,    46,   174,   175,   176,   578,
      39,     0,    52,    53,     0,    54,    55,    56,    40,   184,
      57,     0,     0,     0,    58,    59,    60,    61,    62,    63,
      42,     0,     0,     0,     0,     0,    44,    45,    46,   174,
     175,   176,     0,     0,     0,    52,    53,     0,    54,    55,
      56,     0,     0,    57,     0,     0,     0,    58,    59,    60,
      61,    62,    63,    39,     0,     0,     0,     0,     0,     0,
       0,    40,     0,     0,     0,     0,     0,   577,     0,     0,
       0,     0,     0,    42,     0,   578,     0,     0,     0,    44,
      45,    46,   174,   175,   176,   184,     0,     0,    52,    53,
       0,    54,    55,    56,     0,     0,    57,     0,     0,     0,
      58,    59,    60,    61,    62,    63,     0,     0,   578,     0,
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
      87,   212,    86,   312,   227,   256,   111,    91,    29,   104,
     280,    98,   202,   104,   345,    64,   164,   109,   110,   177,
     178,    98,    98,   180,   188,    41,    87,   286,    34,   435,
      12,    34,   190,     1,    87,   109,    29,   208,   209,   181,
     461,     1,   293,   100,   308,    98,   472,     5,   278,     1,
     273,   681,   216,   101,    64,    19,   198,   105,   106,   131,
     108,   110,   465,     1,   356,    90,    82,     1,   110,    80,
       1,     1,    77,    21,   712,    77,    77,   182,   183,   171,
      80,    77,   508,    78,    81,   133,   240,   109,    27,    84,
     177,   178,    27,   240,   181,   182,   681,   119,   115,   363,
     110,   173,   119,   190,   101,     0,   109,   110,   119,   134,
     103,   198,    14,   377,   752,   199,   200,   119,   105,   119,
     207,   131,   171,   119,   119,   212,   213,   281,   133,   171,
     112,   401,   133,   291,   281,   118,   212,   213,   768,   107,
     399,   562,   163,   164,   108,    84,   474,   107,   440,    84,
     118,   577,   578,   108,   133,   107,   104,   408,   118,   212,
     213,   171,   565,   173,    22,   112,   118,    79,   171,   107,
     163,   164,   244,   107,   180,   224,   107,   107,   133,   507,
     118,    93,   269,   768,   118,   770,   234,   118,   236,   276,
     277,    77,   350,    51,    52,   279,   280,    80,   105,    80,
      24,   465,   208,   209,   291,   208,   209,   112,   166,   167,
      80,   118,   124,   125,   224,   276,   128,   129,   123,   101,
     548,    80,   127,   276,   273,   312,    80,   105,   105,   222,
      79,   113,   541,   542,   244,    27,   119,    86,   119,   645,
     118,   118,   384,    80,    93,    77,   249,   295,   105,   119,
     256,   105,   678,    80,   112,   105,   343,   344,   345,   346,
     119,   118,   104,   350,   106,   123,   315,   343,   118,   127,
     346,   120,   278,   118,   605,   124,   125,   270,   271,   128,
     129,   119,   119,   113,   554,   515,   712,   293,   294,   119,
     343,   113,   119,   309,   378,   382,   702,   384,   393,   113,
     357,   294,   393,   112,   376,   119,   383,   571,   456,   560,
     402,   395,   404,    79,   471,   118,   400,   401,   402,   411,
     101,   413,   545,   104,   345,   489,   752,   591,   402,   755,
     404,   422,   503,   405,   108,   112,   478,   411,   112,   413,
     114,   611,    93,   112,   105,    96,    14,   113,    79,   123,
     492,   134,   103,   119,   123,    86,   108,   118,   127,   125,
      91,    93,    93,   113,    96,    71,   376,   676,   113,   119,
      87,   103,   642,   124,   119,   586,   104,   705,   106,   707,
     464,   465,   469,   711,   104,   472,   106,   474,   475,   120,
     721,   478,   124,   124,   125,   405,   128,   128,   129,   402,
     123,   404,   408,   667,   127,   492,   729,   597,   411,   732,
     413,   495,   104,   497,   106,   499,   422,   522,   523,   506,
     507,   508,   750,   133,   181,   182,   113,   598,   599,   115,
     761,   113,   115,   119,   113,   456,   119,   104,   108,   106,
     461,   198,   112,   654,   114,    23,   104,   698,   106,    84,
     207,    86,   783,   123,   541,   542,   115,   127,   681,   113,
     119,   548,   104,   456,   106,   471,   624,   559,   461,   553,
     554,   104,   736,   106,    65,    66,   482,   483,     4,   482,
     483,   565,   791,   113,    79,   559,    80,    81,   119,   576,
     577,   578,    27,    79,   119,   120,   545,   503,    93,   586,
     503,    96,   766,   767,    80,    81,   717,    93,   103,   515,
     586,    91,   269,    72,    73,    74,    75,   118,   605,    77,
     277,    77,    77,   681,    77,   120,    15,   611,   612,   124,
     125,    80,    81,   586,   120,    80,    81,   624,   124,   125,
      81,   562,   128,   129,    41,   768,   109,   109,   719,   119,
     133,   119,   558,   113,   560,   312,   559,   641,   642,   643,
      80,   113,   113,   113,    91,   558,   653,   654,   655,   562,
     113,   729,   105,   105,   732,   119,   787,   653,   654,   655,
     109,   119,   113,   113,    85,   672,    85,   344,   113,   676,
     113,   678,   598,   599,   681,   598,   599,   119,    25,    27,
     653,   654,   655,    81,   691,   697,   627,   109,    77,   133,
     768,    77,   770,   134,    77,   691,   120,   113,   705,    77,
     707,    80,   120,   697,   711,   712,    80,   384,   119,    27,
     717,   715,    81,   119,   721,   722,   723,   120,   691,   730,
     735,   717,   729,   105,   735,   732,   722,   723,   119,    81,
     755,   101,   112,   659,   101,   742,   659,   113,    84,   120,
      78,   109,   746,   750,   717,   752,   742,   120,   755,   722,
     723,   692,   109,   119,   761,   120,   120,    77,    28,    27,
       9,   768,   113,   770,   105,   118,    37,    27,   775,   742,
     113,    80,   698,    84,   697,    80,   783,   113,    11,   775,
     787,    19,    86,    91,   791,    86,    53,    80,   123,   119,
     113,   787,   469,   719,   113,   105,   719,     4,   475,    84,
      84,   478,   775,   105,   163,    12,   122,   783,   276,   723,
     710,   659,   599,   719,   787,   492,   483,    24,    15,   209,
     672,    28,   542,    30,    31,    32,    33,    34,    35,   506,
      37,    38,    39,    40,   269,    42,    43,    44,   198,   621,
      47,   775,   508,   686,    51,    52,    53,    54,    55,    56,
     679,   471,   729,     3,     4,     5,     6,     7,     8,   626,
      10,   772,    12,    13,   541,   542,    16,    17,    18,    19,
      20,   692,    22,   629,    24,    34,    26,   107,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,   283,    42,    43,    44,   234,   642,    47,   641,   576,
      50,    51,    52,    53,    54,    55,    56,    57,   645,   200,
      79,    80,   497,    63,   558,   122,   123,   735,   392,   126,
     127,   545,   244,    -1,    93,    -1,    76,    96,    -1,    -1,
      -1,   393,    82,    -1,   103,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    -1,    94,    -1,    -1,    -1,    -1,    -1,
      -1,   120,    -1,    -1,    -1,   124,   125,    79,   108,   128,
     129,    -1,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    93,   122,   123,    96,    -1,   126,   127,    -1,    -1,
      -1,   103,   132,   133,   134,   135,    -1,    -1,   138,    -1,
      -1,    -1,   142,   143,    -1,   672,    -1,    -1,   120,   676,
      -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,
      -1,     3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    24,    -1,    26,   712,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,   755,    79,
      -1,    -1,    -1,    -1,    76,    -1,    86,    -1,    -1,    -1,
      82,    -1,    -1,    93,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,   791,    -1,   108,    -1,    -1,   119,
     112,    -1,   114,    -1,   124,   125,    -1,    -1,   128,   129,
     122,   123,    -1,    -1,   126,   127,    -1,    -1,    -1,    -1,
     132,   133,   134,   135,    -1,    -1,   138,     3,     4,     5,
     142,   143,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,
      26,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    79,    -1,    81,    63,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    93,    -1,
      76,    96,    -1,    -1,    -1,    -1,    82,    -1,   103,    93,
      -1,    -1,    96,    89,    -1,    -1,    92,    -1,    94,   103,
      -1,    -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,   124,
     125,    -1,   108,   128,   129,    -1,   112,    -1,   114,    -1,
     124,   125,    -1,    -1,   128,   129,   122,   123,    -1,    -1,
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
      -1,    24,   126,   127,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   112,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,    -1,    24,   126,   127,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    52,    53,    54,    55,    56,    -1,   122,
     123,    -1,    24,    -1,   127,    -1,    28,    -1,    30,    31,
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
     231,   235,   311,   312,   316,   326,   328,   339,   112,   104,
     106,   294,   267,    72,    73,    74,    75,   181,   104,   106,
     211,   212,    19,    37,   179,   226,   228,   312,    14,   294,
     272,   108,   265,   266,   112,   330,   108,   112,   304,   305,
     307,   342,   272,   292,   293,   272,   271,   272,    79,   109,
     120,   125,   129,   267,   268,   276,   278,   279,   309,   323,
     325,   335,   336,   338,   343,   344,   102,   113,   119,   275,
     276,   277,   336,   337,   338,   343,   348,   115,   348,    19,
     265,   265,   134,   170,   161,    71,   201,    80,   119,    81,
      84,   120,   261,   262,   263,   309,   322,   324,   334,   336,
     337,    88,   272,   101,   104,    87,   133,   113,   113,   113,
     113,   113,   156,    78,   158,   159,   160,   151,   151,     4,
     167,    23,    80,   235,   235,   112,   219,   252,   253,   254,
     333,    28,   109,   221,   222,   224,   226,    79,    86,    93,
     113,   124,   125,   128,   129,   221,   239,   318,   319,   348,
     348,    27,   205,    91,    86,   120,   233,   314,   318,   327,
      88,   232,   235,   221,   238,   239,    20,    46,   267,   291,
     295,   296,   297,   295,   118,   270,    77,    77,    77,    77,
     217,   222,   236,   210,   259,   260,   268,   210,    15,   190,
     226,   226,    80,   119,    81,    41,    90,   134,   330,    77,
     133,   344,    80,   119,   216,    86,   292,   332,   341,   322,
      78,    84,   119,   109,   119,   268,   331,   333,   101,   113,
     113,   119,   113,   119,   113,   113,   113,   119,   115,   236,
     330,   330,   120,   172,   308,   320,   321,   337,   344,   133,
     200,   218,   222,   223,   329,   267,   281,   282,   297,   332,
      27,   213,   263,   269,   235,   331,    78,   298,   299,   300,
     330,   331,   333,   272,   113,   113,   119,   105,   346,   347,
      12,   112,   168,   169,   104,   106,   283,   217,   337,    80,
     105,   119,   240,   109,    80,    91,   113,   113,   119,   113,
     113,   115,   104,   106,   204,   226,   222,   316,   328,   228,
     235,   113,   119,   212,   294,    85,   105,   118,   346,    25,
      27,   209,   105,   118,   346,   267,    81,    80,    81,   198,
     222,   243,   112,   312,   221,   133,   134,   109,    77,    77,
     113,   108,   112,   114,   310,   311,   304,    77,   267,   120,
     120,   267,   280,   297,   267,   276,   276,   331,   276,   276,
      77,    80,    80,   333,   342,   119,    77,   133,    80,    81,
     196,   247,   213,    81,   119,   120,   212,   105,   119,    81,
     101,   160,   112,   154,   105,   118,   267,   284,   285,   286,
     290,   284,   346,   113,   222,   254,   103,   104,   112,   241,
     242,   326,   243,   222,   221,     7,    26,   191,   202,   203,
     260,   203,   120,   120,   221,   267,   296,   267,   104,   106,
     208,   260,   222,   243,   241,    84,   184,   329,   109,   113,
     115,   119,    78,   217,   220,   220,   120,   120,   320,    77,
     243,    28,   248,   249,   250,    27,   244,     9,   255,   256,
     257,   267,   297,   299,   276,   301,   331,   154,   113,   284,
     105,   118,    84,    86,   287,   288,   289,   346,   222,   326,
     326,   113,    37,   192,    19,    37,   190,   226,   105,   118,
     346,   270,    26,   194,   206,   207,   260,   207,   185,   328,
      27,   187,    80,   297,   267,    77,   119,    77,   240,    84,
     225,   230,   234,   235,   251,   104,   106,   255,    22,    51,
      52,   112,   182,   258,   315,   316,   257,    81,   101,   113,
     286,   281,   267,   213,   289,    80,   105,    80,   226,   190,
     226,    80,    81,   199,   202,    11,    19,   193,   105,   118,
     346,    86,   104,   106,   188,   218,   217,   103,   249,    91,
     120,   234,   308,   245,   246,   270,   245,   113,   226,   227,
     237,   258,    53,   183,   276,   331,    86,   213,   243,   243,
      80,   197,    81,   199,   243,   112,   242,   326,   267,   190,
     206,   186,   328,    78,   189,   190,    78,   189,   230,   251,
     230,   105,   118,   305,   346,   119,   113,   226,   267,   105,
     113,   243,   326,    84,   328,   105,   105,   118,   346,   346,
     246,    80,   237,   185,   190,   217
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
     177,   177,   178,   179,   179,   180,   180,   181,   181,   181,
     181,   181,   182,   182,   182,   183,   184,   184,   185,   186,
     186,   187,   187,   188,   188,   188,   188,   189,   189,   189,
     189,   190,   191,   191,   191,   191,   191,   192,   192,   193,
     193,   194,   195,   195,   196,   196,   197,   197,   198,   198,
     198,   199,   199,   199,   200,   200,   201,   201,   201,   202,
     202,   203,   203,   203,   203,   204,   204,   205,   205,   206,
     206,   207,   207,   207,   207,   208,   208,   209,   209,   210,
     210,   210,   210,   211,   211,   212,   213,   213,   214,   214,
     215,   215,   215,   216,   216,   217,   218,   219,   219,   220,
     220,   221,   221,   222,   222,   222,   223,   224,   225,   226,
     226,   227,   228,   229,   229,   230,   230,   231,   231,   231,
     232,   233,   233,   234,   235,   235,   235,   235,   235,   235,
     235,   235,   235,   235,   236,   237,   237,   238,   238,   239,
     239,   240,   240,   241,   241,   241,   242,   242,   243,   244,
     244,   244,   245,   245,   246,   247,   248,   248,   249,   249,
     250,   250,   251,   251,   252,   252,   253,   253,   254,   255,
     255,   256,   256,   257,   257,   257,   258,   258,   258,   259,
     259,   260,   261,   261,   262,   262,   263,   264,   264,   264,
     264,   264,   264,   264,   264,   264,   265,   265,   266,   266,
     267,   267,   268,   268,   269,   269,   270,   270,   271,   271,
     271,   271,   272,   272,   272,   272,   272,   272,   272,   272,
     272,   272,   273,   273,   273,   274,   274,   274,   274,   274,
     274,   274,   274,   275,   275,   276,   276,   276,   277,   277,
     278,   278,   278,   278,   278,   278,   278,   279,   279,   280,
     280,   281,   282,   282,   283,   283,   283,   283,   284,   284,
     285,   285,   285,   286,   287,   287,   288,   288,   289,   290,
     291,   292,   293,   293,   294,   294,   295,   295,   295,   295,
     296,   296,   297,   297,   297,   298,   298,   299,   299,   299,
     300,   300,   300,   300,   301,   301,   302,   302,   303,   303,
     304,   304,   304,   305,   305,   306,   306,   306,   306,   307,
     307,   308,   308,   309,   309,   310,   310,   310,   311,   311,
     311,   311,   311,   312,   312,   313,   313,   313,   313,   314,
     314,   315,   316,   316,   317,   318,   318,   318,   319,   319,
     319,   319,   320,   320,   321,   321,   322,   322,   323,   323,
     324,   324,   325,   325,   326,   327,   328,   328,   328,   328,
     328,   329,   329,   330,   330,   330,   331,   332,   332,   333,
     333,   333,   333,   333,   333,   333,   333,   334,   334,   335,
     335,   336,   337,   337,   338,   338,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   340,   340,   341,   341,   342,   343,
     343,   344,   344,   345,   345,   345,   345,   345,   346,   346,
     347,   347,   348,   348
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
       6,     6,     4,     3,     1,     4,     3,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     0,     2,     3,     2,
       1,     0,     2,     3,     3,     3,     3,     3,     2,     1,
       0,     3,     4,     3,     4,     2,     3,     0,     1,     0,
       1,     3,     1,     1,     0,     2,     0,     2,     0,     2,
       2,     0,     2,     4,     3,     1,     4,     3,     0,     1,
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
       0,   509,   509,   526,   527,   529,   533,   534,   535,   537,
     538,   540,   541,   544,   546,   547,   548,   556,   557,   559,
     561,   562,   564,   565,   567,   568,   569,   571,   572,   574,
     575,   577,   578,   582,   583,   585,   586,   588,   590,   591,
     593,   606,   607,   609,   610,   612,   613,   617,   618,   623,
     624,   626,   627,   628,   630,   631,   635,   637,   638,   640,
     641,   642,   643,   646,   647,   654,   656,   658,   660,   661,
     663,   664,   667,   669,   670,   673,   674,   678,   679,   680,
     681,   682,   684,   685,   686,   688,   699,   700,   702,   704,
     705,   709,   710,   712,   713,   714,   715,   717,   718,   719,
     720,   722,   725,   727,   729,   731,   732,   734,   734,   736,
     736,   740,   742,   743,   747,   748,   750,   751,   753,   754,
     755,   757,   758,   759,   763,   764,   766,   767,   768,   810,
     811,   813,   814,   815,   816,   818,   819,   821,   822,   824,
     825,   827,   828,   829,   830,   832,   833,   835,   836,   839,
     840,   841,   842,   844,   845,   847,   849,   850,   858,   859,
     861,   862,   863,   876,   877,   886,   888,   890,   891,   893,
     894,   903,   904,   906,   907,   909,   911,   920,   922,   924,
     925,   927,   930,   932,   933,   935,   936,   938,   940,   941,
     943,   945,   946,   953,   960,   961,   962,   963,   964,   965,
     966,   967,   973,   974,   977,   979,   980,   982,   983,   985,
     986,   993,   994,   996,   997,   998,  1001,  1002,  1020,  1026,
    1027,  1028,  1030,  1031,  1033,  1035,  1037,  1038,  1040,  1041,
    1043,  1044,  1046,  1047,  1053,  1054,  1056,  1057,  1059,  1061,
    1062,  1064,  1065,  1067,  1068,  1069,  1071,  1072,  1073,  1078,
    1080,  1082,  1086,  1087,  1089,  1090,  1094,  1104,  1105,  1107,
    1108,  1109,  1110,  1111,  1112,  1113,  1116,  1117,  1119,  1120,
    1125,  1126,  1130,  1131,  1133,  1134,  1136,  1137,  1142,  1143,
    1144,  1145,  1148,  1149,  1150,  1151,  1152,  1154,  1156,  1157,
    1158,  1160,  1163,  1164,  1165,  1168,  1169,  1170,  1171,  1172,
    1173,  1178,  1179,  1182,  1183,  1188,  1189,  1190,  1195,  1196,
    1214,  1215,  1216,  1217,  1218,  1219,  1220,  1222,  1223,  1236,
    1238,  1248,  1250,  1251,  1254,  1255,  1256,  1257,  1259,  1260,
    1262,  1263,  1264,  1266,  1268,  1269,  1271,  1272,  1281,  1283,
    1285,  1287,  1289,  1290,  1293,  1294,  1296,  1297,  1298,  1299,
    1304,  1305,  1307,  1308,  1309,  1314,  1315,  1317,  1318,  1319,
    1321,  1322,  1323,  1324,  1327,  1328,  1360,  1361,  1363,  1364,
    1366,  1367,  1368,  1370,  1371,  1373,  1374,  1375,  1376,  1378,
    1379,  1381,  1382,  1384,  1385,  1388,  1389,  1390,  1392,  1393,
    1394,  1395,  1396,  1398,  1399,  1401,  1402,  1403,  1404,  1407,
    1408,  1410,  1412,  1413,  1417,  1419,  1420,  1421,  1423,  1424,
    1425,  1426,  1431,  1432,  1434,  1435,  1437,  1438,  1441,  1442,
    1447,  1448,  1450,  1451,  1455,  1457,  1459,  1460,  1461,  1462,
    1463,  1466,  1467,  1469,  1470,  1471,  1473,  1475,  1476,  1478,
    1479,  1480,  1481,  1482,  1483,  1484,  1485,  1487,  1488,  1490,
    1491,  1493,  1495,  1496,  1498,  1499,  1501,  1502,  1503,  1504,
    1505,  1506,  1507,  1508,  1509,  1510,  1511,  1512,  1513,  1514,
    1515,  1516,  1517,  1518,  1520,  1521,  1525,  1526,  1528,  1530,
    1531,  1533,  1534,  1538,  1539,  1540,  1541,  1542,  1547,  1550,
    1554,  1555,  1557,  1558
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
#line 7021 "parser.cc"

#line 1567 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}

pair<vector<Hs::ImpDecl>, optional<Hs::Decls>> make_body(const std::vector<Hs::ImpDecl>& imports, const std::optional<Hs::Decls>& topdecls)
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

Hs::TypeFamilyDecl make_type_family(const Hs::LType& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig,
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

    return Hs::TypeFamilyDecl(lcon, tyvars, kind, eqns);
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

Hs::InstanceDecl make_instance_decl(const Hs::LType& ltype_orig, const optional<Located<Hs::Decls>>& decls)
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
    return {context, ltype, type_inst_decls, method_decls};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::LType& header, const optional<Located<Hs::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);

    std::vector<Hs::FixityDecl> fixity_decls;
    std::vector<Hs::TypeFamilyDecl> type_fam_decls;
    std::vector<Hs::TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<Hs::SignatureDecl> sig_decls;
    Hs::Decls default_method_decls;

    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto F = decl.to<Hs::FixityDecl>())
                fixity_decls.push_back(*F);
            else if (auto TF = decl.to<Hs::TypeFamilyDecl>())
                type_fam_decls.push_back(*TF);
            else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                default_type_inst_decls.push_back(*TI);
            else if (auto S = decl.to<Hs::SignatureDecl>())
                sig_decls.push_back(*S);
            else if (auto V = decl.to<Hs::ValueDecl>())
                default_method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of class "<<name<<", I don't recognize declaration:\n   "<<decl.print();
        }

    return {context, name, check_all_type_vars(type_args), fixity_decls, type_fam_decls, default_type_inst_decls, sig_decls, default_method_decls};
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

