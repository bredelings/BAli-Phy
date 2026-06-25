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
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_decllist: // decllist
        value.YY_MOVE_OR_COPY< Hs::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.YY_MOVE_OR_COPY< Hs::DerivingStrategy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< Hs::Exp > (YY_MOVE (that.value));
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
        value.YY_MOVE_OR_COPY< Hs::LDecl > (YY_MOVE (that.value));
        break;

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
        value.YY_MOVE_OR_COPY< Hs::LExp > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.YY_MOVE_OR_COPY< Hs::LExport > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Hs::LImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.YY_MOVE_OR_COPY< Hs::LStmt > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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

      case symbol_kind::S_altslist: // altslist
        value.YY_MOVE_OR_COPY< Hs::ParsedAlts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.YY_MOVE_OR_COPY< Hs::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.YY_MOVE_OR_COPY< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
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

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Hs::ParsedAlt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_role: // role
        value.YY_MOVE_OR_COPY< Located<std::optional<Role>> > (YY_MOVE (that.value));
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
        value.YY_MOVE_OR_COPY< char32_t > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
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

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.YY_MOVE_OR_COPY< std::vector<Hs::Deriving> > (YY_MOVE (that.value));
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
      case symbol_kind::S_ifgdpats: // ifgdpats
        value.YY_MOVE_OR_COPY< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_apats1: // apats1
        value.YY_MOVE_OR_COPY< std::vector<Hs::LExp> > (YY_MOVE (that.value));
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

      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_stmts: // stmts
        value.YY_MOVE_OR_COPY< std::vector<Hs::LStmt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
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
        value.YY_MOVE_OR_COPY< std::vector<Located<Hs::ParsedAlt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.YY_MOVE_OR_COPY< std::vector<Located<std::optional<Role>>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.YY_MOVE_OR_COPY< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_projection: // projection
        value.YY_MOVE_OR_COPY< std::vector<std::string> > (YY_MOVE (that.value));
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
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_decllist: // decllist
        value.move< Hs::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.move< Hs::DerivingStrategy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.move< Hs::Exp > (YY_MOVE (that.value));
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
        value.move< Hs::LDecl > (YY_MOVE (that.value));
        break;

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
        value.move< Hs::LExp > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.move< Hs::LExport > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::LImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.move< Hs::LStmt > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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

      case symbol_kind::S_altslist: // altslist
        value.move< Hs::ParsedAlts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Hs::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (YY_MOVE (that.value));
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

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::ParsedAlt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_role: // role
        value.move< Located<std::optional<Role>> > (YY_MOVE (that.value));
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
        value.move< char32_t > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (that.value));
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

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.move< std::vector<Hs::Deriving> > (YY_MOVE (that.value));
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
      case symbol_kind::S_ifgdpats: // ifgdpats
        value.move< std::vector<Hs::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_apats1: // apats1
        value.move< std::vector<Hs::LExp> > (YY_MOVE (that.value));
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

      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<Hs::LStmt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
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
        value.move< std::vector<Located<Hs::ParsedAlt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.move< std::vector<Located<std::optional<Role>>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_projection: // projection
        value.move< std::vector<std::string> > (YY_MOVE (that.value));
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
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_decllist: // decllist
        value.copy< Hs::Decls > (that.value);
        break;

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.copy< Hs::DerivingStrategy > (that.value);
        break;

      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.copy< Hs::Exp > (that.value);
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
        value.copy< Hs::LDecl > (that.value);
        break;

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
        value.copy< Hs::LExp > (that.value);
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.copy< Hs::LExport > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Hs::LImpDecl > (that.value);
        break;

      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.copy< Hs::LStmt > (that.value);
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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

      case symbol_kind::S_altslist: // altslist
        value.copy< Hs::ParsedAlts > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.copy< Hs::Stmts > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.copy< Hs::TypeFamilyInstanceEqn > (that.value);
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

      case symbol_kind::S_alt: // alt
        value.copy< Located<Hs::ParsedAlt> > (that.value);
        break;

      case symbol_kind::S_role: // role
        value.copy< Located<std::optional<Role>> > (that.value);
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
        value.copy< char32_t > (that.value);
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.copy< double > (that.value);
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

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.copy< std::vector<Hs::Deriving> > (that.value);
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
      case symbol_kind::S_ifgdpats: // ifgdpats
        value.copy< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_apats1: // apats1
        value.copy< std::vector<Hs::LExp> > (that.value);
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

      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_stmts: // stmts
        value.copy< std::vector<Hs::LStmt> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
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
        value.copy< std::vector<Located<Hs::ParsedAlt>> > (that.value);
        break;

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.copy< std::vector<Located<std::optional<Role>>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.copy< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_projection: // projection
        value.copy< std::vector<std::string> > (that.value);
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
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_decllist: // decllist
        value.move< Hs::Decls > (that.value);
        break;

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        value.move< Hs::DerivingStrategy > (that.value);
        break;

      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        value.move< Hs::Exp > (that.value);
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
        value.move< Hs::LDecl > (that.value);
        break;

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
        value.move< Hs::LExp > (that.value);
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        value.move< Hs::LExport > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Hs::LImpDecl > (that.value);
        break;

      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        value.move< Hs::LStmt > (that.value);
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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

      case symbol_kind::S_altslist: // altslist
        value.move< Hs::ParsedAlts > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Hs::Stmts > (that.value);
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        value.move< Hs::TypeFamilyInstanceEqn > (that.value);
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

      case symbol_kind::S_alt: // alt
        value.move< Located<Hs::ParsedAlt> > (that.value);
        break;

      case symbol_kind::S_role: // role
        value.move< Located<std::optional<Role>> > (that.value);
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
        value.move< char32_t > (that.value);
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        value.move< double > (that.value);
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

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        value.move< std::vector<Hs::Deriving> > (that.value);
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
      case symbol_kind::S_ifgdpats: // ifgdpats
        value.move< std::vector<Hs::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_apats1: // apats1
        value.move< std::vector<Hs::LExp> > (that.value);
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

      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<Hs::LStmt> > (that.value);
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
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
        value.move< std::vector<Located<Hs::ParsedAlt>> > (that.value);
        break;

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        value.move< std::vector<Located<std::optional<Role>>> > (that.value);
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        value.move< std::vector<Located<std::string>> > (that.value);
        break;

      case symbol_kind::S_projection: // projection
        value.move< std::vector<std::string> > (that.value);
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
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_decllist: // decllist
        yylhs.value.emplace< Hs::Decls > ();
        break;

      case symbol_kind::S_deriv_strategy_no_via: // deriv_strategy_no_via
        yylhs.value.emplace< Hs::DerivingStrategy > ();
        break;

      case symbol_kind::S_list: // list
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< Hs::Exp > ();
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
      case symbol_kind::S_gadt_constrs0: // gadt_constrs0
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

      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_standalone_kind_sig: // standalone_kind_sig
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_stand_alone_deriving: // stand_alone_deriving
      case symbol_kind::S_at_decl_cls: // at_decl_cls
      case symbol_kind::S_at_decl_inst: // at_decl_inst
      case symbol_kind::S_decl_cls: // decl_cls
      case symbol_kind::S_decl_inst: // decl_inst
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_sigdecl: // sigdecl
        yylhs.value.emplace< Hs::LDecl > ();
        break;

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
        yylhs.value.emplace< Hs::LExp > ();
        break;

      case symbol_kind::S_export: // export
      case symbol_kind::S_import: // import
        yylhs.value.emplace< Hs::LExport > ();
        break;

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Hs::LImpDecl > ();
        break;

      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
        yylhs.value.emplace< Hs::LStmt > ();
        break;

      case symbol_kind::S_deriv_strategy_via: // deriv_strategy_via
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

      case symbol_kind::S_altslist: // altslist
        yylhs.value.emplace< Hs::ParsedAlts > ();
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        yylhs.value.emplace< Hs::Stmts > ();
        break;

      case symbol_kind::S_ty_fam_inst_eqn: // ty_fam_inst_eqn
        yylhs.value.emplace< Hs::TypeFamilyInstanceEqn > ();
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

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Hs::ParsedAlt> > ();
        break;

      case symbol_kind::S_role: // role
        yylhs.value.emplace< Located<std::optional<Role>> > ();
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
        yylhs.value.emplace< char32_t > ();
        break;

      case symbol_kind::S_PRIMDOUBLE: // "PRIMDOUBLE"
        yylhs.value.emplace< double > ();
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

      case symbol_kind::S_maybe_derivings: // maybe_derivings
      case symbol_kind::S_derivings: // derivings
      case symbol_kind::S_deriving: // deriving
        yylhs.value.emplace< std::vector<Hs::Deriving> > ();
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
      case symbol_kind::S_ifgdpats: // ifgdpats
        yylhs.value.emplace< std::vector<Hs::GuardedRHS> > ();
        break;

      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_apats1: // apats1
        yylhs.value.emplace< std::vector<Hs::LExp> > ();
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

      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_stmts: // stmts
        yylhs.value.emplace< std::vector<Hs::LStmt> > ();
        break;

      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_deriv_types: // deriv_types
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_deriv_clause_types: // deriv_clause_types
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
        yylhs.value.emplace< std::vector<Located<Hs::ParsedAlt>> > ();
        break;

      case symbol_kind::S_maybe_roles: // maybe_roles
      case symbol_kind::S_roles: // roles
        yylhs.value.emplace< std::vector<Located<std::optional<Role>>> > ();
        break;

      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_ops: // ops
      case symbol_kind::S_con_list: // con_list
        yylhs.value.emplace< std::vector<Located<std::string>> > ();
        break;

      case symbol_kind::S_projection: // projection
        yylhs.value.emplace< std::vector<std::string> > ();
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
#line 517 "parser.y"
             {drv.result = yystack_[0].value.as < Hs::Module > ();}
#line 2776 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 534 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{yystack_[4].value.as < Located<std::string> > (),yystack_[2].value.as < std::optional<std::vector<Hs::LExport>> > (),yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2782 "parser.cc"
    break;

  case 4: // module: body2
#line 535 "parser.y"
                                                                 {yylhs.value.as < Hs::Module > () = Hs::Module{{noloc,"Main"},{},yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ().second};}
#line 2788 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 537 "parser.y"
                                                                 {drv.push_module_context();}
#line 2794 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 545 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2800 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 546 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2806 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 548 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2812 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 549 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2818 "parser.cc"
    break;

  case 13: // top: semis top1
#line 552 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > ();}
#line 2824 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 554 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2830 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 555 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Hs::LImpDecl> > (),yystack_[0].value.as < Hs::Decls > ());}
#line 2836 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 556 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Hs::LImpDecl> > (),{});}
#line 2842 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 564 "parser.y"
                                      {yylhs.value.as < std::optional<std::vector<Hs::LExport>> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2848 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 565 "parser.y"
                                      {}
#line 2854 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 567 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 2860 "parser.cc"
    break;

  case 20: // exportlist: %empty
#line 568 "parser.y"
                                      {}
#line 2866 "parser.cc"
    break;

  case 21: // exportlist: exportlist1 ','
#line 569 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 2872 "parser.cc"
    break;

  case 22: // exportlist: ','
#line 570 "parser.y"
                                      {}
#line 2878 "parser.cc"
    break;

  case 23: // exportlist1: exportlist1 "," export
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2884 "parser.cc"
    break;

  case 24: // exportlist1: export
#line 573 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 2890 "parser.cc"
    break;

  case 25: // export: qcname export_subspec
#line 575 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 2896 "parser.cc"
    break;

  case 26: // export: "module" modid
#line 576 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2902 "parser.cc"
    break;

  case 27: // export: "default" modid
#line 577 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::default_}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 2908 "parser.cc"
    break;

  case 28: // export_subspec: %empty
#line 579 "parser.y"
                                      {}
#line 2914 "parser.cc"
    break;

  case 29: // export_subspec: "(" qcnames ")"
#line 580 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec{yystack_[1].value.as < std::vector<Located<std::string>> > ()}; }
#line 2920 "parser.cc"
    break;

  case 30: // export_subspec: "(" ".." ")"
#line 581 "parser.y"
                                      { yylhs.value.as < std::optional<Hs::ExportSubSpec> > () = Hs::ExportSubSpec(); }
#line 2926 "parser.cc"
    break;

  case 31: // qcnames: %empty
#line 583 "parser.y"
                   {}
#line 2932 "parser.cc"
    break;

  case 32: // qcnames: qcnames1
#line 584 "parser.y"
                   {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[0].value.as < std::vector<Located<std::string>> > ();}
#line 2938 "parser.cc"
    break;

  case 33: // qcnames1: qcnames1 "," qcname
#line 586 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2944 "parser.cc"
    break;

  case 34: // qcnames1: qcname
#line 587 "parser.y"
                                      {yylhs.value.as < std::vector<Located<std::string>> > ().push_back(yystack_[0].value.as < Located<std::string> > ());}
#line 2950 "parser.cc"
    break;

  case 35: // qcname: qvar
#line 589 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2956 "parser.cc"
    break;

  case 36: // qcname: oqtycon_no_varcon
#line 590 "parser.y"
                                      { yylhs.value.as < Located<std::string> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()}; }
#line 2962 "parser.cc"
    break;

  case 41: // importdecls: importdecls_semi importdecl
#line 600 "parser.y"
                                         { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[1].value.as < std::vector<Hs::LImpDecl> > (), yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[0].value.as < Hs::LImpDecl > ()); }
#line 2968 "parser.cc"
    break;

  case 42: // importdecls_semi: importdecls_semi importdecl semis1
#line 602 "parser.y"
                                                     { yylhs.value.as < std::vector<Hs::LImpDecl> > () = yystack_[2].value.as < std::vector<Hs::LImpDecl> > (); yylhs.value.as < std::vector<Hs::LImpDecl> > ().push_back(yystack_[1].value.as < Hs::LImpDecl > ()); }
#line 2974 "parser.cc"
    break;

  case 43: // importdecls_semi: %empty
#line 603 "parser.y"
                         { }
#line 2980 "parser.cc"
    break;

  case 44: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 605 "parser.y"
                                                                                                        {
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2988 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 618 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2994 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 619 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 3000 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 621 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 3006 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 622 "parser.y"
                               { }
#line 3012 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 624 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 3018 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 625 "parser.y"
                               { }
#line 3024 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 629 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 3030 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 630 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 3036 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 632 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 3042 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 633 "parser.y"
                                      {}
#line 3048 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 634 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 3054 "parser.cc"
    break;

  case 56: // importlist: ','
#line 635 "parser.y"
                                      {}
#line 3060 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 637 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 3066 "parser.cc"
    break;

  case 58: // importlist1: import
#line 638 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 3072 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 640 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 3078 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 641 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 3084 "parser.cc"
    break;

  case 61: // prec: %empty
#line 646 "parser.y"
                   { }
#line 3090 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 647 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 3096 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 649 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 3102 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 650 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 3108 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 651 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 3114 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 653 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3120 "parser.cc"
    break;

  case 67: // ops: op
#line 654 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 3126 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 658 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ()); }
#line 3132 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 660 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Hs::LDecl > ()); }
#line 3138 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 661 "parser.y"
                                            { }
#line 3144 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 663 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3150 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 664 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3156 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 665 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3162 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 666 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3168 "parser.cc"
    break;

  case 75: // topdecl: stand_alone_deriving
#line 667 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3174 "parser.cc"
    break;

  case 76: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 668 "parser.y"
                                                         {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 3180 "parser.cc"
    break;

  case 77: // topdecl: "foreign" "import" call_conv "STRING" var "::" sigtypedoc
#line 669 "parser.y"
                                                                  {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::ForeignDecl(yystack_[4].value.as < Located<std::string> > (), yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 3186 "parser.cc"
    break;

  case 78: // topdecl: decl_no_th
#line 675 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3192 "parser.cc"
    break;

  case 79: // topdecl: infixexp
#line 677 "parser.y"
                                               { drv.push_error_message(yystack_[0].location, "Unexpected top-level expression.");
                                                yylhs.value.as < Hs::LDecl > () = {yystack_[0].location, Hs::ValueDecl({yystack_[0].location, unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}, Hs::SimpleRHS({yystack_[0].location, Hs::Var("<top-level-expression>")}))}; }
#line 3199 "parser.cc"
    break;

  case 80: // call_conv: "bpcall"
#line 681 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"bpcall"};}
#line 3205 "parser.cc"
    break;

  case 81: // call_conv: "trcall"
#line 682 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"trcall"};}
#line 3211 "parser.cc"
    break;

  case 82: // call_conv: "ecall"
#line 683 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"ecall"};}
#line 3217 "parser.cc"
    break;

  case 83: // cl_decl: "class" tycl_hdr fds where_cls
#line 685 "parser.y"
                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3223 "parser.cc"
    break;

  case 84: // ty_decl: "type" "role" oqtycon maybe_roles
#line 688 "parser.y"
                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::RoleAnnotationDecl({yystack_[1].location, Hs::TypeCon(yystack_[1].value.as < std::string > ())}, yystack_[0].value.as < std::vector<Located<std::optional<Role>>> > ())};}
#line 3229 "parser.cc"
    break;

  case 85: // ty_decl: "type" type "=" ktype
#line 689 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3235 "parser.cc"
    break;

  case 86: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 690 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > (),yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3241 "parser.cc"
    break;

  case 87: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 692 "parser.y"
                                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3247 "parser.cc"
    break;

  case 88: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 693 "parser.y"
                                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3253 "parser.cc"
    break;

  case 89: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 694 "parser.y"
                                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3259 "parser.cc"
    break;

  case 90: // standalone_kind_sig: "type" sks_vars "::" kind
#line 696 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3265 "parser.cc"
    break;

  case 91: // sks_vars: sks_vars "," oqtycon
#line 698 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3271 "parser.cc"
    break;

  case 92: // sks_vars: oqtycon
#line 699 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3277 "parser.cc"
    break;

  case 93: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 702 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3283 "parser.cc"
    break;

  case 94: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 703 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3289 "parser.cc"
    break;

  case 95: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 705 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3299 "parser.cc"
    break;

  case 96: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 711 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3309 "parser.cc"
    break;

  case 97: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 717 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3315 "parser.cc"
    break;

  case 98: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 718 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3321 "parser.cc"
    break;

  case 99: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 719 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3327 "parser.cc"
    break;

  case 100: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 720 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3333 "parser.cc"
    break;

  case 101: // overlap_pragma: %empty
#line 721 "parser.y"
                                               {}
#line 3339 "parser.cc"
    break;

  case 102: // deriv_strategy_no_via: "stock"
#line 723 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::stock;}
#line 3345 "parser.cc"
    break;

  case 103: // deriv_strategy_no_via: "anyclass"
#line 724 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::anyclass;}
#line 3351 "parser.cc"
    break;

  case 104: // deriv_strategy_no_via: "newtype"
#line 725 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::newtype;}
#line 3357 "parser.cc"
    break;

  case 105: // deriv_strategy_via: "via" type
#line 727 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3363 "parser.cc"
    break;

  case 106: // stand_alone_deriving: "deriving" "instance" inst_type
#line 730 "parser.y"
                             {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::StandaloneDerivingDecl({}, yystack_[0].value.as < Hs::LType > ())};}
#line 3369 "parser.cc"
    break;

  case 107: // stand_alone_deriving: "deriving" deriv_strategy_no_via "instance" inst_type
#line 732 "parser.y"
                             {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::StandaloneDerivingDecl(yystack_[2].value.as < Hs::DerivingStrategy > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3375 "parser.cc"
    break;

  case 108: // stand_alone_deriving: "deriving" deriv_strategy_via "instance" inst_type
#line 734 "parser.y"
                             {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::StandaloneDerivingDecl(Hs::DerivingStrategy::via, yystack_[0].value.as < Hs::LType > (), yystack_[2].value.as < Hs::LType > ())};}
#line 3381 "parser.cc"
    break;

  case 114: // where_type_family: %empty
#line 748 "parser.y"
                                                           {}
#line 3387 "parser.cc"
    break;

  case 115: // where_type_family: "where" ty_fam_inst_eqn_list
#line 749 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3393 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 751 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3399 "parser.cc"
    break;

  case 117: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 752 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3405 "parser.cc"
    break;

  case 118: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 753 "parser.y"
                                                           {}
#line 3411 "parser.cc"
    break;

  case 119: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 754 "parser.y"
                                                           {}
#line 3417 "parser.cc"
    break;

  case 120: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 756 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3423 "parser.cc"
    break;

  case 121: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 757 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3429 "parser.cc"
    break;

  case 122: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 758 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3435 "parser.cc"
    break;

  case 123: // ty_fam_inst_eqns: %empty
#line 759 "parser.y"
                                                           {}
#line 3441 "parser.cc"
    break;

  case 124: // ty_fam_inst_eqn: type "=" ctype
#line 761 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3447 "parser.cc"
    break;

  case 125: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 764 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3453 "parser.cc"
    break;

  case 126: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 766 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3459 "parser.cc"
    break;

  case 127: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 768 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3465 "parser.cc"
    break;

  case 128: // at_decl_cls: "type" ty_fam_inst_eqn
#line 770 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3471 "parser.cc"
    break;

  case 129: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 771 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3477 "parser.cc"
    break;

  case 134: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 779 "parser.y"
                                                              { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3483 "parser.cc"
    break;

  case 135: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 782 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3493 "parser.cc"
    break;

  case 136: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 789 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3503 "parser.cc"
    break;

  case 137: // data_or_newtype: "data"
#line 795 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3509 "parser.cc"
    break;

  case 138: // data_or_newtype: "newtype"
#line 796 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3515 "parser.cc"
    break;

  case 139: // opt_class: %empty
#line 799 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3521 "parser.cc"
    break;

  case 140: // opt_class: qtycon
#line 800 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3527 "parser.cc"
    break;

  case 141: // opt_kind_sig: %empty
#line 804 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3533 "parser.cc"
    break;

  case 142: // opt_kind_sig: "::" kind
#line 805 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3539 "parser.cc"
    break;

  case 143: // opt_datafam_kind_sig: %empty
#line 807 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3545 "parser.cc"
    break;

  case 144: // opt_datafam_kind_sig: "::" kind
#line 808 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3551 "parser.cc"
    break;

  case 145: // opt_tyfam_kind_sig: %empty
#line 810 "parser.y"
                                      {}
#line 3557 "parser.cc"
    break;

  case 146: // opt_tyfam_kind_sig: "::" kind
#line 811 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3563 "parser.cc"
    break;

  case 147: // opt_tyfam_kind_sig: "=" tv_bndr
#line 812 "parser.y"
                                      {}
#line 3569 "parser.cc"
    break;

  case 148: // opt_at_kind_inj_sig: %empty
#line 814 "parser.y"
                                      {}
#line 3575 "parser.cc"
    break;

  case 149: // opt_at_kind_inj_sig: "::" kind
#line 815 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3581 "parser.cc"
    break;

  case 150: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 816 "parser.y"
                                                                  {}
#line 3587 "parser.cc"
    break;

  case 151: // tycl_hdr: context "=>" type
#line 819 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3593 "parser.cc"
    break;

  case 152: // tycl_hdr: type
#line 820 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3599 "parser.cc"
    break;

  case 153: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 823 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3605 "parser.cc"
    break;

  case 154: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 824 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3611 "parser.cc"
    break;

  case 155: // datafam_inst_hdr: context "=>" type
#line 825 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3617 "parser.cc"
    break;

  case 156: // datafam_inst_hdr: type
#line 826 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3623 "parser.cc"
    break;

  case 160: // maybe_roles: %empty
#line 835 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = {};}
#line 3629 "parser.cc"
    break;

  case 161: // maybe_roles: roles
#line 836 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = yystack_[0].value.as < std::vector<Located<std::optional<Role>>> > ();}
#line 3635 "parser.cc"
    break;

  case 162: // roles: role
#line 838 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = {yystack_[0].value.as < Located<std::optional<Role>> > ()};}
#line 3641 "parser.cc"
    break;

  case 163: // roles: roles role
#line 839 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = yystack_[1].value.as < std::vector<Located<std::optional<Role>>> > (); yylhs.value.as < std::vector<Located<std::optional<Role>>> > ().push_back(yystack_[0].value.as < Located<std::optional<Role>> > ());}
#line 3647 "parser.cc"
    break;

  case 164: // role: "VARID"
#line 842 "parser.y"
              {
                  if (yystack_[0].value.as < std::string > () == "nominal")
                      yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, Role::Nominal};
                  else if (yystack_[0].value.as < std::string > () == "representational")
                      yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, Role::Representational};
                  else if (yystack_[0].value.as < std::string > () == "phantom")
                      yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, Role::Phantom};
                  else
                  {
                      drv.push_error_message(yystack_[0].location, "Unknown role `" + yystack_[0].value.as < std::string > () + "`");
                      YYERROR;
                  }
              }
#line 3665 "parser.cc"
    break;

  case 165: // role: "_"
#line 855 "parser.y"
                                                {yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, std::optional<Role>{}};}
#line 3671 "parser.cc"
    break;

  case 166: // decl_cls: at_decl_cls
#line 883 "parser.y"
                        {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3677 "parser.cc"
    break;

  case 167: // decl_cls: decl
#line 884 "parser.y"
                        {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3683 "parser.cc"
    break;

  case 168: // decls_cls: decls_cls ";" decl_cls
#line 886 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3689 "parser.cc"
    break;

  case 169: // decls_cls: decls_cls ";"
#line 887 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3695 "parser.cc"
    break;

  case 170: // decls_cls: decl_cls
#line 888 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3701 "parser.cc"
    break;

  case 171: // decls_cls: %empty
#line 889 "parser.y"
                                           {}
#line 3707 "parser.cc"
    break;

  case 172: // decllist_cls: "{" decls_cls "}"
#line 891 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3713 "parser.cc"
    break;

  case 173: // decllist_cls: "vocurly" decls_cls close
#line 892 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3719 "parser.cc"
    break;

  case 174: // where_cls: "where" decllist_cls
#line 894 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3725 "parser.cc"
    break;

  case 175: // where_cls: %empty
#line 895 "parser.y"
                                           {}
#line 3731 "parser.cc"
    break;

  case 176: // decl_inst: at_decl_inst
#line 897 "parser.y"
                                           {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3737 "parser.cc"
    break;

  case 177: // decl_inst: decl
#line 898 "parser.y"
                                           {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3743 "parser.cc"
    break;

  case 178: // decls_inst: decls_inst ";" decl_inst
#line 900 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3749 "parser.cc"
    break;

  case 179: // decls_inst: decls_inst ";"
#line 901 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3755 "parser.cc"
    break;

  case 180: // decls_inst: decl_inst
#line 902 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3761 "parser.cc"
    break;

  case 181: // decls_inst: %empty
#line 903 "parser.y"
                                           {}
#line 3767 "parser.cc"
    break;

  case 182: // decllist_inst: "{" decls_inst "}"
#line 905 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3773 "parser.cc"
    break;

  case 183: // decllist_inst: "vocurly" decls_inst close
#line 906 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3779 "parser.cc"
    break;

  case 184: // where_inst: "where" decllist_inst
#line 908 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3785 "parser.cc"
    break;

  case 185: // where_inst: %empty
#line 909 "parser.y"
                                           {}
#line 3791 "parser.cc"
    break;

  case 186: // decls: decls ";" decl
#line 912 "parser.y"
                        {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3797 "parser.cc"
    break;

  case 187: // decls: decls ";"
#line 913 "parser.y"
                        {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3803 "parser.cc"
    break;

  case 188: // decls: decl
#line 914 "parser.y"
                        {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3809 "parser.cc"
    break;

  case 189: // decls: %empty
#line 915 "parser.y"
                        {}
#line 3815 "parser.cc"
    break;

  case 190: // decllist: "{" decls "}"
#line 917 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3821 "parser.cc"
    break;

  case 191: // decllist: "vocurly" decls close
#line 918 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3827 "parser.cc"
    break;

  case 192: // binds: decllist
#line 920 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3833 "parser.cc"
    break;

  case 193: // wherebinds: "where" binds
#line 922 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3839 "parser.cc"
    break;

  case 194: // wherebinds: %empty
#line 923 "parser.y"
                                 {}
#line 3845 "parser.cc"
    break;

  case 200: // opt_tyconsig: %empty
#line 949 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3851 "parser.cc"
    break;

  case 201: // opt_tyconsig: "::" gtycon
#line 950 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3857 "parser.cc"
    break;

  case 202: // sigtype: ctype
#line 959 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3863 "parser.cc"
    break;

  case 203: // sigtypedoc: ctypedoc
#line 961 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3869 "parser.cc"
    break;

  case 204: // sig_vars: sig_vars "," var
#line 963 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3875 "parser.cc"
    break;

  case 205: // sig_vars: var
#line 964 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3881 "parser.cc"
    break;

  case 206: // sigtypes1: sigtype
#line 966 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3887 "parser.cc"
    break;

  case 207: // sigtypes1: sigtypes1 "," sigtype
#line 967 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3893 "parser.cc"
    break;

  case 208: // ktype: ctype
#line 976 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3899 "parser.cc"
    break;

  case 209: // ktype: ctype "::" kind
#line 977 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3905 "parser.cc"
    break;

  case 210: // ctype: "forall" tv_bndrs "." ctype
#line 979 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3911 "parser.cc"
    break;

  case 211: // ctype: context "=>" ctype
#line 980 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3917 "parser.cc"
    break;

  case 212: // ctype: type
#line 982 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3923 "parser.cc"
    break;

  case 213: // ctypedoc: ctype
#line 984 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3929 "parser.cc"
    break;

  case 214: // context: btype
#line 993 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3935 "parser.cc"
    break;

  case 215: // context_no_ops: btype_no_ops
#line 995 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3941 "parser.cc"
    break;

  case 216: // type: btype
#line 997 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3947 "parser.cc"
    break;

  case 217: // type: btype "->" ctype
#line 998 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3953 "parser.cc"
    break;

  case 218: // typedoc: type
#line 1000 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3959 "parser.cc"
    break;

  case 219: // btype: infixtype
#line 1003 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3965 "parser.cc"
    break;

  case 220: // infixtype: ftype
#line 1005 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3971 "parser.cc"
    break;

  case 221: // infixtype: btype tyop btype
#line 1006 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3977 "parser.cc"
    break;

  case 222: // btype_no_ops: atype_docs
#line 1008 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3983 "parser.cc"
    break;

  case 223: // btype_no_ops: btype_no_ops atype_docs
#line 1009 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3989 "parser.cc"
    break;

  case 224: // ftype: atype
#line 1011 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3995 "parser.cc"
    break;

  case 225: // ftype: ftype tyarg
#line 1013 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 4001 "parser.cc"
    break;

  case 226: // ftype: ftype "@" atype
#line 1014 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 4007 "parser.cc"
    break;

  case 227: // tyarg: atype
#line 1016 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 4013 "parser.cc"
    break;

  case 228: // tyop: qtyconop
#line 1018 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4019 "parser.cc"
    break;

  case 229: // tyop: tyvarop
#line 1019 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4025 "parser.cc"
    break;

  case 230: // atype_docs: atype
#line 1026 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 4031 "parser.cc"
    break;

  case 231: // atype: ntgtycon
#line 1033 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 4037 "parser.cc"
    break;

  case 232: // atype: tyvar
#line 1034 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 4043 "parser.cc"
    break;

  case 233: // atype: "*"
#line 1035 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 4049 "parser.cc"
    break;

  case 234: // atype: PREFIX_BANG atype
#line 1036 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 4055 "parser.cc"
    break;

  case 235: // atype: PREFIX_TILDE atype
#line 1037 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 4061 "parser.cc"
    break;

  case 236: // atype: "{" fielddecls "}"
#line 1038 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 4067 "parser.cc"
    break;

  case 237: // atype: "(" ")"
#line 1039 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 4073 "parser.cc"
    break;

  case 238: // atype: "(" comma_types1 "," ktype ")"
#line 1040 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 4079 "parser.cc"
    break;

  case 239: // atype: "[" ktype "]"
#line 1046 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 4085 "parser.cc"
    break;

  case 240: // atype: "(" ktype ")"
#line 1047 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 4091 "parser.cc"
    break;

  case 241: // inst_type: sigtype
#line 1050 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 4097 "parser.cc"
    break;

  case 242: // deriv_types: typedoc
#line 1052 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4103 "parser.cc"
    break;

  case 243: // deriv_types: typedoc "," deriv_types
#line 1053 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().insert(yylhs.value.as < std::vector<Hs::LType> > ().begin(), yystack_[2].value.as < Hs::LType > ());}
#line 4109 "parser.cc"
    break;

  case 244: // comma_types0: comma_types1
#line 1055 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 4115 "parser.cc"
    break;

  case 245: // comma_types0: %empty
#line 1056 "parser.y"
                                       { /* default construction OK */ }
#line 4121 "parser.cc"
    break;

  case 246: // comma_types1: ktype
#line 1058 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4127 "parser.cc"
    break;

  case 247: // comma_types1: comma_types1 "," ktype
#line 1059 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4133 "parser.cc"
    break;

  case 248: // tv_bndrs: tv_bndrs tv_bndr
#line 1066 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 4139 "parser.cc"
    break;

  case 249: // tv_bndrs: %empty
#line 1067 "parser.y"
                               { /* default construction OK */}
#line 4145 "parser.cc"
    break;

  case 250: // tv_bndr: tv_bndr_no_braces
#line 1069 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 4151 "parser.cc"
    break;

  case 251: // tv_bndr: "{" tyvar "}"
#line 1070 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 4157 "parser.cc"
    break;

  case 252: // tv_bndr: "{" tyvar "::" kind "}"
#line 1071 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 4163 "parser.cc"
    break;

  case 253: // tv_bndr_no_braces: tyvar
#line 1074 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 4169 "parser.cc"
    break;

  case 254: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1075 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 4175 "parser.cc"
    break;

  case 255: // fds: %empty
#line 1079 "parser.y"
                                    { /* default to empty */ }
#line 4181 "parser.cc"
    break;

  case 256: // fds: "|" fds1
#line 1080 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 4187 "parser.cc"
    break;

  case 257: // fds1: fds1 "," fd
#line 1082 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4193 "parser.cc"
    break;

  case 258: // fds1: fd
#line 1083 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4199 "parser.cc"
    break;

  case 259: // fd: varids0 "->" varids0
#line 1086 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 4205 "parser.cc"
    break;

  case 260: // varids0: varids0 tyvar
#line 1088 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4211 "parser.cc"
    break;

  case 261: // varids0: %empty
#line 1089 "parser.y"
                                    { /* default to empty */}
#line 4217 "parser.cc"
    break;

  case 262: // kind: ctype
#line 1094 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 4223 "parser.cc"
    break;

  case 263: // gadt_constrlist: "where" "{" gadt_constrs0 "}"
#line 1100 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4229 "parser.cc"
    break;

  case 264: // gadt_constrlist: "where" "vocurly" gadt_constrs0 close
#line 1101 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4235 "parser.cc"
    break;

  case 265: // gadt_constrlist: %empty
#line 1102 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 4241 "parser.cc"
    break;

  case 266: // gadt_constrs0: gadt_constrs
#line 1104 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[0].value.as < Hs::GADTConstructorsDecl > ();}
#line 4247 "parser.cc"
    break;

  case 267: // gadt_constrs0: %empty
#line 1105 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()={};}
#line 4253 "parser.cc"
    break;

  case 268: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1107 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4259 "parser.cc"
    break;

  case 269: // gadt_constrs: gadt_constr
#line 1108 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4265 "parser.cc"
    break;

  case 270: // gadt_constr: optSemi con_list "::" sigtype
#line 1110 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4271 "parser.cc"
    break;

  case 271: // constrs: "=" constrs1
#line 1112 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 4277 "parser.cc"
    break;

  case 272: // constrs1: constrs1 "|" constr
#line 1114 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4283 "parser.cc"
    break;

  case 273: // constrs1: constr
#line 1115 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4289 "parser.cc"
    break;

  case 274: // constr: forall context_no_ops "=>" constr_stuff
#line 1117 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 4295 "parser.cc"
    break;

  case 275: // constr: forall constr_stuff
#line 1118 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 4301 "parser.cc"
    break;

  case 276: // forall: "forall" tv_bndrs "."
#line 1120 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 4307 "parser.cc"
    break;

  case 277: // forall: %empty
#line 1121 "parser.y"
                                {}
#line 4313 "parser.cc"
    break;

  case 278: // constr_stuff: btype_no_ops
#line 1123 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 4319 "parser.cc"
    break;

  case 279: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1124 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 4329 "parser.cc"
    break;

  case 280: // fielddecls: %empty
#line 1130 "parser.y"
                                {}
#line 4335 "parser.cc"
    break;

  case 281: // fielddecls: fielddecls1
#line 1131 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 4341 "parser.cc"
    break;

  case 282: // fielddecls1: fielddecls1 "," fielddecl
#line 1133 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4347 "parser.cc"
    break;

  case 283: // fielddecls1: fielddecl
#line 1134 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4353 "parser.cc"
    break;

  case 284: // fielddecl: sig_vars "::" ctype
#line 1136 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4359 "parser.cc"
    break;

  case 285: // maybe_derivings: %empty
#line 1138 "parser.y"
                            {}
#line 4365 "parser.cc"
    break;

  case 286: // maybe_derivings: derivings
#line 1139 "parser.y"
                            {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4371 "parser.cc"
    break;

  case 287: // derivings: derivings deriving
#line 1141 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[1].value.as < std::vector<Hs::Deriving> > (); yylhs.value.as < std::vector<Hs::Deriving> > ().insert(yylhs.value.as < std::vector<Hs::Deriving> > ().end(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().begin(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().end());}
#line 4377 "parser.cc"
    break;

  case 288: // derivings: deriving
#line 1142 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4383 "parser.cc"
    break;

  case 289: // deriving: "deriving" deriv_clause_types
#line 1145 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving({}, type));
          }
#line 4392 "parser.cc"
    break;

  case 290: // deriving: "deriving" deriv_strategy_no_via deriv_clause_types
#line 1150 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(yystack_[1].value.as < Hs::DerivingStrategy > (), type));
          }
#line 4401 "parser.cc"
    break;

  case 291: // deriving: "deriving" deriv_clause_types deriv_strategy_via
#line 1155 "parser.y"
          {
              for(auto& type: yystack_[1].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(Hs::DerivingStrategy::via, type, yystack_[0].value.as < Hs::LType > ()));
          }
#line 4410 "parser.cc"
    break;

  case 292: // deriv_clause_types: qtycondoc
#line 1160 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())});}
#line 4416 "parser.cc"
    break;

  case 293: // deriv_clause_types: "(" ")"
#line 1161 "parser.y"
                                        {}
#line 4422 "parser.cc"
    break;

  case 294: // deriv_clause_types: "(" deriv_types ")"
#line 1162 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > ();}
#line 4428 "parser.cc"
    break;

  case 295: // decl_no_th: sigdecl
#line 1167 "parser.y"
                              {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 4434 "parser.cc"
    break;

  case 296: // decl_no_th: infixexp rhs
#line 1169 "parser.y"
                              {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4440 "parser.cc"
    break;

  case 297: // decl: decl_no_th
#line 1171 "parser.y"
                              {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 4446 "parser.cc"
    break;

  case 298: // rhs: "=" exp wherebinds
#line 1175 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Hs::LExp > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4452 "parser.cc"
    break;

  case 299: // rhs: gdrhs wherebinds
#line 1176 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4458 "parser.cc"
    break;

  case 300: // gdrhs: gdrhs gdrh
#line 1178 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4464 "parser.cc"
    break;

  case 301: // gdrhs: gdrh
#line 1179 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4470 "parser.cc"
    break;

  case 302: // gdrh: "|" guardquals "=" exp
#line 1183 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Hs::LStmt> > (),yystack_[0].value.as < Hs::LExp > ()};}
#line 4476 "parser.cc"
    break;

  case 303: // sigdecl: sig_vars "::" sigtypedoc
#line 1193 "parser.y"
                                  { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeSigDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4482 "parser.cc"
    break;

  case 304: // sigdecl: infix prec ops
#line 1194 "parser.y"
                         { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4488 "parser.cc"
    break;

  case 305: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1196 "parser.y"
                                                    {}
#line 4494 "parser.cc"
    break;

  case 306: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1197 "parser.y"
                                            { yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::InlinePragma({yystack_[3].location,yystack_[3].value.as < std::string > ()},{yystack_[1].location,yystack_[1].value.as < std::string > ()})}; }
#line 4500 "parser.cc"
    break;

  case 307: // sigdecl: "{-# SCC" qvar "#-}"
#line 1198 "parser.y"
                              {}
#line 4506 "parser.cc"
    break;

  case 308: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1199 "parser.y"
                                     {}
#line 4512 "parser.cc"
    break;

  case 309: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1200 "parser.y"
                                                               {}
#line 4518 "parser.cc"
    break;

  case 310: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1201 "parser.y"
                                                                      {}
#line 4524 "parser.cc"
    break;

  case 311: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1202 "parser.y"
                                                     {}
#line 4530 "parser.cc"
    break;

  case 316: // exp: infixexp "::" sigtype
#line 1214 "parser.y"
                           { yylhs.value.as < Hs::LExp > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4536 "parser.cc"
    break;

  case 317: // exp: infixexp
#line 1215 "parser.y"
                           { yylhs.value.as < Hs::LExp > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4542 "parser.cc"
    break;

  case 318: // infixexp: exp10
#line 1219 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Hs::LExp > ()})};}
#line 4548 "parser.cc"
    break;

  case 319: // infixexp: infixexp qop exp10
#line 1220 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < Hs::Exp > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4554 "parser.cc"
    break;

  case 320: // exp10: PREFIX_MINUS fexp
#line 1222 "parser.y"
                                        {yylhs.value.as < Hs::LExp > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Hs::LExp > ()} )};}
#line 4560 "parser.cc"
    break;

  case 321: // exp10: fexp
#line 1223 "parser.y"
                               {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4566 "parser.cc"
    break;

  case 324: // fexp: fexp aexp
#line 1231 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = make_parsed_app(yylhs.location, yystack_[1].value.as < Hs::LExp > (), yystack_[0].value.as < Hs::LExp > ());}
#line 4572 "parser.cc"
    break;

  case 325: // fexp: fexp "@" atype
#line 1232 "parser.y"
                                 {}
#line 4578 "parser.cc"
    break;

  case 326: // fexp: "static" aexp
#line 1233 "parser.y"
                                 {}
#line 4584 "parser.cc"
    break;

  case 327: // fexp: aexp
#line 1234 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4590 "parser.cc"
    break;

  case 328: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1237 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedAsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Hs::LExp > ())}; }
#line 4596 "parser.cc"
    break;

  case 329: // aexp: PREFIX_TILDE aexp
#line 1238 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedLazyPattern(yystack_[0].value.as < Hs::LExp > ())}; }
#line 4602 "parser.cc"
    break;

  case 330: // aexp: PREFIX_BANG aexp
#line 1239 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedStrictPattern(yystack_[0].value.as < Hs::LExp > ())}; }
#line 4608 "parser.cc"
    break;

  case 331: // aexp: "\\" apats1 "->" exp
#line 1240 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedLambda(yystack_[2].value.as < std::vector<Hs::LExp> > (),yystack_[0].value.as < Hs::LExp > ())}; }
#line 4614 "parser.cc"
    break;

  case 332: // aexp: "let" binds "in" exp
#line 1241 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Let(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Hs::LExp > ())}; }
#line 4620 "parser.cc"
    break;

  case 333: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1243 "parser.y"
                                                       {yylhs.value.as < Hs::LExp > () = {yystack_[7].location+yystack_[0].location,Hs::If(yystack_[6].value.as < Hs::LExp > (),yystack_[3].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::LExp > ())}; }
#line 4626 "parser.cc"
    break;

  case 334: // aexp: "if" ifgdpats
#line 1244 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::MultiWayIf(yystack_[0].value.as < std::vector<Hs::GuardedRHS> > ())}; }
#line 4632 "parser.cc"
    break;

  case 335: // aexp: "case" exp "of" altslist
#line 1245 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedCase(yystack_[2].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::ParsedAlts > ())}; }
#line 4638 "parser.cc"
    break;

  case 336: // aexp: "do" stmtlist
#line 1246 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4644 "parser.cc"
    break;

  case 337: // aexp: "mdo" stmtlist
#line 1247 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4650 "parser.cc"
    break;

  case 338: // aexp: aexp1
#line 1249 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4656 "parser.cc"
    break;

  case 339: // aexp1: aexp1 "{" fbinds "}"
#line 1252 "parser.y"
                                     { yylhs.value.as < Hs::LExp > () = make_record_expression(yylhs.location, yystack_[3].value.as < Hs::LExp > (), yystack_[1].value.as < Located<Hs::FieldBindings> > ()); }
#line 4662 "parser.cc"
    break;

  case 340: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1253 "parser.y"
                                     { yylhs.value.as < Hs::LExp > () = make_record_field_selection(yylhs.location, yystack_[2].value.as < Hs::LExp > (), yystack_[0].value.as < std::string > ()); }
#line 4668 "parser.cc"
    break;

  case 341: // aexp1: aexp2
#line 1254 "parser.y"
                                     { yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > (); }
#line 4674 "parser.cc"
    break;

  case 342: // aexp2: qvar
#line 1257 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4680 "parser.cc"
    break;

  case 343: // aexp2: qcon
#line 1258 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4686 "parser.cc"
    break;

  case 344: // aexp2: literal
#line 1259 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, yystack_[0].value.as < Hs::Exp > ()};}
#line 4692 "parser.cc"
    break;

  case 345: // aexp2: "(" texp ")"
#line 1260 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, unloc(yystack_[1].value.as < Hs::LExp > ())};}
#line 4698 "parser.cc"
    break;

  case 346: // aexp2: "(" tup_exprs ")"
#line 1261 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Hs::LExp> > ())};}
#line 4704 "parser.cc"
    break;

  case 347: // aexp2: "(" projection ")"
#line 1262 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = make_record_projection(yylhs.location, yystack_[1].value.as < std::vector<std::string> > ());}
#line 4710 "parser.cc"
    break;

  case 348: // aexp2: "[" list "]"
#line 1267 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, yystack_[1].value.as < Hs::Exp > ()};}
#line 4716 "parser.cc"
    break;

  case 349: // aexp2: "_"
#line 1268 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedWildcardPattern()};}
#line 4722 "parser.cc"
    break;

  case 350: // projection: projection TIGHT_INFIX_DOT field
#line 1271 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4728 "parser.cc"
    break;

  case 351: // projection: PREFIX_DOT field
#line 1272 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4734 "parser.cc"
    break;

  case 352: // texp: exp
#line 1277 "parser.y"
                      {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4740 "parser.cc"
    break;

  case 353: // texp: infixexp qop
#line 1278 "parser.y"
                      {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < Hs::Exp > ()} )}; }
#line 4746 "parser.cc"
    break;

  case 354: // texp: qopm infixexp
#line 1279 "parser.y"
                      {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < Hs::Exp > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4752 "parser.cc"
    break;

  case 355: // tup_exprs: tup_exprs "," texp
#line 1284 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LExp> > () = yystack_[2].value.as < std::vector<Hs::LExp> > (); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4758 "parser.cc"
    break;

  case 356: // tup_exprs: texp "," texp
#line 1285 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[2].value.as < Hs::LExp > ()); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4764 "parser.cc"
    break;

  case 357: // list: texp
#line 1303 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::List({yystack_[0].value.as < Hs::LExp > ()}); }
#line 4770 "parser.cc"
    break;

  case 358: // list: lexps
#line 1304 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::List(yystack_[0].value.as < std::vector<Hs::LExp> > ()); }
#line 4776 "parser.cc"
    break;

  case 359: // list: texp ".."
#line 1305 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFrom(yystack_[1].value.as < Hs::LExp > ()); }
#line 4782 "parser.cc"
    break;

  case 360: // list: texp "," exp ".."
#line 1306 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFromThen(yystack_[3].value.as < Hs::LExp > (),yystack_[1].value.as < Hs::LExp > ()); }
#line 4788 "parser.cc"
    break;

  case 361: // list: texp ".." exp
#line 1307 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFromTo(yystack_[2].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::LExp > ()); }
#line 4794 "parser.cc"
    break;

  case 362: // list: texp "," exp ".." exp
#line 1308 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFromThenTo(yystack_[4].value.as < Hs::LExp > (), yystack_[2].value.as < Hs::LExp > (), yystack_[0].value.as < Hs::LExp > ()); }
#line 4800 "parser.cc"
    break;

  case 363: // list: texp "|" squals
#line 1309 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListComprehension(yystack_[2].value.as < Hs::LExp > (), yystack_[0].value.as < std::vector<Hs::LStmt> > ()); }
#line 4806 "parser.cc"
    break;

  case 364: // lexps: lexps "," texp
#line 1311 "parser.y"
                                 { yylhs.value.as < std::vector<Hs::LExp> > () = yystack_[2].value.as < std::vector<Hs::LExp> > (); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4812 "parser.cc"
    break;

  case 365: // lexps: texp "," texp
#line 1312 "parser.y"
                                 { yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[2].value.as < Hs::LExp > ()); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4818 "parser.cc"
    break;

  case 366: // squals: squals "," qual
#line 1325 "parser.y"
                                          {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[2].value.as < std::vector<Hs::LStmt> > (); yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4824 "parser.cc"
    break;

  case 367: // squals: qual
#line 1327 "parser.y"
                                          {yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4830 "parser.cc"
    break;

  case 368: // guardquals: guardquals1
#line 1337 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[0].value.as < std::vector<Hs::LStmt> > ();}
#line 4836 "parser.cc"
    break;

  case 369: // guardquals1: guardquals1 "," qual
#line 1339 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[2].value.as < std::vector<Hs::LStmt> > ();yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4842 "parser.cc"
    break;

  case 370: // guardquals1: qual
#line 1340 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4848 "parser.cc"
    break;

  case 371: // altslist: "{" alts "}"
#line 1343 "parser.y"
                                 {yylhs.value.as < Hs::ParsedAlts > () = Hs::ParsedAlts{yystack_[1].value.as < std::vector<Located<Hs::ParsedAlt>> > ()};}
#line 4854 "parser.cc"
    break;

  case 372: // altslist: "vocurly" alts close
#line 1344 "parser.y"
                                 {yylhs.value.as < Hs::ParsedAlts > () = Hs::ParsedAlts{yystack_[1].value.as < std::vector<Located<Hs::ParsedAlt>> > ()};}
#line 4860 "parser.cc"
    break;

  case 373: // altslist: "{" "}"
#line 1345 "parser.y"
                                 {}
#line 4866 "parser.cc"
    break;

  case 374: // altslist: "vocurly" close
#line 1346 "parser.y"
                                 {}
#line 4872 "parser.cc"
    break;

  case 375: // alts: alts1
#line 1348 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[0].value.as < std::vector<Located<Hs::ParsedAlt>> > ();}
#line 4878 "parser.cc"
    break;

  case 376: // alts: ";" alts
#line 1349 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[0].value.as < std::vector<Located<Hs::ParsedAlt>> > ();}
#line 4884 "parser.cc"
    break;

  case 377: // alts1: alts1 ";" alt
#line 1351 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[2].value.as < std::vector<Located<Hs::ParsedAlt>> > (); yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > ().push_back(yystack_[0].value.as < Located<Hs::ParsedAlt> > ());}
#line 4890 "parser.cc"
    break;

  case 378: // alts1: alts1 ";"
#line 1352 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[1].value.as < std::vector<Located<Hs::ParsedAlt>> > ();}
#line 4896 "parser.cc"
    break;

  case 379: // alts1: alt
#line 1353 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > ().push_back(yystack_[0].value.as < Located<Hs::ParsedAlt> > ());}
#line 4902 "parser.cc"
    break;

  case 380: // alt: pat alt_rhs
#line 1355 "parser.y"
                                 {yylhs.value.as < Located<Hs::ParsedAlt> > () = Located<Hs::ParsedAlt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4908 "parser.cc"
    break;

  case 381: // alt_rhs: "->" exp wherebinds
#line 1357 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Hs::LExp > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4914 "parser.cc"
    break;

  case 382: // alt_rhs: gdpats wherebinds
#line 1358 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4920 "parser.cc"
    break;

  case 383: // gdpats: gdpats gdpat
#line 1360 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4926 "parser.cc"
    break;

  case 384: // gdpats: gdpat
#line 1361 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4932 "parser.cc"
    break;

  case 385: // ifgdpats: "{" gdpats "}"
#line 1363 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > ();}
#line 4938 "parser.cc"
    break;

  case 386: // ifgdpats: gdpats close
#line 1364 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > ();}
#line 4944 "parser.cc"
    break;

  case 387: // gdpat: "|" guardquals "->" exp
#line 1366 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Hs::LStmt> > (),yystack_[0].value.as < Hs::LExp > ()};}
#line 4950 "parser.cc"
    break;

  case 388: // pat: exp
#line 1368 "parser.y"
              {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4956 "parser.cc"
    break;

  case 389: // bindpat: exp
#line 1370 "parser.y"
              {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4962 "parser.cc"
    break;

  case 390: // apat: aexp
#line 1372 "parser.y"
              {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4968 "parser.cc"
    break;

  case 391: // apats1: apats1 apat
#line 1374 "parser.y"
                    {yylhs.value.as < std::vector<Hs::LExp> > () = yystack_[1].value.as < std::vector<Hs::LExp> > (); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4974 "parser.cc"
    break;

  case 392: // apats1: apat
#line 1375 "parser.y"
                    {yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4980 "parser.cc"
    break;

  case 393: // stmtlist: "{" stmts "}"
#line 1378 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Hs::LStmt> > ()};}
#line 4986 "parser.cc"
    break;

  case 394: // stmtlist: "vocurly" stmts close
#line 1379 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Hs::LStmt> > ()};}
#line 4992 "parser.cc"
    break;

  case 395: // stmts: stmts ";" stmt
#line 1381 "parser.y"
                       {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[2].value.as < std::vector<Hs::LStmt> > (); yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4998 "parser.cc"
    break;

  case 396: // stmts: stmts ";"
#line 1382 "parser.y"
                       {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[1].value.as < std::vector<Hs::LStmt> > ();}
#line 5004 "parser.cc"
    break;

  case 397: // stmts: stmt
#line 1383 "parser.y"
                       {yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 5010 "parser.cc"
    break;

  case 398: // stmts: %empty
#line 1384 "parser.y"
                       {}
#line 5016 "parser.cc"
    break;

  case 399: // stmt: qual
#line 1389 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = yystack_[0].value.as < Hs::LStmt > ();}
#line 5022 "parser.cc"
    break;

  case 400: // stmt: "rec" stmtlist
#line 1390 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 5028 "parser.cc"
    break;

  case 401: // qual: bindpat "<-" exp
#line 1392 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::ParsedPatQual(yystack_[2].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::LExp > ())};}
#line 5034 "parser.cc"
    break;

  case 402: // qual: exp
#line 1393 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Hs::LExp > ())};}
#line 5040 "parser.cc"
    break;

  case 403: // qual: "let" binds
#line 1394 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 5046 "parser.cc"
    break;

  case 404: // fbinds: fbinds1
#line 1399 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 5052 "parser.cc"
    break;

  case 405: // fbinds: %empty
#line 1400 "parser.y"
                        {}
#line 5058 "parser.cc"
    break;

  case 406: // fbinds1: fbind "," fbinds1
#line 1402 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5064 "parser.cc"
    break;

  case 407: // fbinds1: fbind
#line 1403 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5070 "parser.cc"
    break;

  case 408: // fbinds1: ".."
#line 1404 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = yylhs.location; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5076 "parser.cc"
    break;

  case 409: // fbind: qvar "=" texp
#line 1406 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Hs::LExp > ())}};}
#line 5082 "parser.cc"
    break;

  case 410: // fbind: qvar
#line 1407 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 5088 "parser.cc"
    break;

  case 411: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1408 "parser.y"
                                                      {}
#line 5094 "parser.cc"
    break;

  case 412: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1409 "parser.y"
                                                      {}
#line 5100 "parser.cc"
    break;

  case 415: // qcon: gen_qcon
#line 1445 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5106 "parser.cc"
    break;

  case 416: // qcon: sysdcon
#line 1446 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5112 "parser.cc"
    break;

  case 417: // gen_qcon: qconid
#line 1448 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5118 "parser.cc"
    break;

  case 418: // gen_qcon: "(" qconsym ")"
#line 1449 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5124 "parser.cc"
    break;

  case 419: // con: conid
#line 1451 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5130 "parser.cc"
    break;

  case 420: // con: "(" consym ")"
#line 1452 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5136 "parser.cc"
    break;

  case 421: // con: sysdcon
#line 1453 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5142 "parser.cc"
    break;

  case 422: // con_list: con_list "," con
#line 1455 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 5148 "parser.cc"
    break;

  case 423: // con_list: con
#line 1456 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 5154 "parser.cc"
    break;

  case 424: // sysdcon_no_list: "(" ")"
#line 1458 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 5160 "parser.cc"
    break;

  case 425: // sysdcon_no_list: "(" commas ")"
#line 1459 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5166 "parser.cc"
    break;

  case 426: // sysdcon_no_list: "(#" "#)"
#line 1460 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 5172 "parser.cc"
    break;

  case 427: // sysdcon_no_list: "(#" commas "#)"
#line 1461 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5178 "parser.cc"
    break;

  case 428: // sysdcon: sysdcon_no_list
#line 1463 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5184 "parser.cc"
    break;

  case 429: // sysdcon: "[" "]"
#line 1464 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 5190 "parser.cc"
    break;

  case 430: // conop: consym
#line 1466 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5196 "parser.cc"
    break;

  case 431: // conop: "`" conid "`"
#line 1467 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5202 "parser.cc"
    break;

  case 432: // qconop: qconsym
#line 1469 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5208 "parser.cc"
    break;

  case 433: // qconop: "`" qconid "`"
#line 1470 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5214 "parser.cc"
    break;

  case 434: // gtycon: ntgtycon
#line 1473 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5220 "parser.cc"
    break;

  case 435: // gtycon: "(" ")"
#line 1474 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 5226 "parser.cc"
    break;

  case 436: // gtycon: "(#" "#)"
#line 1475 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 5232 "parser.cc"
    break;

  case 437: // ntgtycon: oqtycon
#line 1477 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5238 "parser.cc"
    break;

  case 438: // ntgtycon: "(" commas ")"
#line 1478 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5244 "parser.cc"
    break;

  case 439: // ntgtycon: "(#" commas "#)"
#line 1479 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5250 "parser.cc"
    break;

  case 440: // ntgtycon: "(" "->" ")"
#line 1480 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 5256 "parser.cc"
    break;

  case 441: // ntgtycon: "[" "]"
#line 1481 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 5262 "parser.cc"
    break;

  case 442: // oqtycon: qtycon
#line 1483 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5268 "parser.cc"
    break;

  case 443: // oqtycon: "(" qtyconsym ")"
#line 1484 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5274 "parser.cc"
    break;

  case 444: // oqtycon_no_varcon: qtycon
#line 1486 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5280 "parser.cc"
    break;

  case 445: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1487 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5286 "parser.cc"
    break;

  case 446: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1488 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5292 "parser.cc"
    break;

  case 447: // oqtycon_no_varcon: "(" ":" ")"
#line 1489 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 5298 "parser.cc"
    break;

  case 448: // qtyconop: qtyconsym
#line 1492 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5304 "parser.cc"
    break;

  case 449: // qtyconop: "`" qtycon "`"
#line 1493 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5310 "parser.cc"
    break;

  case 450: // qtycondoc: qtycon
#line 1495 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5316 "parser.cc"
    break;

  case 451: // qtycon: "QCONID"
#line 1497 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5322 "parser.cc"
    break;

  case 452: // qtycon: tycon
#line 1498 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5328 "parser.cc"
    break;

  case 453: // tycon: "CONID"
#line 1502 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5334 "parser.cc"
    break;

  case 454: // qtyconsym: "QCONSYM"
#line 1504 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5340 "parser.cc"
    break;

  case 455: // qtyconsym: "QVARSYM"
#line 1505 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5346 "parser.cc"
    break;

  case 456: // qtyconsym: tyconsym
#line 1506 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5352 "parser.cc"
    break;

  case 457: // tyconsym: "CONSYM"
#line 1508 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5358 "parser.cc"
    break;

  case 458: // tyconsym: "VARSYM"
#line 1509 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5364 "parser.cc"
    break;

  case 459: // tyconsym: ":"
#line 1510 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5370 "parser.cc"
    break;

  case 460: // tyconsym: "-"
#line 1511 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 5376 "parser.cc"
    break;

  case 461: // op: varop
#line 1516 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5382 "parser.cc"
    break;

  case 462: // op: conop
#line 1517 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5388 "parser.cc"
    break;

  case 463: // varop: varsym
#line 1519 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5394 "parser.cc"
    break;

  case 464: // varop: "`" varid "`"
#line 1520 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5400 "parser.cc"
    break;

  case 465: // qop: qvarop
#line 1522 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5406 "parser.cc"
    break;

  case 466: // qop: qconop
#line 1523 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5412 "parser.cc"
    break;

  case 467: // qopm: qvaropm
#line 1526 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5418 "parser.cc"
    break;

  case 468: // qopm: qconop
#line 1527 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5424 "parser.cc"
    break;

  case 469: // qvarop: qvarsym
#line 1532 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5430 "parser.cc"
    break;

  case 470: // qvarop: "`" qvarid "`"
#line 1533 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5436 "parser.cc"
    break;

  case 471: // qvaropm: qvarsym_no_minus
#line 1535 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 5442 "parser.cc"
    break;

  case 472: // qvaropm: "`" qvarid "`"
#line 1536 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5448 "parser.cc"
    break;

  case 473: // tyvar: tyvarid
#line 1540 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5454 "parser.cc"
    break;

  case 474: // tyvarop: "`" tyvarid "`"
#line 1542 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5460 "parser.cc"
    break;

  case 475: // tyvarid: "VARID"
#line 1544 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5466 "parser.cc"
    break;

  case 476: // tyvarid: special_id
#line 1545 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5472 "parser.cc"
    break;

  case 477: // tyvarid: "unsafe"
#line 1546 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5478 "parser.cc"
    break;

  case 478: // tyvarid: "safe"
#line 1547 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5484 "parser.cc"
    break;

  case 479: // tyvarid: "interruptible"
#line 1548 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5490 "parser.cc"
    break;

  case 480: // var: varid
#line 1551 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5496 "parser.cc"
    break;

  case 481: // var: "(" varsym ")"
#line 1552 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5502 "parser.cc"
    break;

  case 482: // qvar: qvarid
#line 1554 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5508 "parser.cc"
    break;

  case 483: // qvar: "(" varsym ")"
#line 1555 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5514 "parser.cc"
    break;

  case 484: // qvar: "(" qvarsym1 ")"
#line 1556 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5520 "parser.cc"
    break;

  case 485: // field: varid
#line 1558 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5526 "parser.cc"
    break;

  case 486: // qvarid: varid
#line 1560 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5532 "parser.cc"
    break;

  case 487: // qvarid: "QVARID"
#line 1561 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5538 "parser.cc"
    break;

  case 488: // varid: "VARID"
#line 1563 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5544 "parser.cc"
    break;

  case 489: // varid: special_id
#line 1564 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5550 "parser.cc"
    break;

  case 490: // varid: "unsafe"
#line 1565 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5556 "parser.cc"
    break;

  case 491: // varid: "safe"
#line 1566 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5562 "parser.cc"
    break;

  case 492: // varid: "interruptible"
#line 1567 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5568 "parser.cc"
    break;

  case 493: // varid: "forall"
#line 1568 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5574 "parser.cc"
    break;

  case 494: // varid: "family"
#line 1569 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5580 "parser.cc"
    break;

  case 495: // varid: "role"
#line 1570 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5586 "parser.cc"
    break;

  case 496: // qvarsym: varsym
#line 1572 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5592 "parser.cc"
    break;

  case 497: // qvarsym: qvarsym1
#line 1573 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5598 "parser.cc"
    break;

  case 498: // qvarsym_no_minus: varsym_no_minus
#line 1575 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5604 "parser.cc"
    break;

  case 499: // qvarsym_no_minus: qvarsym1
#line 1576 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5610 "parser.cc"
    break;

  case 500: // qvarsym1: "QVARSYM"
#line 1578 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5616 "parser.cc"
    break;

  case 501: // varsym: varsym_no_minus
#line 1580 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5622 "parser.cc"
    break;

  case 502: // varsym: "-"
#line 1581 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5628 "parser.cc"
    break;

  case 503: // varsym_no_minus: "VARSYM"
#line 1583 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5634 "parser.cc"
    break;

  case 504: // varsym_no_minus: special_sym
#line 1584 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5640 "parser.cc"
    break;

  case 505: // special_id: "as"
#line 1586 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5646 "parser.cc"
    break;

  case 506: // special_id: "qualified"
#line 1587 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5652 "parser.cc"
    break;

  case 507: // special_id: "hiding"
#line 1588 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5658 "parser.cc"
    break;

  case 508: // special_id: "export"
#line 1589 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5664 "parser.cc"
    break;

  case 509: // special_id: "label"
#line 1590 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5670 "parser.cc"
    break;

  case 510: // special_id: "dynamic"
#line 1591 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5676 "parser.cc"
    break;

  case 511: // special_id: "stdcall"
#line 1592 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5682 "parser.cc"
    break;

  case 512: // special_id: "ccall"
#line 1593 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5688 "parser.cc"
    break;

  case 513: // special_id: "capi"
#line 1594 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5694 "parser.cc"
    break;

  case 514: // special_id: "prim"
#line 1595 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5700 "parser.cc"
    break;

  case 515: // special_id: "javascript"
#line 1596 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5706 "parser.cc"
    break;

  case 516: // special_id: "group"
#line 1597 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5712 "parser.cc"
    break;

  case 517: // special_id: "stock"
#line 1598 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5718 "parser.cc"
    break;

  case 518: // special_id: "anyclass"
#line 1599 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5724 "parser.cc"
    break;

  case 519: // special_id: "via"
#line 1600 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5730 "parser.cc"
    break;

  case 520: // special_id: "unit"
#line 1601 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5736 "parser.cc"
    break;

  case 521: // special_id: "dependency"
#line 1602 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5742 "parser.cc"
    break;

  case 522: // special_id: "signature"
#line 1603 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5748 "parser.cc"
    break;

  case 523: // special_sym: "."
#line 1605 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5754 "parser.cc"
    break;

  case 524: // special_sym: "*"
#line 1606 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5760 "parser.cc"
    break;

  case 525: // qconid: conid
#line 1610 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5766 "parser.cc"
    break;

  case 526: // qconid: "QCONID"
#line 1611 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5772 "parser.cc"
    break;

  case 527: // conid: "CONID"
#line 1613 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5778 "parser.cc"
    break;

  case 528: // qconsym: consym
#line 1615 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5784 "parser.cc"
    break;

  case 529: // qconsym: "QCONSYM"
#line 1616 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5790 "parser.cc"
    break;

  case 530: // consym: "CONSYM"
#line 1618 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5796 "parser.cc"
    break;

  case 531: // consym: ":"
#line 1619 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5802 "parser.cc"
    break;

  case 532: // literal: "CHAR"
#line 1623 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char32_t > ()});}
#line 5808 "parser.cc"
    break;

  case 533: // literal: "STRING"
#line 1624 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5814 "parser.cc"
    break;

  case 534: // literal: "INTEGER"
#line 1625 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5820 "parser.cc"
    break;

  case 535: // literal: "RATIONAL"
#line 1626 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5826 "parser.cc"
    break;

  case 536: // literal: "PRIMINTEGER"
#line 1627 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5832 "parser.cc"
    break;

  case 538: // close: error
#line 1635 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 5838 "parser.cc"
    break;

  case 539: // modid: "CONID"
#line 1639 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5844 "parser.cc"
    break;

  case 540: // modid: "QCONID"
#line 1640 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5850 "parser.cc"
    break;

  case 541: // commas: commas ","
#line 1642 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5856 "parser.cc"
    break;

  case 542: // commas: ","
#line 1643 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5862 "parser.cc"
    break;


#line 5866 "parser.cc"

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


  const short parser::yypact_ninf_ = -745;

  const short parser::yytable_ninf_ = -502;

  const short
  parser::yypact_[] =
  {
      79,   131,  -745,    88,  -745,  -745,  -745,  -745,  -745,   248,
      -9,    68,  -745,    70,   -34,   -34,    10,  -745,  -745,  -745,
    -745,   184,  -745,  -745,  -745,   171,  -745,   206,   291,  1486,
     368,   407,   284,  -745,   907,  -745,   119,  -745,  -745,  -745,
     131,  -745,   131,  -745,  -745,  -745,  -745,  -745,  -745,  -745,
    -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,
    -745,  -745,  -745,  -745,  -745,   537,  -745,  -745,  -745,  -745,
    -745,   325,    98,  -745,   354,  -745,  -745,  -745,  -745,  -745,
    -745,  -745,   308,  -745,   131,  -745,   375,  -745,  2760,  4768,
     437,   225,   266,   345,  2208,  -745,  -745,  -745,   403,   379,
    -745,  3772,   492,   345,  3450,  5111,   398,  3450,  3450,  3036,
    3450,  1932,  1794,   312,   400,  -745,  -745,  -745,  -745,  -745,
    -745,  -745,    38,   400,   402,   284,  -745,  -745,  -745,  -745,
    -745,    55,   101,  -745,  -745,   339,  -745,  3174,  -745,   299,
    -745,  -745,  -745,  -745,  -745,  -745,   436,   123,  -745,  -745,
    -745,  -745,   409,  -745,  -745,   442,  -745,  -745,  -745,  -745,
     443,  -745,   444,   448,   451,  -745,  -745,  -745,  4870,  -745,
    4907,  -745,  -745,  -745,  -745,   546,  -745,  1794,   550,   700,
    -745,  -745,  -745,  4768,  4768,  -745,  5213,  3962,  3564,   449,
    -745,   489,   485,  -745,   731,  -745,  4152,  -745,  -745,  -745,
    -745,  -745,  -745,  -745,  4768,   466,  -745,  4240,  -745,  -745,
    -745,  4768,   561,   562,  2484,  2484,  -745,  2898,   497,   468,
      43,  -745,  -745,   507,   515,   517,   518,  4240,  1351,  1351,
    -745,   582,  4768,  4768,   211,   125,   516,   445,   138,   222,
    -745,  -745,   207,   -16,   490,   278,  -745,   139,  -745,  -745,
    -745,  -745,  3312,  -745,  3174,  -745,  -745,  -745,  5074,  -745,
    -745,  -745,   700,    60,   500,   479,  -745,  2760,  -745,  -745,
    -745,  -745,  -745,  -745,  5368,  -745,  -745,   268,   -15,    82,
     448,   488,   501,   502,   108,  -745,   333,   -22,  5111,  -745,
    4240,  5111,  5111,  -745,   314,   375,   542,   483,  4768,  4240,
    5213,  2760,  2898,  5074,  -745,    33,  -745,  -745,  2760,  -745,
    -745,  -745,  -745,  4768,  -745,  5368,   842,  3450,  -745,  -745,
    -745,  -745,  -745,  -745,  -745,   505,   506,   509,  -745,   520,
      70,   131,    29,   382,  4240,  -745,  -745,   357,   146,   521,
     510,  -745,  -745,  -745,  -745,   512,   552,   544,  -745,  -745,
     525,  -745,  -745,  -745,  -745,  -745,  -745,   526,   524,   536,
    -745,   276,   343,  -745,   624,  4768,  4240,  1592,  4768,  -745,
    -745,  -745,  4768,  -745,  -745,   571,  4240,  -745,  -745,  -745,
    -745,  4240,  4240,   379,   345,   574,   575,   132,  -745,  -745,
      41,   570,   545,  -745,     2,  -745,   646,  -745,  -745,  -745,
    -745,  -745,  -745,   645,   183,  -745,  -745,   339,    62,  2760,
    -745,   595,   452,   160,    37,  4240,   211,  4240,  -745,  -745,
    -745,   547,  -745,   605,   573,   297,   398,   606,  2760,  -745,
     564,   567,  2760,  2760,  2898,  2070,  -745,  2070,   888,  -745,
    -745,  5368,  -745,  -745,  2070,  -745,  2070,   149,  -745,  -745,
    -745,  -745,   557,   584,   619,   620,   618,   621,  5176,   583,
    -745,  -745,  -745,  -745,  -745,  4328,   -10,   454,  -745,  -745,
    -745,  -745,   677,   623,   593,   379,  -745,  -745,  -745,  -745,
    -745,  -745,   609,  -745,   596,   638,   625,   626,  -745,  -745,
    -745,  5009,  -745,  -745,  -745,   628,  1527,  -745,  -745,  2346,
    1656,  -745,  -745,   629,  4240,  -745,  5213,  5405,  -745,  4240,
    4240,  -745,  -745,  4240,  -745,  -745,  -745,   612,  -745,  5559,
     383,  -745,  -745,  -745,   613,   616,   665,  -745,  4240,  -745,
    -745,   634,   622,  -745,  -745,   582,  -745,  2760,  -745,  2484,
    -745,  2760,  2898,  -745,  2760,   388,  -745,  -745,  1351,  -745,
    -745,  4240,  4240,  5518,   648,  -745,  -745,  -745,    37,  -745,
    -745,  -745,  -745,  -745,  5213,  -745,  -745,   640,   919,   348,
    -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,   631,  -745,
     659,  -745,  -745,  -745,  -745,  -745,   644,  -745,  -745,  -745,
    4240,  4240,   637,   639,   314,  -745,   454,   647,  -745,  -745,
     683,  4240,   734,   738,   759,  -745,  2760,  -745,  -745,  -745,
     842,  2070,  5368,  -745,  1527,   131,  -745,   354,   661,   103,
    -745,  -745,  2622,  -745,   671,   664,  -745,   417,    70,  -745,
    -745,  -745,  -745,  4240,  5654,  5654,  -745,  -745,  -745,  -745,
    -745,   673,  -745,  -745,  -745,  1213,  1213,  -745,  -745,  -745,
    -745,  -745,  4240,  -745,  -745,  -745,  -745,   468,  1067,  1067,
    -745,  -745,  -745,  -745,  -745,  5654,   758,  -745,   707,  -745,
    -745,  2898,  2760,  -745,  -745,    16,    17,  -745,  -745,  -745,
    5273,   738,   759,  4768,  -745,  -745,  -745,   705,  -745,  4768,
     414,   759,   317,  -745,   759,  -745,  -745,  -745,  -745,     7,
    -745,   679,  -745,  -745,  -745,  4972,  -745,  -745,  -745,  2760,
    2760,  -745,    46,  -745,  -745,     4,   717,  -745,  -745,  5654,
     762,  4064,  -745,  -745,   188,  -745,    69,  -745,   789,  -745,
     782,  -745,   782,  -745,   240,  -745,    71,  -745,   715,   423,
    -745,  4240,  -745,  -745,  -745,  4240,  -745,  4768,  4768,   759,
    -745,  -745,  5442,   734,   711,  3668,  -745,  -745,  -745,   241,
      78,  -745,  4416,   260,   750,  -745,  -745,  -745,  2070,  5368,
    -745,  -745,  -745,   677,  -745,  4240,  -745,  4240,  -745,  4768,
    4768,  4768,  -745,   459,  -745,  1213,  -745,  2760,  -745,  4768,
     542,  -745,  1067,  -745,  5654,  4504,  4592,  -745,  -745,  -745,
    -745,   714,   665,  -745,  -745,  -745,  4768,   682,  -745,  4768,
     702,   690,  -745,   398,    70,  -745,  -745,   692,   696,  -745,
    -745,  -745,  -745,  -745,   708,   699,   571,  -745,   467,  4240,
    4680,  -745,  -745,  -745,  -745,  4328,  -745,  5654,  -745,   710,
     255,  -745,    70,    91,  4768,  3860,  -745,  4768,  -745,   468,
     150,  -745,  4768,  -745,  -745,  -745,  -745,  5617,  -745,  -745,
    3564,   735,   737,   454,  -745,  -745,  -745,  4768,  -745,  -745,
    -745,  -745,  4240,  -745,   717,  5654,   738,   759,  -745,  -745,
    -745,   759,  -745,  -745
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   539,   540,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   538,   537,    12,   199,   195,     0,     0,    20,
       0,    46,    41,    15,    14,   198,     0,     6,     7,   505,
       0,   507,     0,   506,   493,   508,   509,   510,   491,   492,
     490,   494,   495,   511,   512,   513,   514,   515,   516,   517,
     518,   519,   520,   522,   521,     0,   488,   453,   487,   451,
      22,     0,    19,    24,    28,    36,   444,   452,    35,   482,
     486,   489,     0,    45,     0,    38,    42,   349,     0,     0,
     137,   139,     0,     0,     0,    63,    64,    65,   101,     0,
     138,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   312,   527,   526,   532,   533,   534,
     535,   536,   312,   312,    61,    68,    71,    72,    73,    74,
      75,   159,     0,    78,   295,    79,   318,   321,   327,   338,
     341,   343,   415,   428,   416,   205,   342,   486,   417,   525,
     344,   196,     0,    27,    26,     0,   502,   524,   523,   503,
       0,   500,     0,     0,     0,   501,   504,    17,     0,    21,
      31,    25,    40,    40,     3,    48,    37,     0,     0,   317,
     478,   479,   477,     0,     0,   233,   280,     0,     0,     0,
     475,   255,     0,   152,   216,   219,   220,   224,   231,   437,
     442,   232,   473,   476,     0,     0,   140,     0,   104,   102,
     103,     0,     0,     0,   398,   398,   336,     0,     0,   323,
       0,   334,   384,     0,     0,     0,     0,     0,   189,   189,
     192,     0,     0,     0,     0,     0,     0,   216,   437,     0,
     337,   326,     0,     0,     0,     0,   423,   200,   421,   419,
     390,   392,     0,   329,   320,   330,   531,   429,     0,   530,
     529,   352,   317,   357,     0,   358,   468,     0,   467,   471,
     499,   498,   432,   528,     0,   424,   542,     0,     0,     0,
     499,     0,   498,   432,     0,   426,     0,     0,     0,   313,
       0,     0,     0,    62,     0,    69,   159,     0,     0,     0,
       0,     0,     0,     0,   296,   194,   301,   466,     0,   465,
     469,   497,   496,     0,   324,     0,   405,     0,   197,   447,
     446,   445,   484,   483,    23,     0,     0,    32,    34,     0,
       0,     0,    50,     0,     0,   235,   234,     0,     0,     0,
     281,   283,   480,   249,   441,     0,   208,     0,   212,   459,
       0,   460,   237,   458,   457,   455,   454,   246,     0,     0,
     456,     0,     0,   261,   175,     0,     0,     0,     0,   228,
     448,   229,     0,   225,   227,   143,   245,   241,   202,   106,
     105,     0,     0,     0,     0,   402,     0,     0,   397,   399,
       0,     0,   368,   370,     0,   322,     0,   383,   386,    98,
      97,    99,   100,   185,     0,   297,   188,     0,     0,     0,
      94,     0,   145,     0,   160,     0,     0,     0,    80,    81,
      82,     0,   307,     0,     0,     0,     0,     0,     0,   391,
       0,     0,   353,   359,     0,     0,   348,     0,   354,   351,
     485,     0,   347,   345,     0,   346,     0,   483,   418,   425,
     541,   427,     0,     0,     0,     0,     0,     0,     0,   304,
     462,    67,   461,   463,   430,     0,     0,   141,   303,   213,
     203,   204,   194,     0,     0,     0,   299,   300,   319,   325,
     340,   408,     0,   404,   407,   410,     0,   486,   328,    30,
      29,     0,     9,    10,    47,     0,    54,    44,    49,     0,
       0,   335,   316,     0,     0,   236,     0,     0,   239,     0,
       0,   440,   240,     0,   443,   438,   439,   256,   258,     0,
       0,    83,   151,   217,     0,     0,   221,   226,     0,    89,
     246,     0,   244,   107,   108,   403,   400,     0,   393,   396,
     394,     0,     0,   385,     0,     0,    93,   190,   187,   191,
     332,     0,     0,     0,   109,   165,   164,    84,   161,   162,
     262,    90,    91,    85,     0,   308,   420,     0,     0,     0,
     201,   434,   422,   305,   331,   472,   433,   361,   363,   367,
     352,   365,   364,   350,   356,   355,     0,   314,   306,   311,
       0,     0,     0,     0,     0,   249,   141,     0,   156,   158,
       0,     0,   277,   265,   285,   298,     0,   470,   193,   339,
       0,     0,     0,    33,    54,     0,    56,    28,     0,    53,
      58,   373,     0,   388,     0,   375,   379,     0,     0,   374,
     481,   284,   282,     0,     0,     0,   248,   250,   253,   209,
     211,   247,   261,   261,   260,   171,   171,   174,   449,   474,
     144,    76,     0,   401,   395,   387,   369,   323,   181,   181,
     184,   186,   124,   146,   147,     0,   114,   163,     0,   435,
     436,     0,   360,   315,   206,     0,     0,   464,   431,    66,
       0,   265,   285,     0,   157,   142,   249,   271,   273,     0,
       0,   285,     0,    86,   286,   288,   302,   406,   409,   412,
     414,     0,    60,    59,    51,     0,    55,   376,   371,   378,
       0,   380,   194,   372,   210,     0,     0,   238,   257,   259,
     130,     0,   166,   170,     0,   167,     0,   247,     0,   137,
     132,   176,   132,   180,     0,   177,     0,   110,     0,     0,
      88,     0,   366,   362,   309,     0,   310,     0,     0,   285,
      95,   155,     0,   277,     0,   278,   222,   230,   275,   323,
     323,    87,     0,     0,   289,   292,   450,   287,     0,     0,
      52,    57,   377,   194,   382,     0,   251,     0,   131,     0,
       0,     0,   128,   148,   172,   169,   173,     0,   133,     0,
     159,   182,   179,   183,     0,   123,   123,   115,    77,   207,
     154,     0,   214,    96,   276,   272,     0,     0,   223,     0,
       0,   266,   269,     0,     0,   293,   218,   242,     0,   290,
     291,   411,   413,   381,     0,     0,   143,   129,   148,     0,
       0,   126,   168,   333,   134,     0,   178,   111,   113,     0,
       0,   122,     0,     0,     0,   278,   274,   279,   263,   323,
       0,   264,     0,   294,   252,   254,   125,     0,   127,   149,
       0,     0,   232,   141,   112,   118,   116,   121,   119,   117,
     153,   268,     0,   243,   232,     0,   265,   285,   120,   270,
     150,   285,   135,   136
  };

  const short
  parser::yypgoto_[] =
  {
    -745,  -745,  -745,  -745,  -745,  -745,  -745,    42,  -745,  -745,
    -745,  -745,   651,   212,  -745,  -745,    -6,   703,  -745,  -745,
    -745,  -745,  -745,  -745,  -745,  -745,   210,  -745,   130,  -745,
    -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,  -745,
    -745,  -745,   144,    73,  -745,  -745,   -37,  -745,  -745,  -745,
      44,  -221,  -745,  -745,   107,  -745,   807,  -745,  -564,    18,
    -745,    14,   549,     8,  -274,  -745,  -745,   287,    63,   204,
    -745,  -745,    61,   196,  -745,  -745,   642,  -745,  -329,  -445,
     845,  -745,  -745,  -318,   122,  -162,   296,  -181,   355,  -745,
     -82,  -745,   -89,  -745,  -100,  -745,  -389,  -745,  -745,  -745,
    -690,   -30,  -188,    15,  -745,   493,  -552,   335,  -744,  -745,
    -745,   236,   247,  -472,  -652,   134,  -745,    52,  -563,  -745,
     142,  -745,    85,  -745,  -745,   386,  -633,  -745,   208,   140,
     870,  -214,  -745,  -745,   600,  -745,   430,  -745,    47,   -21,
    -273,  -210,   799,   145,  -745,  -745,  -745,   -44,  -745,  -745,
    -745,  -745,   607,  -745,  -745,  -455,  -745,   209,  -745,  -198,
    -745,  -194,  -745,  -745,   670,  -745,   -75,   713,   391,  -199,
    -745,   322,  -745,  -745,  -745,  -745,   508,   135,  -745,  -101,
    -707,   -97,  -745,   527,   -71,  -745,  -745,  -745,    21,  -745,
    -191,  -745,   359,  -745,   697,  -745,  -745,  -745,  -423,  -745,
    -346,  -266,    35,  -249,  -182,    -3,  -745,  -745,   -29,   -57,
     -60,    64,  -745,  -177,   -96,   -53,  -241,  -745,  -113,   -23,
    -111
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   174,     6,    10,    19,    30,
      71,    72,    73,   171,   326,   327,   617,    86,    11,    20,
      21,    32,    84,   332,   497,   498,   618,   619,   620,   294,
     124,   459,    33,    34,   125,   421,   126,   127,   128,   235,
     129,   227,   212,   213,   130,   666,   737,   837,   740,   797,
     840,   841,   722,   779,   789,   731,   732,   205,   603,   529,
     554,   831,   191,   596,   298,   557,   558,   559,   723,   724,
     647,   521,   733,   734,   660,   546,   404,   230,   231,   476,
      27,    36,   427,   377,   468,   132,   675,   357,   378,   470,
     347,   754,   348,   817,   194,   195,   755,   196,   373,   368,
     756,   197,   379,   818,   531,   358,   507,   636,   637,   364,
     517,   518,   519,   561,   691,   810,   811,   812,   604,   687,
     688,   689,   758,   339,   340,   341,   693,   694,   695,   764,
     405,   725,   304,   305,   306,   134,   288,   289,   261,   179,
     136,   813,   137,   138,   139,   140,   277,   278,   279,   264,
     265,   578,   391,   392,   501,   624,   625,   626,   711,   220,
     221,   222,   627,   386,   251,   252,   216,   387,   388,   389,
     482,   483,   484,   699,   141,   142,   246,   247,   143,   144,
     460,   266,   570,   198,   199,    75,   369,   765,   200,    77,
     359,   360,   461,   462,   308,   267,   309,   268,   201,   371,
     202,   145,   146,   486,    79,    80,   310,   269,   270,   312,
     165,   203,   166,   148,   149,   272,   273,   150,    24,     9,
     284
  };

  const short
  parser::yytable_[] =
  {
     193,   237,   286,   370,   424,   248,   345,   192,   164,   396,
     249,   410,   236,   135,   406,   406,   502,   153,   393,   154,
     394,   525,   465,    74,   338,   439,   397,   605,   240,   749,
     238,   147,   681,   682,   471,   478,   163,   639,   307,   403,
     555,   495,    22,   680,    22,   628,   370,    13,   809,   750,
      76,   271,   282,   464,   535,   281,   650,   290,   761,   283,
     475,   175,   422,    22,    78,   808,   480,   263,   599,   452,
      22,    22,    22,   475,   296,    25,   430,   361,   362,  -267,
     663,   431,   307,   280,   638,   775,   861,   217,    12,   768,
     262,   262,    22,    81,   744,   746,   644,    17,    81,   443,
       1,    26,   455,   393,   237,   444,   311,   398,   543,   769,
     776,   237,   206,   861,   453,   375,   803,   282,   302,   423,
     164,   474,   380,    29,   283,   600,   431,   297,   217,   685,
     638,   217,   237,   237,   752,   178,   745,   745,   809,   433,
     243,   219,   496,   411,   412,   434,   608,   287,   280,    23,
     311,    23,    81,   335,   336,   808,   262,   808,    81,   478,
     539,   556,    74,   414,   328,   307,   374,   707,    81,    81,
      23,    81,    81,    81,    81,    81,    81,    23,    23,    23,
     435,   548,   299,   342,     2,   164,  -267,    18,   785,    76,
     792,    76,   583,   533,   534,   530,   445,   395,    31,    23,
     397,    81,   446,    78,  -480,    78,   415,   407,   407,   193,
     867,   715,   716,   163,   329,   330,   192,   493,   168,   -92,
     425,   300,   449,   705,   881,   147,   147,   504,   450,   151,
    -481,   872,    81,   311,    81,   579,   563,   463,   538,   152,
     349,    81,   169,  -480,   882,   416,   438,   706,   883,   241,
      81,   539,   250,   253,   351,   255,     7,   638,   -92,   426,
       8,   385,   385,   418,   385,   237,   300,   774,   526,  -481,
     426,   440,   674,   674,   419,   420,   522,   540,    81,    81,
     503,    81,   314,   479,    37,   207,   353,   354,   208,   547,
     355,   356,    81,    81,   784,   549,   644,   342,   668,   876,
     877,   156,   548,   824,   157,   825,    35,   785,   494,   536,
     307,   158,   440,   487,    14,    15,    81,   209,    81,   738,
     210,   211,    81,   454,   413,   248,   456,   457,   823,   638,
     249,    81,   641,   159,   661,   370,    67,   161,    81,   208,
      69,   307,   527,   656,   338,   562,   791,  -267,   472,   385,
      67,   485,    81,   464,    69,    81,    81,   859,   256,   792,
     395,   866,   593,   700,    81,    81,    81,    81,   209,    38,
     441,   210,    81,   762,   867,   271,   598,   271,   311,    81,
      81,    81,   442,   597,   271,    67,   271,   629,   524,    69,
     515,   581,   275,   582,   256,    82,   450,   250,   276,   314,
     584,   315,   585,    85,   316,   259,   567,   862,   156,   311,
     568,   157,   569,   172,   262,   173,   262,   845,   158,   256,
     847,   301,    67,   262,   302,   262,    69,   799,   285,   712,
     762,    83,   276,   156,   638,   458,   157,   874,   440,   167,
     159,   259,    67,   158,   735,   735,    69,   728,   838,   451,
     214,   156,   215,   450,   157,   592,   550,   361,   362,   516,
     303,   158,   488,   450,   670,   159,   259,   170,   276,   161,
     260,   727,   742,    81,   204,   574,   223,   224,   225,   226,
     577,   385,   580,   159,   228,   613,   229,   499,   645,   500,
     646,   864,    81,   658,   176,   659,    81,    81,    81,    81,
     782,    81,   217,   342,   710,    81,   239,   244,    81,   287,
      81,   245,    76,   113,   464,   713,   835,    76,   397,   759,
     822,   760,    81,   115,   317,   349,    78,   407,   795,   738,
     796,    78,   366,   552,   553,   601,   602,   463,   293,   351,
     829,   830,   346,   346,   318,   147,   623,   623,   829,   857,
     331,   271,   291,   292,   879,    81,   319,   320,   321,   827,
      81,   342,   322,    81,    81,   323,   367,   698,   834,   276,
      81,   353,   354,   333,   363,   355,   356,   365,   735,   376,
     381,   382,   217,   237,   653,   399,   385,   395,   655,   385,
     262,   657,   702,   400,   751,   401,   402,   409,   417,   437,
     257,    81,   447,    81,   464,    81,    81,   487,    81,   440,
     436,   370,    81,   786,   297,  -501,   448,   155,   466,   489,
     490,   237,   508,   793,   407,   407,   492,   505,    81,   491,
     506,   156,   783,   509,   157,    76,   510,   407,   407,   511,
     512,   158,   147,   147,   513,   485,   878,   237,   802,    78,
     514,   520,   528,   696,   469,   147,   147,   541,   800,   757,
    -389,   537,   237,   159,   160,   542,   801,   161,   162,   623,
      81,   544,   545,   816,    81,    81,    81,   551,    81,   237,
     237,   237,   564,   565,   573,   575,    81,   566,   576,   237,
     826,   411,   828,   586,   587,   237,   237,   588,   589,   590,
     411,   851,   591,   594,   475,   606,   411,   411,   271,    81,
      81,   593,   248,   766,   607,   609,   610,   249,   385,   743,
     611,   523,    81,    81,   821,   757,    76,   612,  -485,   868,
     869,   346,   642,   665,   648,    81,    81,   649,   672,   683,
      78,   614,   652,   630,   237,   349,   598,   262,   651,   361,
     344,   671,   237,   597,   673,   870,   623,   773,   677,   351,
     678,   684,   686,   816,   407,   690,   440,   237,   692,    81,
     560,   407,   346,    81,    81,   704,   757,   708,   411,   757,
     256,   334,   147,   709,   766,   739,   367,   717,   741,   147,
     753,   353,   354,   770,   156,   355,   356,   157,   777,   778,
     787,   788,   794,   806,   158,   211,   844,   115,   848,   849,
     853,   349,   852,   855,   854,   757,   865,   757,   366,   324,
     875,   303,  -253,  -214,   701,   351,   159,   259,   295,   703,
     161,   260,    81,    81,   833,   771,   763,   820,   880,   790,
     843,   131,   858,   863,   856,   667,    39,   467,   832,    81,
     726,    81,   367,   836,    41,   736,    81,   353,   354,   631,
      28,   355,   356,   798,   560,   640,    43,   873,   346,   532,
      44,   408,    45,    46,    47,    48,    49,    50,   718,    51,
      52,    53,    54,   560,    55,    56,    57,   676,   664,    58,
     719,   846,   632,    59,   814,   805,    60,    61,    62,    63,
      64,   871,   767,   819,   133,   477,   662,   560,   254,   473,
      87,    39,    88,    89,    90,    91,    92,    93,   772,    41,
      94,   481,   429,    95,    96,    97,    98,    99,   390,   100,
     654,    43,   697,   101,   572,    44,   102,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,   850,    55,
      56,    57,   571,   679,    58,   242,   560,   104,    59,   432,
       0,    60,    61,    62,    63,    64,    66,     0,   256,     0,
      68,   105,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   156,     0,   106,   157,     0,     0,   714,     0,
     107,     0,   158,     0,     0,     0,     0,   108,     0,   349,
     109,     0,   110,     0,     0,     0,   350,   346,     0,   303,
       0,     0,     0,   351,   159,   259,   111,     0,   161,   260,
     112,     0,   113,     0,     0,     0,     0,     0,     0,     0,
     114,    66,   115,   669,     0,    68,   116,     0,     0,   276,
       0,   117,   118,   119,   120,   353,   354,   121,     0,   355,
     356,     0,   122,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      87,    39,    88,     0,   729,     0,     0,    93,     0,    41,
      94,     0,     0,    95,    96,    97,     0,    99,     0,   100,
       0,    43,     0,   730,     0,    44,   469,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
     560,   105,   560,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     112,     0,   113,     0,   560,   662,     0,     0,     0,     0,
     114,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,     0,   121,     0,     0,
       0,     0,   122,   123,     0,   346,    87,    39,    88,     0,
     720,     0,     0,    93,     0,    41,    94,     0,     0,    95,
      96,    97,     0,    99,     0,     0,     0,    43,     0,   721,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   104,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,   105,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     106,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   112,     0,   113,     0,
       0,     0,     0,     0,     0,     0,   114,    66,   115,     0,
       0,    68,   116,     0,     0,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,   122,   123,
       0,    93,     0,    41,    94,     0,     0,    95,    96,    97,
       0,    99,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,   104,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,   105,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   106,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   112,     0,   113,     0,     0,     0,
       0,     0,     0,     0,   114,    66,   115,     0,     0,    68,
     116,     0,     0,     0,     0,   117,   118,   119,   120,     0,
      39,   121,     0,     0,    40,     0,   122,   123,    41,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,    39,     0,    58,     0,     0,     0,    59,     0,    41,
      60,    61,    62,    63,    64,     0,     0,     0,   615,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,    65,
       0,     0,     0,     0,    41,     0,     0,     0,     0,     0,
      66,    67,     0,     0,    68,    69,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,     0,
      70,    53,    54,     0,    55,    56,    57,     0,     0,    58,
      65,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,    66,    67,     0,     0,    68,    69,    22,     0,    87,
      39,    88,     0,     0,     0,     0,    93,     0,    41,    94,
       0,   616,     0,     0,     0,     0,    99,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   104,    59,     0,     0,
      60,    61,    62,    63,    64,     0,   190,    67,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   109,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    23,   111,     0,     0,     0,   177,
       0,   113,     0,     0,     0,   622,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,     0,     0,     0,
     117,   118,   119,   120,     0,     0,   121,    87,    39,    88,
       0,     0,     0,     0,    93,     0,    41,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,   104,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   256,     0,     0,   107,     0,     0,
       0,     0,     0,     0,   108,     0,     0,   109,   156,   110,
       0,   157,     0,     0,     0,     0,     0,   274,   158,     0,
       0,     0,     0,   111,     0,     0,     0,   177,   275,   113,
       0,     0,     0,     0,   276,   258,     0,     0,    66,   115,
     159,   259,    68,   116,   161,   260,     0,     0,   117,   118,
     119,   120,     0,     0,   121,    87,    39,    88,     0,     0,
       0,     0,    93,     0,    41,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   104,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   256,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,     0,     0,   109,     0,   110,     0,   157,
       0,     0,     0,     0,     0,     0,   158,     0,     0,     0,
       0,   111,   257,     0,     0,   177,     0,   113,     0,     0,
       0,     0,     0,   258,     0,     0,    66,   115,   159,   259,
      68,   116,   161,   260,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,     0,     0,     0,
      93,     0,    41,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     104,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     256,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,   109,     0,   110,     0,   157,     0,     0,
       0,     0,     0,     0,   158,     0,     0,     0,     0,   111,
       0,     0,     0,   177,     0,   113,     0,     0,     0,     0,
       0,   258,     0,     0,    66,   115,   159,   259,    68,   116,
     161,   260,     0,     0,   117,   118,   119,   120,     0,     0,
     121,    87,    39,    88,     0,     0,     0,     0,    93,     0,
      41,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   104,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,   217,     0,     0,     0,     0,   108,     0,
       0,   109,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   218,     0,     0,     0,   111,     0,     0,
       0,   177,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,     0,     0,   121,    87,
      39,    88,     0,     0,     0,     0,    93,     0,    41,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   104,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   109,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   621,     0,     0,   111,     0,     0,     0,   177,
       0,   113,     0,     0,     0,   622,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,     0,     0,     0,
     117,   118,   119,   120,     0,     0,   121,    87,    39,    88,
       0,     0,     0,     0,    93,     0,    41,    94,     0,     0,
       0,     0,     0,     0,   383,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,     0,    55,    56,    57,     0,
     384,    58,     0,     0,   104,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,     0,   108,     0,     0,   109,     0,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,   177,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,     0,   117,   118,
     119,   120,     0,     0,   121,    87,    39,    88,     0,     0,
       0,     0,    93,     0,    41,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,   104,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,   108,     0,     0,   109,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,   177,     0,   113,     0,     0,
       0,   622,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,     0,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,     0,     0,     0,
      93,     0,    41,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     104,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,   109,     0,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   177,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,     0,     0,
     121,    87,    39,    88,     0,     0,     0,     0,    93,     0,
      41,    94,     0,     0,     0,     0,     0,     0,   383,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   104,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,     0,
       0,   109,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,   177,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,     0,     0,   121,    87,
      39,    88,     0,     0,     0,     0,    93,     0,    41,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      43,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   104,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,   177,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,     0,     0,     0,
     117,   118,   119,   120,     0,     0,   121,    87,    39,    88,
       0,     0,     0,     0,    93,     0,    41,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     103,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   107,     0,     0,
       0,     0,     0,   313,   108,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,   177,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,     0,   117,   118,
     119,   120,     0,     0,   121,    87,    39,    88,     0,     0,
       0,     0,    93,     0,    41,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    43,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,     0,     0,     0,   428,
       0,     0,   108,     0,     0,     0,     0,   110,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,   177,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,     0,     0,     0,   117,   118,   119,   120,
       0,     0,   121,    87,    39,    88,     0,     0,     0,     0,
      93,     0,    41,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    43,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   177,     0,   113,     0,     0,    39,     0,
       0,     0,     0,     0,    66,   115,    41,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,    43,     0,
     121,     0,   343,     0,    45,    46,    47,   180,   181,   182,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   349,     0,     0,     0,     0,     0,
       0,   350,     0,     0,   183,     0,     0,     0,   351,   184,
       0,   185,     0,     0,     0,     0,     0,     0,     0,   186,
       0,     0,    39,   187,     0,     0,     0,   188,   352,   189,
      41,     0,     0,     0,   276,     0,     0,     0,   190,    67,
     353,   354,    43,    69,   355,   356,     0,     0,    45,    46,
      47,   180,   181,   182,     0,     0,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   256,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
    -215,     0,     0,   184,     0,   185,     0,     0,     0,     0,
       0,     0,     0,   186,     0,     0,    39,   187,     0,     0,
       0,   188,     0,   189,    41,     0,     0,     0,     0,   807,
       0,   232,   190,    67,     0,   259,    43,    69,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,   233,
     234,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,    39,     0,     0,   184,     0,   185,
       0,     0,    41,     0,     0,     0,     0,   186,     0,     0,
       0,   187,     0,     0,    43,   188,     0,   189,     0,     0,
      45,    46,    47,   180,   181,   182,   190,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     256,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,     0,     0,     0,   184,     0,   185,     0,     0,
       0,     0,     0,     0,     0,   186,    39,     0,     0,   187,
       0,     0,     0,   188,    41,   189,     0,     0,     0,     0,
       0,   807,     0,     0,   190,    67,    43,   259,     0,    69,
     343,     0,    45,    46,    47,   180,   181,   182,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,     0,     0,     0,   184,     0,   185,
       0,     0,     0,     0,     0,     0,     0,   186,    39,     0,
       0,   187,   344,     0,     0,   188,    41,   189,     0,     0,
       0,     0,     0,   780,     0,     0,   190,    67,    43,     0,
       0,    69,     0,     0,    45,    46,    47,   180,   181,   182,
       0,   781,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,    39,     0,     0,   184,
       0,   185,     0,     0,    41,     0,     0,     0,     0,   186,
       0,     0,     0,   187,     0,     0,    43,   188,     0,   189,
       0,     0,    45,    46,    47,   180,   181,   182,   190,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   372,   183,     0,    39,     0,     0,   184,     0,   185,
       0,     0,    41,     0,     0,     0,     0,   186,     0,     0,
       0,   187,     0,     0,    43,   188,     0,   189,   343,     0,
      45,    46,    47,   180,   181,   182,   190,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,    39,     0,     0,   184,     0,   185,     0,     0,
      41,     0,     0,     0,     0,   186,     0,     0,     0,   187,
       0,     0,    43,   188,     0,   189,   595,     0,    45,    46,
      47,   180,   181,   182,   190,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
      39,     0,     0,   184,     0,   185,     0,     0,    41,     0,
       0,     0,     0,   186,     0,     0,     0,   187,     0,     0,
      43,   188,     0,   189,     0,     0,    45,    46,    47,   180,
     181,   182,   190,    67,     0,    53,    54,    69,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   183,     0,    39,     0,
       0,   184,     0,   185,     0,     0,    41,     0,     0,     0,
       0,   186,     0,     0,     0,   187,     0,     0,    43,   188,
     815,   189,     0,     0,    45,    46,    47,   180,   181,   182,
     190,    67,     0,    53,    54,    69,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   839,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   183,     0,    39,     0,     0,   184,
       0,   185,     0,     0,    41,     0,     0,     0,     0,   186,
       0,     0,     0,   187,     0,     0,    43,   188,     0,   189,
       0,     0,    45,    46,    47,   180,   181,   182,   190,    67,
       0,    53,    54,    69,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   842,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   183,     0,    39,     0,     0,   184,     0,   185,
       0,     0,    41,     0,     0,     0,     0,   186,     0,     0,
       0,   187,     0,     0,    43,   188,     0,   189,   343,     0,
      45,    46,    47,   180,   181,   182,   190,    67,     0,    53,
      54,    69,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,     0,    39,     0,     0,   184,     0,   185,     0,     0,
      41,     0,     0,     0,     0,   186,     0,     0,     0,   187,
       0,     0,    43,   860,     0,   189,     0,     0,    45,    46,
      47,   180,   181,   182,   190,    67,     0,    53,    54,    69,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,     0,
       0,     0,     0,   184,     0,   185,     0,     0,     0,     0,
       0,     0,     0,   186,    39,     0,     0,   187,    40,     0,
       0,   188,    41,   189,     0,     0,     0,     0,     0,     0,
       0,    42,   190,    67,    43,     0,     0,    69,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    39,    55,    56,    57,     0,     0,    58,     0,    41,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,    65,    41,     0,   325,     0,     0,     0,
       0,     0,     0,   615,    66,    67,    43,     0,    68,    69,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    39,    55,    56,    57,     0,     0,    58,
      65,    41,     0,    59,     0,     0,    60,    61,    62,    63,
      64,    66,    67,    43,     0,    68,    69,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,    65,    41,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    66,    67,    43,     0,
      68,    69,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    39,    55,    56,    57,     0,
       0,    58,    65,    41,     0,    59,     0,     0,    60,    61,
      62,    63,    64,    66,    67,    43,     0,    68,    69,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,     0,     0,     0,     0,     0,    41,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    66,   115,
      43,     0,    68,   116,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    39,    55,    56,
      57,     0,     0,    58,   242,    41,     0,    59,     0,     0,
      60,    61,    62,    63,    64,    66,     0,    43,     0,    68,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    41,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    43,     0,     0,
      66,   115,     0,    45,    46,    47,   180,   181,   182,     0,
       0,     0,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,   337,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   747,     0,     0,     0,     0,
       0,     0,    39,     0,     0,     0,     0,   748,   634,     0,
      41,     0,     0,     0,     0,     0,   635,     0,     0,     0,
       0,     0,    43,     0,     0,     0,    44,   190,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    39,
      55,    56,    57,     0,     0,    58,     0,    41,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,    43,
       0,     0,     0,     0,     0,    45,    46,    47,   180,   181,
     182,     0,     0,     0,    53,    54,    39,    55,    56,    57,
       0,     0,    58,     0,    41,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,    43,     0,     0,     0,
       0,     0,    45,    46,    47,   180,   181,   182,     0,     0,
       0,    53,    54,     0,    55,    56,    57,     0,     0,    58,
       0,     0,    66,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,   633,
     634,     0,     0,     0,     0,     0,     0,     0,   635,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,   190,
      41,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    43,     0,     0,     0,   804,   634,    45,    46,
      47,   180,   181,   182,     0,   635,     0,    53,    54,     0,
      55,    56,    57,    39,     0,    58,   190,     0,     0,    59,
       0,    41,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,    43,     0,     0,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,    39,     0,   634,     0,     0,     0,     0,     0,    41,
       0,   635,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    43,   190,     0,     0,     0,   643,    45,    46,    47,
     180,   181,   182,     0,     0,     0,    53,    54,    39,    55,
      56,    57,     0,     0,    58,     0,    41,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,    43,     0,
       0,     0,     0,   190,    45,    46,    47,   180,   181,   182,
       0,     0,     0,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     635,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190
  };

  const short
  parser::yycheck_[] =
  {
      89,   101,   113,   194,   245,   106,   187,    89,    65,   219,
     106,   232,   101,    34,   228,   229,   334,    40,   217,    42,
     218,   367,   296,    29,   186,   274,   220,   472,   103,   681,
     101,    34,   596,   596,   300,   308,    65,   509,   135,   227,
       3,    12,     1,   595,     1,   500,   237,     5,   755,   682,
      29,   111,   112,   294,   383,   112,   528,    19,   691,   112,
      27,    84,    78,     1,    29,   755,   315,   111,    78,    91,
       1,     1,     1,    27,    19,   109,   258,   188,   189,     1,
     552,   258,   179,   112,   507,    81,   830,    85,     0,    82,
     111,   112,     1,    29,    78,    78,   519,   106,    34,   114,
      21,   135,   290,   302,   204,   120,   135,   220,   106,   102,
     106,   211,    91,   857,   136,   204,   749,   177,    85,   135,
     177,   303,   211,   113,   177,   135,   303,    72,    85,   601,
     553,    85,   232,   233,   686,    88,   120,   120,   845,    79,
     105,    94,   113,   232,   233,    85,   475,   109,   177,   108,
     179,   108,    88,   183,   184,   845,   177,   847,    94,   432,
     119,   124,   168,   234,   170,   262,   196,   622,   104,   105,
     108,   107,   108,   109,   110,   111,   112,   108,   108,   108,
     120,   119,    81,   186,   105,   242,   108,   119,   119,   168,
     119,   170,   441,   381,   382,   376,   114,   119,    14,   108,
     394,   137,   120,   168,    81,   170,    81,   228,   229,   298,
     119,   634,   635,   242,   172,   173,   298,   330,   120,    81,
      81,   120,   114,   120,   876,   228,   229,    81,   120,   110,
      81,    81,   168,   262,   170,   434,   417,   294,   106,   120,
      80,   177,   144,   120,   877,   120,   267,   144,   881,   104,
     186,   119,   107,   108,    94,   110,   125,   680,   120,   120,
     129,   214,   215,    41,   217,   365,   120,   712,   368,   120,
     120,   274,   590,   591,    52,    53,   365,   390,   214,   215,
     337,   217,   137,   313,    78,    19,   126,   127,    22,   106,
     130,   131,   228,   229,   106,   408,   719,   300,   564,   863,
     863,    94,   119,   775,    97,   777,   135,   119,   331,   384,
     407,   104,   315,   316,    66,    67,   252,    51,   254,   665,
      54,    55,   258,   288,   113,   426,   291,   292,   773,   752,
     426,   267,   513,   126,   548,   526,   125,   130,   274,    22,
     129,   438,   372,   542,   506,   416,   106,   106,   301,   302,
     125,   316,   288,   594,   129,   291,   292,   829,    80,   119,
     119,   106,   458,   612,   300,   301,   302,   303,    51,    78,
     102,    54,   308,   113,   119,   435,   465,   437,   407,   315,
     316,   317,   114,   465,   444,   125,   446,   500,   367,   129,
     114,   435,   114,   437,    80,    27,   120,   252,   120,   254,
     444,   102,   446,   119,   105,   127,   109,   830,    94,   438,
     113,    97,   115,   105,   435,   107,   437,   806,   104,    80,
     809,    82,   125,   444,    85,   446,   129,   745,   116,   627,
     113,    24,   120,    94,   857,   121,    97,   860,   441,   114,
     126,   127,   125,   104,   658,   659,   129,   657,   794,   116,
     105,    94,   107,   120,    97,   458,   409,   568,   569,   116,
     121,   104,   317,   120,   116,   126,   127,   113,   120,   130,
     131,   652,   671,   409,    37,   428,    73,    74,    75,    76,
     433,   434,   435,   126,   105,   491,   107,   105,   105,   107,
     107,   837,   428,   105,   119,   107,   432,   433,   434,   435,
     721,   437,    85,   506,    87,   441,    14,   109,   444,   109,
     446,   113,   491,   115,   755,   628,   790,   496,   712,   105,
     769,   107,   458,   125,    88,    80,   491,   548,   105,   875,
     107,   496,    87,    81,    82,    81,    82,   594,   136,    94,
      81,    82,   187,   188,   135,   548,   499,   500,    81,    82,
       4,   611,   122,   123,   872,   491,   114,   114,   114,   780,
     496,   564,   114,   499,   500,   114,   121,   611,   789,   120,
     506,   126,   127,    23,    85,   130,   131,    92,   792,   113,
      19,    19,    85,   683,   537,    78,   539,   119,   541,   542,
     611,   544,   615,    78,   683,    78,    78,    15,    82,   120,
     110,   537,   114,   539,   845,   541,   542,   610,   544,   612,
     110,   802,   548,   726,    72,   114,   114,    80,   135,   114,
     114,   721,   110,   736,   645,   646,   106,   106,   564,   120,
     120,    94,   721,    81,    97,   614,    92,   658,   659,   114,
     114,   104,   645,   646,   120,   610,   867,   747,   748,   614,
     114,    27,    81,   606,   299,   658,   659,    87,   747,   689,
      86,    86,   762,   126,   127,   120,   748,   130,   131,   622,
     606,    25,    27,   762,   610,   611,   612,    82,   614,   779,
     780,   781,   135,    78,    78,   121,   622,   114,   121,   789,
     779,   780,   781,   136,   110,   795,   796,    78,    78,    81,
     789,   814,    81,   120,    27,    82,   795,   796,   768,   645,
     646,   807,   813,   692,   121,   106,   120,   813,   671,   672,
      82,   366,   658,   659,   768,   755,   705,   102,   102,   842,
     843,   376,   120,    85,   121,   671,   672,   121,    79,    92,
     705,   113,   120,   114,   844,    80,   835,   768,   114,   860,
     110,   120,   852,   835,   110,   844,   709,   710,   121,    94,
     121,    78,    28,   852,   785,    27,   769,   867,     9,   705,
     415,   792,   417,   709,   710,   114,   806,   106,   867,   809,
      80,    81,   785,   119,   763,    27,   121,   114,    81,   792,
      85,   126,   127,   114,    94,   130,   131,    97,    81,    37,
      11,    19,    87,    92,   104,    55,    92,   125,   106,   119,
     114,    80,   120,   114,   106,   845,   106,   847,    87,   168,
      85,   121,    85,    92,   614,    94,   126,   127,   125,   617,
     130,   131,   768,   769,   787,   705,   692,   764,   875,   732,
     796,    34,   828,   835,   826,   558,     4,   298,   785,   785,
     646,   787,   121,   792,    12,   659,   792,   126,   127,   504,
      15,   130,   131,   741,   509,   510,    24,   852,   513,   376,
      28,   229,    30,    31,    32,    33,    34,    35,   642,    37,
      38,    39,    40,   528,    42,    43,    44,   591,   553,    47,
     643,   806,   506,    51,   760,   753,    54,    55,    56,    57,
      58,   849,   694,   763,    34,   305,   551,   552,   109,   302,
       3,     4,     5,     6,     7,     8,     9,    10,   709,    12,
      13,    79,   252,    16,    17,    18,    19,    20,   215,    22,
     539,    24,   610,    26,   426,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,   813,    42,
      43,    44,   425,   594,    47,   113,   601,    50,    51,   262,
      -1,    54,    55,    56,    57,    58,   124,    -1,    80,    -1,
     128,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    94,    -1,    77,    97,    -1,    -1,   633,    -1,
      83,    -1,   104,    -1,    -1,    -1,    -1,    90,    -1,    80,
      93,    -1,    95,    -1,    -1,    -1,    87,   652,    -1,   121,
      -1,    -1,    -1,    94,   126,   127,   109,    -1,   130,   131,
     113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     123,   124,   125,   114,    -1,   128,   129,    -1,    -1,   120,
      -1,   134,   135,   136,   137,   126,   127,   140,    -1,   130,
     131,    -1,   145,   146,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,    -1,     7,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,    22,
      -1,    24,    -1,    26,    -1,    28,   741,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
     775,    64,   777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,   829,   830,    -1,    -1,    -1,    -1,
     123,   124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,    -1,    -1,
      -1,    -1,   145,   146,    -1,   860,     3,     4,     5,    -1,
       7,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    16,
      17,    18,    -1,    20,    -1,    -1,    -1,    24,    -1,    26,
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
     137,    -1,    -1,   140,     3,     4,     5,    -1,   145,   146,
      -1,    10,    -1,    12,    13,    -1,    -1,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   123,   124,   125,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
       4,   140,    -1,    -1,     8,    -1,   145,   146,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    21,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,     4,    -1,    47,    -1,    -1,    -1,    51,    -1,    12,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    21,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,   113,
      -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,
     124,   125,    -1,    -1,   128,   129,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
     144,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
     113,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,   124,   125,    -1,    -1,   128,   129,     1,    -1,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,   144,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,   124,   125,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   108,   109,    -1,    -1,    -1,   113,
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
      -1,    -1,    -1,    -1,    80,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    95,
      -1,    97,    -1,    -1,    -1,    -1,    -1,   103,   104,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,   113,   114,   115,
      -1,    -1,    -1,    -1,   120,   121,    -1,    -1,   124,   125,
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
      -1,   109,   110,    -1,    -1,   113,    -1,   115,    -1,    -1,
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
      80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    -1,    95,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,
      -1,   121,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    85,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,
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
      -1,    -1,   106,    -1,    -1,   109,    -1,    -1,    -1,   113,
      -1,   115,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
     124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,
     134,   135,   136,   137,    -1,    -1,   140,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      46,    47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,
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
      -1,   119,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
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
      90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
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
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,
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
      -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    95,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
      -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,
     136,   137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,
      -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    87,
      -1,    -1,    90,    -1,    -1,    -1,    -1,    95,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,
     128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,
      -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,   124,   125,    12,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    24,    -1,
     140,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    90,    -1,    -1,    -1,    94,    95,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,     4,   109,    -1,    -1,    -1,   113,   114,   115,
      12,    -1,    -1,    -1,   120,    -1,    -1,    -1,   124,   125,
     126,   127,    24,   129,   130,   131,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      92,    -1,    -1,    95,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,     4,   109,    -1,    -1,
      -1,   113,    -1,   115,    12,    -1,    -1,    -1,    -1,   121,
      -1,    19,   124,   125,    -1,   127,    24,   129,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
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
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    -1,    -1,    95,    -1,    97,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,
      -1,    -1,    -1,   113,    12,   115,    -1,    -1,    -1,    -1,
      -1,   121,    -1,    -1,   124,   125,    24,   127,    -1,   129,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    -1,    -1,    95,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,     4,    -1,
      -1,   109,   110,    -1,    -1,   113,    12,   115,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,   124,   125,    24,    -1,
      -1,   129,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    -1,    39,    40,    -1,    42,    43,    44,    -1,
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
      -1,    89,    90,    -1,     4,    -1,    -1,    95,    -1,    97,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,    -1,    24,   113,    -1,   115,    28,    -1,
      30,    31,    32,    33,    34,    35,   124,   125,    -1,    39,
      40,   129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,    -1,    24,   113,    -1,   115,    28,    -1,    30,    31,
      32,    33,    34,    35,   124,   125,    -1,    39,    40,   129,
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
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,     4,    -1,
      -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,
     114,   115,    -1,    -1,    30,    31,    32,    33,    34,    35,
     124,   125,    -1,    39,    40,   129,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,
      -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   109,    -1,    -1,    24,   113,    -1,   115,
      -1,    -1,    30,    31,    32,    33,    34,    35,   124,   125,
      -1,    39,    40,   129,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,
      -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,   109,    -1,    -1,    24,   113,    -1,   115,    28,    -1,
      30,    31,    32,    33,    34,    35,   124,   125,    -1,    39,
      40,   129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,
      12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,
      -1,    -1,    24,   113,    -1,   115,    -1,    -1,    30,    31,
      32,    33,    34,    35,   124,   125,    -1,    39,    40,   129,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    -1,    -1,    95,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,     4,    -1,    -1,   109,     8,    -1,
      -1,   113,    12,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    21,   124,   125,    24,    -1,    -1,   129,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,     4,    42,    43,    44,    -1,    -1,    47,    -1,    12,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    12,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    21,   124,   125,    24,    -1,   128,   129,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
     113,    12,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,   124,   125,    24,    -1,   128,   129,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    24,    -1,
     128,   129,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,   113,    12,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,   124,   125,    24,    -1,   128,   129,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
      24,    -1,   128,   129,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,     4,    42,    43,
      44,    -1,    -1,    47,   113,    12,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,   124,    -1,    24,    -1,   128,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,
     124,   125,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,   113,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,   124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,   104,   105,    -1,
      12,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,   124,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,     4,
      42,    43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,     4,    42,    43,    44,
      -1,    -1,    47,    -1,    12,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    -1,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,   124,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,   124,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    -1,    -1,    -1,   104,   105,    30,    31,
      32,    33,    34,    35,    -1,   113,    -1,    39,    40,    -1,
      42,    43,    44,     4,    -1,    47,   124,    -1,    -1,    51,
      -1,    12,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,     4,    -1,   105,    -1,    -1,    -1,    -1,    -1,    12,
      -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,   124,    -1,    -1,    -1,    87,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    24,    -1,
      -1,    -1,    -1,   124,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   105,   148,   149,   150,   153,   125,   129,   366,
     154,   165,     0,   154,    66,    67,   151,   106,   119,   155,
     166,   167,     1,   108,   365,   109,   135,   227,   227,   113,
     156,    14,   168,   179,   180,   135,   228,    78,    78,     4,
       8,    12,    21,    24,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    42,    43,    44,    47,    51,
      54,    55,    56,    57,    58,   113,   124,   125,   128,   129,
     144,   157,   158,   159,   163,   332,   335,   336,   349,   351,
     352,   358,    27,    24,   169,   119,   164,     3,     5,     6,
       7,     8,     9,    10,    13,    16,    17,    18,    19,    20,
      22,    26,    29,    36,    50,    64,    77,    83,    90,    93,
      95,   109,   113,   115,   123,   125,   129,   134,   135,   136,
     137,   140,   145,   146,   177,   181,   183,   184,   185,   187,
     191,   203,   232,   277,   282,   286,   287,   289,   290,   291,
     292,   321,   322,   325,   326,   348,   349,   352,   360,   361,
     364,   110,   120,   366,   366,    80,    94,    97,   104,   126,
     127,   130,   131,   355,   356,   357,   359,   114,   120,   144,
     113,   160,   105,   107,   152,   366,   119,   113,   285,   286,
      33,    34,    35,    90,    95,    97,   105,   109,   113,   115,
     124,   209,   237,   239,   241,   242,   244,   248,   330,   331,
     335,   345,   347,   358,    37,   204,   335,    19,    22,    51,
      54,    55,   189,   190,   105,   107,   313,    85,   105,   285,
     306,   307,   308,    73,    74,    75,    76,   188,   105,   107,
     224,   225,    19,    37,    38,   186,   239,   241,   331,    14,
     313,   290,   113,   349,   109,   113,   323,   324,   326,   361,
     290,   311,   312,   290,   289,   290,    80,   110,   121,   127,
     131,   285,   286,   294,   296,   297,   328,   342,   344,   354,
     355,   357,   362,   363,   103,   114,   120,   293,   294,   295,
     355,   356,   357,   362,   367,   116,   367,   109,   283,   284,
      19,   283,   283,   136,   176,   164,    19,    72,   211,    81,
     120,    82,    85,   121,   279,   280,   281,   328,   341,   343,
     353,   355,   356,    89,   290,   102,   105,    88,   135,   114,
     114,   114,   114,   114,   159,    79,   161,   162,   163,   154,
     154,     4,   170,    23,    81,   248,   248,   113,   232,   270,
     271,   272,   352,    28,   110,   234,   235,   237,   239,    80,
      87,    94,   114,   126,   127,   130,   131,   234,   252,   337,
     338,   367,   367,    85,   256,    92,    87,   121,   246,   333,
     337,   346,    89,   245,   248,   239,   113,   230,   235,   249,
     239,    19,    19,    20,    46,   285,   310,   314,   315,   316,
     314,   299,   300,   316,   306,   119,   288,   308,   365,    78,
      78,    78,    78,   249,   223,   277,   278,   286,   223,    15,
     198,   239,   239,   113,   331,    81,   120,    82,    41,    52,
      53,   182,    78,   135,   363,    81,   120,   229,    87,   311,
     351,   360,   341,    79,    85,   120,   110,   120,   286,   350,
     352,   102,   114,   114,   120,   114,   120,   114,   114,   114,
     120,   116,    91,   136,   349,   249,   349,   349,   121,   178,
     327,   339,   340,   356,   363,   211,   135,   209,   231,   235,
     236,   348,   285,   299,   351,    27,   226,   281,   287,   248,
     350,    79,   317,   318,   319,   349,   350,   352,   290,   114,
     114,   120,   106,   365,   366,    12,   113,   171,   172,   105,
     107,   301,   230,   356,    81,   106,   120,   253,   110,    81,
      92,   114,   114,   120,   114,   114,   116,   257,   258,   259,
      27,   218,   239,   235,   335,   347,   241,   248,    81,   206,
     234,   251,   252,   249,   249,   225,   313,    86,   106,   119,
     365,    87,   120,   106,    25,    27,   222,   106,   119,   365,
     285,    82,    81,    82,   207,     3,   124,   212,   213,   214,
     235,   260,   331,   234,   135,    78,   114,   109,   113,   115,
     329,   330,   323,    78,   285,   121,   121,   285,   298,   316,
     285,   294,   294,   350,   294,   294,   136,   110,    78,    78,
      81,    81,   352,   361,   120,    28,   210,   237,   239,    78,
     135,    81,    82,   205,   265,   226,    82,   121,   225,   106,
     120,    82,   102,   163,   113,    21,   144,   163,   173,   174,
     175,   106,   119,   285,   302,   303,   304,   309,   302,   365,
     114,   235,   272,   104,   105,   113,   254,   255,   345,   260,
     235,   234,   120,    87,   345,   105,   107,   217,   121,   121,
     260,   114,   120,   285,   315,   285,   316,   285,   105,   107,
     221,   278,   235,   260,   254,    85,   192,   214,   348,   114,
     116,   120,    79,   110,   230,   233,   233,   121,   121,   339,
     253,   205,   265,    92,    78,   260,    28,   266,   267,   268,
      27,   261,     9,   273,   274,   275,   285,   318,   294,   320,
     350,   173,   366,   160,   114,   120,   144,   302,   106,   119,
      87,   305,   306,   365,   235,   345,   345,   114,   258,   259,
       7,    26,   199,   215,   216,   278,   216,   234,   288,     7,
      26,   202,   203,   219,   220,   278,   220,   193,   347,    27,
     195,    81,   316,   285,    78,   120,    78,    92,   104,   261,
     273,   239,   253,    85,   238,   243,   247,   248,   269,   105,
     107,   273,   113,   189,   276,   334,   335,   275,    82,   102,
     114,   175,   304,   285,   226,    81,   106,    81,    37,   200,
      19,    37,   198,   239,   106,   119,   365,    11,    19,   201,
     201,   106,   119,   365,    87,   105,   107,   196,   231,   230,
     239,   237,   241,   273,   104,   267,    92,   121,   247,   327,
     262,   263,   264,   288,   262,   114,   239,   240,   250,   276,
     190,   294,   350,   226,   260,   260,   239,   198,   239,    81,
      82,   208,   215,   285,   198,   211,   219,   194,   347,    79,
     197,   198,    79,   197,    92,   243,   269,   243,   106,   119,
     324,   365,   120,   114,   106,   114,   206,    82,   208,   260,
     113,   255,   345,   210,   347,   106,   106,   119,   365,   365,
     239,   264,    81,   250,   345,    85,   205,   265,   198,   230,
     193,   261,   273,   273
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
     180,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     182,   182,   182,   183,   184,   184,   184,   184,   184,   184,
     185,   186,   186,   187,   187,   187,   187,   188,   188,   188,
     188,   188,   189,   189,   189,   190,   191,   191,   191,   192,
     192,   193,   194,   194,   195,   195,   196,   196,   196,   196,
     197,   197,   197,   197,   198,   199,   199,   199,   199,   199,
     200,   200,   201,   201,   202,   202,   202,   203,   203,   204,
     204,   205,   205,   206,   206,   207,   207,   207,   208,   208,
     208,   209,   209,   210,   210,   210,   210,   211,   211,   211,
     212,   212,   213,   213,   214,   214,   215,   215,   216,   216,
     216,   216,   217,   217,   218,   218,   219,   219,   220,   220,
     220,   220,   221,   221,   222,   222,   223,   223,   223,   223,
     224,   224,   225,   226,   226,   227,   227,   228,   228,   228,
     229,   229,   230,   231,   232,   232,   233,   233,   234,   234,
     235,   235,   235,   236,   237,   238,   239,   239,   240,   241,
     242,   242,   243,   243,   244,   244,   244,   245,   246,   246,
     247,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   249,   250,   250,   251,   251,   252,   252,   253,   253,
     254,   254,   254,   255,   255,   256,   256,   257,   257,   258,
     259,   259,   260,   261,   261,   261,   262,   262,   263,   263,
     264,   265,   266,   266,   267,   267,   268,   268,   269,   269,
     270,   270,   271,   271,   272,   273,   273,   274,   274,   275,
     275,   275,   276,   276,   276,   277,   277,   278,   279,   279,
     280,   280,   281,   282,   282,   282,   282,   282,   282,   282,
     282,   282,   283,   283,   284,   284,   285,   285,   286,   286,
     287,   287,   288,   288,   289,   289,   289,   289,   290,   290,
     290,   290,   290,   290,   290,   290,   290,   290,   290,   291,
     291,   291,   292,   292,   292,   292,   292,   292,   292,   292,
     293,   293,   294,   294,   294,   295,   295,   296,   296,   296,
     296,   296,   296,   296,   297,   297,   298,   298,   299,   300,
     300,   301,   301,   301,   301,   302,   302,   303,   303,   303,
     304,   305,   305,   306,   306,   307,   307,   308,   309,   310,
     311,   312,   312,   313,   313,   314,   314,   314,   314,   315,
     315,   316,   316,   316,   317,   317,   318,   318,   318,   319,
     319,   319,   319,   320,   320,   321,   321,   322,   322,   323,
     323,   323,   324,   324,   325,   325,   325,   325,   326,   326,
     327,   327,   328,   328,   329,   329,   329,   330,   330,   330,
     330,   330,   331,   331,   332,   332,   332,   332,   333,   333,
     334,   335,   335,   336,   337,   337,   337,   338,   338,   338,
     338,   339,   339,   340,   340,   341,   341,   342,   342,   343,
     343,   344,   344,   345,   346,   347,   347,   347,   347,   347,
     348,   348,   349,   349,   349,   350,   351,   351,   352,   352,
     352,   352,   352,   352,   352,   352,   353,   353,   354,   354,
     355,   356,   356,   357,   357,   358,   358,   358,   358,   358,
     358,   358,   358,   358,   358,   358,   358,   358,   358,   358,
     358,   358,   358,   359,   359,   360,   360,   361,   362,   362,
     363,   363,   364,   364,   364,   364,   364,   365,   365,   366,
     366,   367,   367
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
       0,     1,     1,     1,     1,     1,     5,     7,     1,     1,
       1,     1,     1,     4,     4,     4,     5,     6,     6,     4,
       4,     3,     1,     4,     3,     6,     7,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     3,     4,     4,     0,
       2,     3,     2,     1,     0,     2,     3,     3,     3,     3,
       3,     2,     1,     0,     3,     4,     3,     4,     2,     3,
       0,     1,     0,     1,     3,     6,     7,     1,     1,     0,
       1,     0,     2,     0,     2,     0,     2,     2,     0,     2,
       4,     3,     1,     6,     4,     3,     1,     4,     3,     0,
       0,     1,     1,     2,     1,     1,     1,     1,     3,     2,
       1,     0,     3,     3,     2,     0,     1,     1,     3,     2,
       1,     0,     3,     3,     2,     0,     3,     2,     1,     0,
       3,     3,     1,     2,     0,     1,     3,     3,     1,     0,
       0,     2,     1,     1,     3,     1,     1,     3,     1,     3,
       4,     3,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     3,     1,     2,     1,     2,     3,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     3,     2,     5,     3,
       3,     1,     1,     3,     1,     0,     1,     3,     2,     0,
       1,     3,     5,     1,     5,     0,     2,     3,     1,     3,
       2,     0,     1,     4,     4,     0,     1,     0,     3,     1,
       4,     2,     3,     1,     4,     2,     3,     0,     1,     3,
       0,     1,     3,     1,     3,     0,     1,     2,     1,     2,
       3,     3,     1,     2,     3,     1,     2,     1,     3,     2,
       2,     1,     4,     3,     3,     4,     4,     3,     4,     6,
       6,     4,     0,     1,     3,     4,     3,     1,     1,     3,
       2,     1,     1,     0,     2,     3,     2,     1,     3,     2,
       2,     4,     4,     8,     2,     4,     2,     2,     1,     4,
       3,     1,     1,     1,     1,     3,     3,     3,     3,     1,
       3,     2,     1,     2,     2,     3,     3,     1,     1,     2,
       4,     3,     5,     3,     3,     3,     3,     1,     1,     3,
       1,     3,     3,     2,     2,     1,     2,     3,     2,     1,
       2,     3,     2,     2,     1,     3,     2,     4,     1,     1,
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
  "deriv_strategy_via", "stand_alone_deriving", "opt_injective_info",
  "injectivity_cond", "inj_varids", "where_type_family",
  "ty_fam_inst_eqn_list", "ty_fam_inst_eqns", "ty_fam_inst_eqn",
  "at_decl_cls", "opt_family", "opt_instance", "at_decl_inst",
  "data_or_newtype", "opt_class", "opt_kind_sig", "opt_datafam_kind_sig",
  "opt_tyfam_kind_sig", "opt_at_kind_inj_sig", "tycl_hdr",
  "datafam_inst_hdr", "capi_ctype", "maybe_roles", "roles", "role",
  "decl_cls", "decls_cls", "decllist_cls", "where_cls", "decl_inst",
  "decls_inst", "decllist_inst", "where_inst", "decls", "decllist",
  "binds", "wherebinds", "strings", "stringlist", "opt_tyconsig",
  "sigtype", "sigtypedoc", "sig_vars", "sigtypes1", "ktype", "ctype",
  "ctypedoc", "context", "context_no_ops", "type", "typedoc", "btype",
  "infixtype", "btype_no_ops", "ftype", "tyarg", "tyop", "atype_docs",
  "atype", "inst_type", "deriv_types", "comma_types0", "comma_types1",
  "tv_bndrs", "tv_bndr", "tv_bndr_no_braces", "fds", "fds1", "fd",
  "varids0", "kind", "gadt_constrlist", "gadt_constrs0", "gadt_constrs",
  "gadt_constr", "constrs", "constrs1", "constr", "forall", "constr_stuff",
  "fielddecls", "fielddecls1", "fielddecl", "maybe_derivings", "derivings",
  "deriving", "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs",
  "gdrh", "sigdecl", "activation", "explicit_activation", "exp",
  "infixexp", "exp10", "optSemi", "fexp", "aexp", "aexp1", "aexp2",
  "projection", "texp", "tup_exprs", "list", "lexps", "squals",
  "guardquals", "guardquals1", "altslist", "alts", "alts1", "alt",
  "alt_rhs", "gdpats", "ifgdpats", "gdpat", "pat", "bindpat", "apat",
  "apats1", "stmtlist", "stmts", "stmt", "qual", "fbinds", "fbinds1",
  "fbind", "fieldToUpdate", "qcon", "gen_qcon", "con", "con_list",
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
       0,   517,   517,   534,   535,   537,   541,   542,   543,   545,
     546,   548,   549,   552,   554,   555,   556,   564,   565,   567,
     568,   569,   570,   572,   573,   575,   576,   577,   579,   580,
     581,   583,   584,   586,   587,   589,   590,   594,   595,   597,
     598,   600,   602,   603,   605,   618,   619,   621,   622,   624,
     625,   629,   630,   632,   633,   634,   635,   637,   638,   640,
     641,   646,   647,   649,   650,   651,   653,   654,   658,   660,
     661,   663,   664,   665,   666,   667,   668,   669,   675,   677,
     681,   682,   683,   685,   688,   689,   690,   692,   693,   694,
     696,   698,   699,   702,   703,   704,   710,   717,   718,   719,
     720,   721,   723,   724,   725,   727,   729,   731,   733,   738,
     739,   741,   743,   744,   748,   749,   751,   752,   753,   754,
     756,   757,   758,   759,   761,   764,   766,   768,   770,   771,
     773,   773,   775,   775,   779,   781,   788,   795,   796,   799,
     800,   804,   805,   807,   808,   810,   811,   812,   814,   815,
     816,   819,   820,   823,   824,   825,   826,   828,   829,   830,
     835,   836,   838,   839,   841,   855,   883,   884,   886,   887,
     888,   889,   891,   892,   894,   895,   897,   898,   900,   901,
     902,   903,   905,   906,   908,   909,   912,   913,   914,   915,
     917,   918,   920,   922,   923,   931,   932,   934,   935,   936,
     949,   950,   959,   961,   963,   964,   966,   967,   976,   977,
     979,   980,   982,   984,   993,   995,   997,   998,  1000,  1003,
    1005,  1006,  1008,  1009,  1011,  1013,  1014,  1016,  1018,  1019,
    1026,  1033,  1034,  1035,  1036,  1037,  1038,  1039,  1040,  1046,
    1047,  1050,  1052,  1053,  1055,  1056,  1058,  1059,  1066,  1067,
    1069,  1070,  1071,  1074,  1075,  1079,  1080,  1082,  1083,  1086,
    1088,  1089,  1094,  1100,  1101,  1102,  1104,  1105,  1107,  1108,
    1110,  1112,  1114,  1115,  1117,  1118,  1120,  1121,  1123,  1124,
    1130,  1131,  1133,  1134,  1136,  1138,  1139,  1141,  1142,  1144,
    1149,  1154,  1160,  1161,  1162,  1167,  1169,  1171,  1175,  1176,
    1178,  1179,  1183,  1193,  1194,  1196,  1197,  1198,  1199,  1200,
    1201,  1202,  1205,  1206,  1208,  1209,  1214,  1215,  1219,  1220,
    1222,  1223,  1225,  1226,  1231,  1232,  1233,  1234,  1237,  1238,
    1239,  1240,  1241,  1243,  1244,  1245,  1246,  1247,  1249,  1252,
    1253,  1254,  1257,  1258,  1259,  1260,  1261,  1262,  1267,  1268,
    1271,  1272,  1277,  1278,  1279,  1284,  1285,  1303,  1304,  1305,
    1306,  1307,  1308,  1309,  1311,  1312,  1325,  1327,  1337,  1339,
    1340,  1343,  1344,  1345,  1346,  1348,  1349,  1351,  1352,  1353,
    1355,  1357,  1358,  1360,  1361,  1363,  1364,  1366,  1368,  1370,
    1372,  1374,  1375,  1378,  1379,  1381,  1382,  1383,  1384,  1389,
    1390,  1392,  1393,  1394,  1399,  1400,  1402,  1403,  1404,  1406,
    1407,  1408,  1409,  1412,  1413,  1445,  1446,  1448,  1449,  1451,
    1452,  1453,  1455,  1456,  1458,  1459,  1460,  1461,  1463,  1464,
    1466,  1467,  1469,  1470,  1473,  1474,  1475,  1477,  1478,  1479,
    1480,  1481,  1483,  1484,  1486,  1487,  1488,  1489,  1492,  1493,
    1495,  1497,  1498,  1502,  1504,  1505,  1506,  1508,  1509,  1510,
    1511,  1516,  1517,  1519,  1520,  1522,  1523,  1526,  1527,  1532,
    1533,  1535,  1536,  1540,  1542,  1544,  1545,  1546,  1547,  1548,
    1551,  1552,  1554,  1555,  1556,  1558,  1560,  1561,  1563,  1564,
    1565,  1566,  1567,  1568,  1569,  1570,  1572,  1573,  1575,  1576,
    1578,  1580,  1581,  1583,  1584,  1586,  1587,  1588,  1589,  1590,
    1591,  1592,  1593,  1594,  1595,  1596,  1597,  1598,  1599,  1600,
    1601,  1602,  1603,  1605,  1606,  1610,  1611,  1613,  1615,  1616,
    1618,  1619,  1623,  1624,  1625,  1626,  1627,  1632,  1635,  1639,
    1640,  1642,  1643
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
#line 8019 "parser.cc"

#line 1652 "parser.y"


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

Hs::LExp make_record_field_selection(const yy::location& loc, const Hs::LExp& object, const std::string& field)
{
    Hs::LExp selector = {loc, Hs::Var(field)};
    return make_parsed_app(loc, selector, object);
}

Hs::LExp make_parsed_app(const yy::location& loc, const Hs::LExp& head, const Hs::LExp& arg)
{
    // Accumulate parser application syntax into one flat source spine.
    std::vector<Hs::LExp> terms;
    if (auto app = unloc(head).to<Hs::ParsedApp>())
        terms = app->terms;
    else
        terms.push_back(head);
    terms.push_back(arg);
    return {loc, Hs::ParsedApp(terms)};
}

Hs::LExp make_record_expression(const yy::location& loc, const Hs::LExp& head, const Located<Hs::FieldBindings>& fbinds)
{
    return {loc, Hs::RecordSyntax(head, fbinds)};
}

Hs::LExp make_record_projection(const yy::location& loc, const std::vector<std::string>& fields)
{
    Hs::LVar arg = {loc, Hs::Var("v$record_dot")};
    Hs::LExp body = {loc, unloc(arg)};

    for(const auto& field: fields)
        body = make_record_field_selection(loc, body, field);

    Hs::LPat arg_pat = {loc, Hs::VarPattern(arg)};
    return {loc, Hs::Lambda({arg_pat}, body)};
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
        else if (auto tok = type.to<Hs::TypeOfKind>())
        {
            if (auto tv = unloc(tok->type).to<Hs::TypeVar>())
            {
                auto tvok = *tv;
                tvok.kind = tok->kind;
                ltype_vars.push_back({loc,tvok});
            }
            else
                throw myexception()<<"Type '"<<type<<"' is not a type variable";
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
                                           const Hs::ConstructorsDecl& constrs,
                                           const std::vector<Hs::Deriving>& derivings)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs, derivings)};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k,
                                           const Hs::GADTConstructorsDecl& constrs,
                                           const std::vector<Hs::Deriving>& derivings)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs, derivings)};
}

Hs::InstanceDecl make_instance_decl(const std::optional<std::string>& oprag, const Hs::LType& polytype, const optional<Located<Hs::Decls>>& decls)
{
    std::vector<Hs::TypeFamilyInstanceDecl> type_inst_decls;
    std::vector<Hs::DataFamilyInstanceDecl> data_inst_decls;
    Hs::Decls method_decls;
    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                type_inst_decls.push_back(*TI);
            else if (auto DI = decl.to<Hs::DataFamilyInstanceDecl>())
                data_inst_decls.push_back(*DI);
            else if (auto V = decl.to<Hs::ValueDecl>())
                method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of instance "<<unloc(polytype).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {oprag, polytype, type_inst_decls, data_inst_decls, method_decls};
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
