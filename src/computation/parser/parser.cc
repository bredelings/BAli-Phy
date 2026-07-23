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

  case 44: // importdecl: "import" optqualified modid optqualified maybeas maybeimpspec
#line 605 "parser.y"
                                                                                                                     {
    if (yystack_[4].value.as < bool > () and yystack_[2].value.as < bool > ())
        drv.push_error_message(yystack_[2].location, "Multiple occurrences of 'qualified'");
    else if (yystack_[2].value.as < bool > () and not drv.has_extension(LangExt::ImportQualifiedPost))
        drv.push_error_message(yystack_[2].location, "ImportQualifiedPost is required for postpositive 'qualified'");
    yylhs.value.as < Hs::LImpDecl > () = {yylhs.location, Hs::ImpDecl(yystack_[4].value.as < bool > () or yystack_[2].value.as < bool > (),yystack_[3].value.as < Located<std::string> > (),yystack_[1].value.as < std::optional<Located<std::string>> > (),yystack_[0].value.as < std::optional<Hs::ImpSpec> > ())};
}
#line 2992 "parser.cc"
    break;

  case 45: // optqualified: "qualified"
#line 622 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2998 "parser.cc"
    break;

  case 46: // optqualified: %empty
#line 623 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 3004 "parser.cc"
    break;

  case 47: // maybeas: "as" modid
#line 625 "parser.y"
                               { yylhs.value.as < std::optional<Located<std::string>> > () = yystack_[0].value.as < Located<std::string> > (); }
#line 3010 "parser.cc"
    break;

  case 48: // maybeas: %empty
#line 626 "parser.y"
                               { }
#line 3016 "parser.cc"
    break;

  case 49: // maybeimpspec: impspec
#line 628 "parser.y"
                               { yylhs.value.as < std::optional<Hs::ImpSpec> > () = yystack_[0].value.as < Hs::ImpSpec > (); }
#line 3022 "parser.cc"
    break;

  case 50: // maybeimpspec: %empty
#line 629 "parser.y"
                               { }
#line 3028 "parser.cc"
    break;

  case 51: // impspec: "(" importlist ")"
#line 633 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{false, yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 3034 "parser.cc"
    break;

  case 52: // impspec: "hiding" "(" importlist ")"
#line 634 "parser.y"
                                      { yylhs.value.as < Hs::ImpSpec > () = Hs::ImpSpec{true,  yystack_[1].value.as < std::vector<Hs::LExport> > ()}; }
#line 3040 "parser.cc"
    break;

  case 53: // importlist: importlist1
#line 636 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[0].value.as < std::vector<Hs::LExport> > ();}
#line 3046 "parser.cc"
    break;

  case 54: // importlist: %empty
#line 637 "parser.y"
                                      {}
#line 3052 "parser.cc"
    break;

  case 55: // importlist: importlist1 ','
#line 638 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[1].value.as < std::vector<Hs::LExport> > ();}
#line 3058 "parser.cc"
    break;

  case 56: // importlist: ','
#line 639 "parser.y"
                                      {}
#line 3064 "parser.cc"
    break;

  case 57: // importlist1: importlist1 "," import
#line 641 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > () = yystack_[2].value.as < std::vector<Hs::LExport> > (); yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 3070 "parser.cc"
    break;

  case 58: // importlist1: import
#line 642 "parser.y"
                                      {yylhs.value.as < std::vector<Hs::LExport> > ().push_back(yystack_[0].value.as < Hs::LExport > ());}
#line 3076 "parser.cc"
    break;

  case 59: // import: qcname export_subspec
#line 644 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{}, yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < std::optional<Hs::ExportSubSpec> > ()}}; }
#line 3082 "parser.cc"
    break;

  case 60: // import: "module" modid
#line 645 "parser.y"
                                      {yylhs.value.as < Hs::LExport > () = {yylhs.location,Hs::Export{{{yystack_[1].location,Hs::ImpExpNs::module}}, yystack_[0].value.as < Located<std::string> > (), {}}}; }
#line 3088 "parser.cc"
    break;

  case 61: // prec: %empty
#line 650 "parser.y"
                   { }
#line 3094 "parser.cc"
    break;

  case 62: // prec: "INTEGER"
#line 651 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < integer > ().convert_to<int>(); }
#line 3100 "parser.cc"
    break;

  case 63: // infix: "infix"
#line 653 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infix; }
#line 3106 "parser.cc"
    break;

  case 64: // infix: "infixl"
#line 654 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixl; }
#line 3112 "parser.cc"
    break;

  case 65: // infix: "infixr"
#line 655 "parser.y"
                   { yylhs.value.as < Hs::Fixity > () = Hs::Fixity::infixr; }
#line 3118 "parser.cc"
    break;

  case 66: // ops: ops "," op
#line 657 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()}); }
#line 3124 "parser.cc"
    break;

  case 67: // ops: op
#line 658 "parser.y"
                   { yylhs.value.as < std::vector<Located<std::string>> > () = {{yystack_[0].location,yystack_[0].value.as < std::string > ()}}; }
#line 3130 "parser.cc"
    break;

  case 68: // topdecls: topdecls_semi topdecl
#line 662 "parser.y"
                                 { yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ()); }
#line 3136 "parser.cc"
    break;

  case 69: // topdecls_semi: topdecls_semi topdecl semis1
#line 664 "parser.y"
                                            { yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[1].value.as < Hs::LDecl > ()); }
#line 3142 "parser.cc"
    break;

  case 70: // topdecls_semi: %empty
#line 665 "parser.y"
                                            { }
#line 3148 "parser.cc"
    break;

  case 71: // topdecl: cl_decl
#line 667 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3154 "parser.cc"
    break;

  case 72: // topdecl: ty_decl
#line 668 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3160 "parser.cc"
    break;

  case 73: // topdecl: standalone_kind_sig
#line 669 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3166 "parser.cc"
    break;

  case 74: // topdecl: inst_decl
#line 670 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3172 "parser.cc"
    break;

  case 75: // topdecl: stand_alone_deriving
#line 671 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3178 "parser.cc"
    break;

  case 76: // topdecl: "default" opt_class "(" comma_types0 ")"
#line 672 "parser.y"
                                                         {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::DefaultDecl(yystack_[3].value.as < std::optional<Located<std::string>> > (),yystack_[1].value.as < std::vector<Hs::LType> > ())}; }
#line 3184 "parser.cc"
    break;

  case 77: // topdecl: "foreign" "import" call_conv "STRING" var "::" sigtypedoc
#line 673 "parser.y"
                                                                  {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::ForeignDecl(yystack_[4].value.as < Located<std::string> > (), yystack_[3].value.as < std::string > (), {yystack_[2].location,yystack_[2].value.as < std::string > ()}, yystack_[0].value.as < Hs::LType > ())};}
#line 3190 "parser.cc"
    break;

  case 78: // topdecl: decl_no_th
#line 679 "parser.y"
                                               {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3196 "parser.cc"
    break;

  case 79: // topdecl: infixexp
#line 681 "parser.y"
                                               { drv.push_error_message(yystack_[0].location, "Unexpected top-level expression.");
                                                yylhs.value.as < Hs::LDecl > () = {yystack_[0].location, Hs::ValueDecl({yystack_[0].location, unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}, Hs::SimpleRHS({yystack_[0].location, Hs::Var("<top-level-expression>")}))}; }
#line 3203 "parser.cc"
    break;

  case 80: // call_conv: "bpcall"
#line 685 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"bpcall"};}
#line 3209 "parser.cc"
    break;

  case 81: // call_conv: "trcall"
#line 686 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"trcall"};}
#line 3215 "parser.cc"
    break;

  case 82: // call_conv: "ecall"
#line 687 "parser.y"
                    {yylhs.value.as < Located<std::string> > () = {yylhs.location,"ecall"};}
#line 3221 "parser.cc"
    break;

  case 83: // cl_decl: "class" tycl_hdr fds where_cls
#line 689 "parser.y"
                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location,make_class_decl(yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first,yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,yystack_[1].value.as < std::vector<Hs::FunDep> > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3227 "parser.cc"
    break;

  case 84: // ty_decl: "type" "role" oqtycon maybe_roles
#line 692 "parser.y"
                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::RoleAnnotationDecl({yystack_[1].location, Hs::TypeCon(yystack_[1].value.as < std::string > ())}, yystack_[0].value.as < std::vector<Located<std::optional<Role>>> > ())};}
#line 3233 "parser.cc"
    break;

  case 85: // ty_decl: "type" type "=" ktype
#line 693 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_type_synonym(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3239 "parser.cc"
    break;

  case 86: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 694 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_data_or_newtype(yystack_[4].value.as < Hs::DataOrNewtype > (), yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[2].value.as < std::pair<Hs::Context,Hs::LType> > ().second,{},yystack_[1].value.as < Hs::ConstructorsDecl > (),yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3245 "parser.cc"
    break;

  case 87: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 696 "parser.y"
                                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_data_or_newtype(yystack_[5].value.as < Hs::DataOrNewtype > (), yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().first, yystack_[3].value.as < std::pair<Hs::Context,Hs::LType> > ().second, yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ())};}
#line 3251 "parser.cc"
    break;

  case 88: // ty_decl: "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family
#line 697 "parser.y"
                                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[3].value.as < Hs::LType > (), yystack_[2].value.as < std::optional<Located<Hs::Kind>> > (), yystack_[0].value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > ())};}
#line 3257 "parser.cc"
    break;

  case 89: // ty_decl: "data" "family" type opt_datafam_kind_sig
#line 698 "parser.y"
                                                                                          {yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})};}
#line 3263 "parser.cc"
    break;

  case 90: // standalone_kind_sig: "type" sks_vars "::" kind
#line 700 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::KindSigDecl(yystack_[2].value.as < std::vector<Hs::LTypeCon> > (),yystack_[0].value.as < Hs::Kind > ())};}
#line 3269 "parser.cc"
    break;

  case 91: // sks_vars: sks_vars "," oqtycon
#line 702 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = yystack_[2].value.as < std::vector<Hs::LTypeCon> > (); yylhs.value.as < std::vector<Hs::LTypeCon> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}); }
#line 3275 "parser.cc"
    break;

  case 92: // sks_vars: oqtycon
#line 703 "parser.y"
                                                                           {yylhs.value.as < std::vector<Hs::LTypeCon> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}}; }
#line 3281 "parser.cc"
    break;

  case 93: // inst_decl: "instance" overlap_pragma inst_type where_inst
#line 706 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location,make_instance_decl(yystack_[2].value.as < std::optional<std::string> > (),yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < std::optional<Located<Hs::Decls>> > ())};}
#line 3287 "parser.cc"
    break;

  case 94: // inst_decl: "type" "instance" ty_fam_inst_eqn
#line 707 "parser.y"
                                                                           {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};}
#line 3293 "parser.cc"
    break;

  case 95: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 709 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3303 "parser.cc"
    break;

  case 96: // inst_decl: data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 715 "parser.y"
           {
	       auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
	       auto [con, args] = check_type_or_class_header2(type);
	       yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	   }
#line 3313 "parser.cc"
    break;

  case 97: // overlap_pragma: "{-# OVERLAPPABLE" "#-}"
#line 721 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPABLE"; }
#line 3319 "parser.cc"
    break;

  case 98: // overlap_pragma: "{-# OVERLAPPING" "#-}"
#line 722 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPPING"; }
#line 3325 "parser.cc"
    break;

  case 99: // overlap_pragma: "{-# OVERLAPS" "#-}"
#line 723 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "OVERLAPS"; }
#line 3331 "parser.cc"
    break;

  case 100: // overlap_pragma: "{-# INCOHERENT" "#-}"
#line 724 "parser.y"
                                               { yylhs.value.as < std::optional<std::string> > () = "INCOHERENT"; }
#line 3337 "parser.cc"
    break;

  case 101: // overlap_pragma: %empty
#line 725 "parser.y"
                                               {}
#line 3343 "parser.cc"
    break;

  case 102: // deriv_strategy_no_via: "stock"
#line 727 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::stock;}
#line 3349 "parser.cc"
    break;

  case 103: // deriv_strategy_no_via: "anyclass"
#line 728 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::anyclass;}
#line 3355 "parser.cc"
    break;

  case 104: // deriv_strategy_no_via: "newtype"
#line 729 "parser.y"
                                    {yylhs.value.as < Hs::DerivingStrategy > () = Hs::DerivingStrategy::newtype;}
#line 3361 "parser.cc"
    break;

  case 105: // deriv_strategy_via: "via" type
#line 731 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3367 "parser.cc"
    break;

  case 106: // stand_alone_deriving: "deriving" "instance" inst_type
#line 734 "parser.y"
                             {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::StandaloneDerivingDecl({}, yystack_[0].value.as < Hs::LType > ())};}
#line 3373 "parser.cc"
    break;

  case 107: // stand_alone_deriving: "deriving" deriv_strategy_no_via "instance" inst_type
#line 736 "parser.y"
                             {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::StandaloneDerivingDecl(yystack_[2].value.as < Hs::DerivingStrategy > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3379 "parser.cc"
    break;

  case 108: // stand_alone_deriving: "deriving" deriv_strategy_via "instance" inst_type
#line 738 "parser.y"
                             {yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::StandaloneDerivingDecl(Hs::DerivingStrategy::via, yystack_[0].value.as < Hs::LType > (), yystack_[2].value.as < Hs::LType > ())};}
#line 3385 "parser.cc"
    break;

  case 114: // where_type_family: %empty
#line 752 "parser.y"
                                                           {}
#line 3391 "parser.cc"
    break;

  case 115: // where_type_family: "where" ty_fam_inst_eqn_list
#line 753 "parser.y"
                                                           {yylhs.value.as < std::optional<std::vector<Hs::TypeFamilyInstanceEqn>> > () = yystack_[0].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3397 "parser.cc"
    break;

  case 116: // ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
#line 755 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3403 "parser.cc"
    break;

  case 117: // ty_fam_inst_eqn_list: "vocurly" ty_fam_inst_eqns close
#line 756 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3409 "parser.cc"
    break;

  case 118: // ty_fam_inst_eqn_list: "{" ".." "}"
#line 757 "parser.y"
                                                           {}
#line 3415 "parser.cc"
    break;

  case 119: // ty_fam_inst_eqn_list: "vocurly" ".." close
#line 758 "parser.y"
                                                           {}
#line 3421 "parser.cc"
    break;

  case 120: // ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
#line 760 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[2].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > (); yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ().push_back(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ());}
#line 3427 "parser.cc"
    break;

  case 121: // ty_fam_inst_eqns: ty_fam_inst_eqns ";"
#line 761 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = yystack_[1].value.as < std::vector<Hs::TypeFamilyInstanceEqn> > ();}
#line 3433 "parser.cc"
    break;

  case 122: // ty_fam_inst_eqns: ty_fam_inst_eqn
#line 762 "parser.y"
                                                           {yylhs.value.as < std::vector<Hs::TypeFamilyInstanceEqn> > () = {yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ()};}
#line 3439 "parser.cc"
    break;

  case 123: // ty_fam_inst_eqns: %empty
#line 763 "parser.y"
                                                           {}
#line 3445 "parser.cc"
    break;

  case 124: // ty_fam_inst_eqn: type "=" ctype
#line 765 "parser.y"
                                                           {yylhs.value.as < Hs::TypeFamilyInstanceEqn > () = make_type_family_instance_eqn(yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ());}
#line 3451 "parser.cc"
    break;

  case 125: // at_decl_cls: "data" opt_family type opt_datafam_kind_sig
#line 768 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::DataFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3457 "parser.cc"
    break;

  case 126: // at_decl_cls: "type" type opt_at_kind_inj_sig
#line 770 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3463 "parser.cc"
    break;

  case 127: // at_decl_cls: "type" "family" type opt_at_kind_inj_sig
#line 772 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, make_family_decl(Hs::TypeFamily, yystack_[1].value.as < Hs::LType > (), yystack_[0].value.as < std::optional<Located<Hs::Kind>> > (), {})}; }
#line 3469 "parser.cc"
    break;

  case 128: // at_decl_cls: "type" ty_fam_inst_eqn
#line 774 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3475 "parser.cc"
    break;

  case 129: // at_decl_cls: "type" "instance" ty_fam_inst_eqn
#line 775 "parser.y"
                                                               { yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3481 "parser.cc"
    break;

  case 134: // at_decl_inst: "type" opt_instance ty_fam_inst_eqn
#line 783 "parser.y"
                                                              { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeFamilyInstanceDecl(yystack_[0].value.as < Hs::TypeFamilyInstanceEqn > ())};    }
#line 3487 "parser.cc"
    break;

  case 135: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
#line 786 "parser.y"
              {
		  auto& [tvs, context, type] = yystack_[2].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[5].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), {}, yystack_[1].value.as < Hs::ConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3497 "parser.cc"
    break;

  case 136: // at_decl_inst: data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
#line 793 "parser.y"
              {
		  auto& [tvs,context,type] = yystack_[3].value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > ();
		  auto [con, args] = check_type_or_class_header2(type);
		  yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::DataFamilyInstanceDecl(tvs, con, args, Hs::DataDefn(yystack_[6].value.as < Hs::DataOrNewtype > (), context ? unloc(*context) : Hs::Context(), yystack_[2].value.as < std::optional<Hs::Kind> > (), yystack_[1].value.as < Hs::GADTConstructorsDecl > (), yystack_[0].value.as < std::vector<Hs::Deriving> > ()))};
	      }
#line 3507 "parser.cc"
    break;

  case 137: // data_or_newtype: "data"
#line 799 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::data;}
#line 3513 "parser.cc"
    break;

  case 138: // data_or_newtype: "newtype"
#line 800 "parser.y"
                           {yylhs.value.as < Hs::DataOrNewtype > ()=Hs::DataOrNewtype::newtype;}
#line 3519 "parser.cc"
    break;

  case 139: // opt_class: %empty
#line 803 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {};}
#line 3525 "parser.cc"
    break;

  case 140: // opt_class: qtycon
#line 804 "parser.y"
                  {yylhs.value.as < std::optional<Located<std::string>> > () = {yystack_[0].location,yystack_[0].value.as < std::string > ()};}
#line 3531 "parser.cc"
    break;

  case 141: // opt_kind_sig: %empty
#line 808 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = {};}
#line 3537 "parser.cc"
    break;

  case 142: // opt_kind_sig: "::" kind
#line 809 "parser.y"
                           {yylhs.value.as < std::optional<Hs::Kind> > () = yystack_[0].value.as < Hs::Kind > ();}
#line 3543 "parser.cc"
    break;

  case 143: // opt_datafam_kind_sig: %empty
#line 811 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {};}
#line 3549 "parser.cc"
    break;

  case 144: // opt_datafam_kind_sig: "::" kind
#line 812 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3555 "parser.cc"
    break;

  case 145: // opt_tyfam_kind_sig: %empty
#line 814 "parser.y"
                                      {}
#line 3561 "parser.cc"
    break;

  case 146: // opt_tyfam_kind_sig: "::" kind
#line 815 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3567 "parser.cc"
    break;

  case 147: // opt_tyfam_kind_sig: "=" tv_bndr
#line 816 "parser.y"
                                      {}
#line 3573 "parser.cc"
    break;

  case 148: // opt_at_kind_inj_sig: %empty
#line 818 "parser.y"
                                      {}
#line 3579 "parser.cc"
    break;

  case 149: // opt_at_kind_inj_sig: "::" kind
#line 819 "parser.y"
                                      {yylhs.value.as < std::optional<Located<Hs::Kind>> > () = {{yystack_[0].location,yystack_[0].value.as < Hs::Kind > ()}};}
#line 3585 "parser.cc"
    break;

  case 150: // opt_at_kind_inj_sig: "=" tv_bndr_no_braces "|" injectivity_cond
#line 820 "parser.y"
                                                                  {}
#line 3591 "parser.cc"
    break;

  case 151: // tycl_hdr: context "=>" type
#line 823 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ()};}
#line 3597 "parser.cc"
    break;

  case 152: // tycl_hdr: type
#line 824 "parser.y"
                             {yylhs.value.as < std::pair<Hs::Context,Hs::LType> > () = {{},yystack_[0].value.as < Hs::LType > ()};}
#line 3603 "parser.cc"
    break;

  case 153: // datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type
#line 827 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[4].value.as < std::vector<Hs::LTypeVar> > ()}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3609 "parser.cc"
    break;

  case 154: // datafam_inst_hdr: "forall" tv_bndrs "=>" type
#line 828 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{yystack_[2].value.as < std::vector<Hs::LTypeVar> > ()}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3615 "parser.cc"
    break;

  case 155: // datafam_inst_hdr: context "=>" type
#line 829 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {{yystack_[2].location,yystack_[2].value.as < Hs::Context > ()}}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3621 "parser.cc"
    break;

  case 156: // datafam_inst_hdr: type
#line 830 "parser.y"
                                                          { yylhs.value.as < std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType> > () = {{}, {}, yystack_[0].value.as < Hs::LType > ()}; }
#line 3627 "parser.cc"
    break;

  case 160: // maybe_roles: %empty
#line 839 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = {};}
#line 3633 "parser.cc"
    break;

  case 161: // maybe_roles: roles
#line 840 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = yystack_[0].value.as < std::vector<Located<std::optional<Role>>> > ();}
#line 3639 "parser.cc"
    break;

  case 162: // roles: role
#line 842 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = {yystack_[0].value.as < Located<std::optional<Role>> > ()};}
#line 3645 "parser.cc"
    break;

  case 163: // roles: roles role
#line 843 "parser.y"
                                               {yylhs.value.as < std::vector<Located<std::optional<Role>>> > () = yystack_[1].value.as < std::vector<Located<std::optional<Role>>> > (); yylhs.value.as < std::vector<Located<std::optional<Role>>> > ().push_back(yystack_[0].value.as < Located<std::optional<Role>> > ());}
#line 3651 "parser.cc"
    break;

  case 164: // role: "VARID"
#line 846 "parser.y"
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
#line 3669 "parser.cc"
    break;

  case 165: // role: "_"
#line 859 "parser.y"
                                                {yylhs.value.as < Located<std::optional<Role>> > () = {yylhs.location, std::optional<Role>{}};}
#line 3675 "parser.cc"
    break;

  case 166: // decl_cls: at_decl_cls
#line 887 "parser.y"
                        {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3681 "parser.cc"
    break;

  case 167: // decl_cls: "default" var "::" sigtypedoc
#line 889 "parser.y"
           { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeSigDecl{{{yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())}},yystack_[0].value.as < Hs::LType > (),Hs::TypeSigKind::default_method}}; }
#line 3687 "parser.cc"
    break;

  case 168: // decl_cls: decl
#line 890 "parser.y"
                        {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3693 "parser.cc"
    break;

  case 169: // decls_cls: decls_cls ";" decl_cls
#line 892 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3699 "parser.cc"
    break;

  case 170: // decls_cls: decls_cls ";"
#line 893 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3705 "parser.cc"
    break;

  case 171: // decls_cls: decl_cls
#line 894 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3711 "parser.cc"
    break;

  case 172: // decls_cls: %empty
#line 895 "parser.y"
                                           {}
#line 3717 "parser.cc"
    break;

  case 173: // decllist_cls: "{" decls_cls "}"
#line 897 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3723 "parser.cc"
    break;

  case 174: // decllist_cls: "vocurly" decls_cls close
#line 898 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3729 "parser.cc"
    break;

  case 175: // where_cls: "where" decllist_cls
#line 900 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3735 "parser.cc"
    break;

  case 176: // where_cls: %empty
#line 901 "parser.y"
                                           {}
#line 3741 "parser.cc"
    break;

  case 177: // decl_inst: at_decl_inst
#line 903 "parser.y"
                                           {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3747 "parser.cc"
    break;

  case 178: // decl_inst: decl
#line 904 "parser.y"
                                           {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 3753 "parser.cc"
    break;

  case 179: // decls_inst: decls_inst ";" decl_inst
#line 906 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3759 "parser.cc"
    break;

  case 180: // decls_inst: decls_inst ";"
#line 907 "parser.y"
                                           {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3765 "parser.cc"
    break;

  case 181: // decls_inst: decl_inst
#line 908 "parser.y"
                                           {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3771 "parser.cc"
    break;

  case 182: // decls_inst: %empty
#line 909 "parser.y"
                                           {}
#line 3777 "parser.cc"
    break;

  case 183: // decllist_inst: "{" decls_inst "}"
#line 911 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3783 "parser.cc"
    break;

  case 184: // decllist_inst: "vocurly" decls_inst close
#line 912 "parser.y"
                                           {yylhs.value.as < Located<Hs::Decls> > () = {yystack_[1].location,yystack_[1].value.as < Hs::Decls > ()};}
#line 3789 "parser.cc"
    break;

  case 185: // where_inst: "where" decllist_inst
#line 914 "parser.y"
                                           {yylhs.value.as < std::optional<Located<Hs::Decls>> > () = yystack_[0].value.as < Located<Hs::Decls> > ();}
#line 3795 "parser.cc"
    break;

  case 186: // where_inst: %empty
#line 915 "parser.y"
                                           {}
#line 3801 "parser.cc"
    break;

  case 187: // decls: decls ";" decl
#line 918 "parser.y"
                        {yylhs.value.as < Hs::Decls > () = yystack_[2].value.as < Hs::Decls > (); yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3807 "parser.cc"
    break;

  case 188: // decls: decls ";"
#line 919 "parser.y"
                        {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3813 "parser.cc"
    break;

  case 189: // decls: decl
#line 920 "parser.y"
                        {yylhs.value.as < Hs::Decls > ().push_back(yystack_[0].value.as < Hs::LDecl > ());}
#line 3819 "parser.cc"
    break;

  case 190: // decls: %empty
#line 921 "parser.y"
                        {}
#line 3825 "parser.cc"
    break;

  case 191: // decllist: "{" decls "}"
#line 923 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3831 "parser.cc"
    break;

  case 192: // decllist: "vocurly" decls close
#line 924 "parser.y"
                                 {yylhs.value.as < Hs::Decls > () = yystack_[1].value.as < Hs::Decls > ();}
#line 3837 "parser.cc"
    break;

  case 193: // binds: decllist
#line 926 "parser.y"
                                 {yylhs.value.as < Located<Hs::Binds> > () = {yystack_[0].location,{yystack_[0].value.as < Hs::Decls > ()}};}
#line 3843 "parser.cc"
    break;

  case 194: // wherebinds: "where" binds
#line 928 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Hs::Binds>> > () = yystack_[0].value.as < Located<Hs::Binds> > ();}
#line 3849 "parser.cc"
    break;

  case 195: // wherebinds: %empty
#line 929 "parser.y"
                                 {}
#line 3855 "parser.cc"
    break;

  case 201: // opt_tyconsig: %empty
#line 955 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {};}
#line 3861 "parser.cc"
    break;

  case 202: // opt_tyconsig: "::" gtycon
#line 956 "parser.y"
                                 {yylhs.value.as < std::optional<Hs::LType> > () = {{yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())}};}
#line 3867 "parser.cc"
    break;

  case 203: // sigtype: ctype
#line 965 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3873 "parser.cc"
    break;

  case 204: // sigtypedoc: ctypedoc
#line 967 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3879 "parser.cc"
    break;

  case 205: // sig_vars: sig_vars "," var
#line 969 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > () = yystack_[2].value.as < std::vector<Hs::LVar> > (); yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3885 "parser.cc"
    break;

  case 206: // sig_vars: var
#line 970 "parser.y"
                           {yylhs.value.as < std::vector<Hs::LVar> > ().push_back({yystack_[0].location,Hs::Var(yystack_[0].value.as < std::string > ())});}
#line 3891 "parser.cc"
    break;

  case 207: // sigtypes1: sigtype
#line 972 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3897 "parser.cc"
    break;

  case 208: // sigtypes1: sigtypes1 "," sigtype
#line 973 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3903 "parser.cc"
    break;

  case 209: // ktype: ctype
#line 982 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 3909 "parser.cc"
    break;

  case 210: // ktype: ctype "::" kind
#line 983 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeOfKind(yystack_[2].value.as < Hs::LType > (), yystack_[0].value.as < Hs::Kind > ())};}
#line 3915 "parser.cc"
    break;

  case 211: // ctype: "forall" tv_bndrs "." ctype
#line 985 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ForallType(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < Hs::LType > ())};}
#line 3921 "parser.cc"
    break;

  case 212: // ctype: context "=>" ctype
#line 986 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ConstrainedType(yystack_[2].value.as < Hs::Context > (),yystack_[0].value.as < Hs::LType > ())};}
#line 3927 "parser.cc"
    break;

  case 213: // ctype: type
#line 988 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3933 "parser.cc"
    break;

  case 214: // ctypedoc: ctype
#line 990 "parser.y"
          { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3939 "parser.cc"
    break;

  case 215: // context: btype
#line 999 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(yystack_[0].value.as < Hs::LType > ());}
#line 3945 "parser.cc"
    break;

  case 216: // context_no_ops: btype_no_ops
#line 1001 "parser.y"
                                   {yylhs.value.as < Hs::Context > () = make_context(Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ()));}
#line 3951 "parser.cc"
    break;

  case 217: // type: btype
#line 1003 "parser.y"
      { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3957 "parser.cc"
    break;

  case 218: // type: btype "->" ctype
#line 1004 "parser.y"
                                   {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon("->")},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3963 "parser.cc"
    break;

  case 219: // typedoc: type
#line 1006 "parser.y"
         { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3969 "parser.cc"
    break;

  case 220: // btype: infixtype
#line 1009 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3975 "parser.cc"
    break;

  case 221: // infixtype: ftype
#line 1011 "parser.y"
           { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 3981 "parser.cc"
    break;

  case 222: // infixtype: btype tyop btype
#line 1012 "parser.y"
                                    {yylhs.value.as < Hs::LType > () = Hs::type_apply({{yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},yystack_[2].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ()});}
#line 3987 "parser.cc"
    break;

  case 223: // btype_no_ops: atype_docs
#line 1014 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3993 "parser.cc"
    break;

  case 224: // btype_no_ops: btype_no_ops atype_docs
#line 1015 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 3999 "parser.cc"
    break;

  case 225: // ftype: atype
#line 1017 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 4005 "parser.cc"
    break;

  case 226: // ftype: ftype tyarg
#line 1019 "parser.y"
                                   { yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeApp(yystack_[1].value.as < Hs::LType > (),yystack_[0].value.as < Hs::LType > ())}; }
#line 4011 "parser.cc"
    break;

  case 227: // ftype: ftype "@" atype
#line 1020 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[2].value.as < Hs::LType > (); }
#line 4017 "parser.cc"
    break;

  case 228: // tyarg: atype
#line 1022 "parser.y"
       { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 4023 "parser.cc"
    break;

  case 229: // tyop: qtyconop
#line 1024 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4029 "parser.cc"
    break;

  case 230: // tyop: tyvarop
#line 1025 "parser.y"
                                   {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4035 "parser.cc"
    break;

  case 231: // atype_docs: atype
#line 1032 "parser.y"
            { yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > (); }
#line 4041 "parser.cc"
    break;

  case 232: // atype: ntgtycon
#line 1039 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon(yystack_[0].value.as < std::string > ())};}
#line 4047 "parser.cc"
    break;

  case 233: // atype: tyvar
#line 1040 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 4053 "parser.cc"
    break;

  case 234: // atype: "*"
#line 1041 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("*")};}
#line 4059 "parser.cc"
    break;

  case 235: // atype: PREFIX_BANG atype
#line 1042 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::StrictType(yystack_[0].value.as < Hs::LType > ())};}
#line 4065 "parser.cc"
    break;

  case 236: // atype: PREFIX_TILDE atype
#line 1043 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::LazyType(yystack_[0].value.as < Hs::LType > ())};}
#line 4071 "parser.cc"
    break;

  case 237: // atype: "{" fielddecls "}"
#line 1044 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::FieldDecls(yystack_[1].value.as < std::vector<Hs::FieldDecl> > ())};}
#line 4077 "parser.cc"
    break;

  case 238: // atype: "(" ")"
#line 1045 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TypeCon("()")};}
#line 4083 "parser.cc"
    break;

  case 239: // atype: "(" comma_types1 "," ktype ")"
#line 1046 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<Hs::LType> > ();ts.push_back(yystack_[1].value.as < Hs::LType > ());yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::TupleType(ts)};}
#line 4089 "parser.cc"
    break;

  case 240: // atype: "[" ktype "]"
#line 1052 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = {yylhs.location,Hs::ListType{yystack_[1].value.as < Hs::LType > ()}}; }
#line 4095 "parser.cc"
    break;

  case 241: // atype: "(" ktype ")"
#line 1053 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[1].value.as < Hs::LType > ();}
#line 4101 "parser.cc"
    break;

  case 242: // inst_type: sigtype
#line 1056 "parser.y"
                                       {yylhs.value.as < Hs::LType > () = yystack_[0].value.as < Hs::LType > ();}
#line 4107 "parser.cc"
    break;

  case 243: // deriv_types: typedoc
#line 1058 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4113 "parser.cc"
    break;

  case 244: // deriv_types: typedoc "," deriv_types
#line 1059 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().insert(yylhs.value.as < std::vector<Hs::LType> > ().begin(), yystack_[2].value.as < Hs::LType > ());}
#line 4119 "parser.cc"
    break;

  case 245: // comma_types0: comma_types1
#line 1061 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[0].value.as < std::vector<Hs::LType> > ();}
#line 4125 "parser.cc"
    break;

  case 246: // comma_types0: %empty
#line 1062 "parser.y"
                                       { /* default construction OK */ }
#line 4131 "parser.cc"
    break;

  case 247: // comma_types1: ktype
#line 1064 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4137 "parser.cc"
    break;

  case 248: // comma_types1: comma_types1 "," ktype
#line 1065 "parser.y"
                                       {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[2].value.as < std::vector<Hs::LType> > (); yylhs.value.as < std::vector<Hs::LType> > ().push_back(yystack_[0].value.as < Hs::LType > ());}
#line 4143 "parser.cc"
    break;

  case 249: // tv_bndrs: tv_bndrs tv_bndr
#line 1072 "parser.y"
                               {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back(yystack_[0].value.as < Hs::LTypeVar > ());}
#line 4149 "parser.cc"
    break;

  case 250: // tv_bndrs: %empty
#line 1073 "parser.y"
                               { /* default construction OK */}
#line 4155 "parser.cc"
    break;

  case 251: // tv_bndr: tv_bndr_no_braces
#line 1075 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = yystack_[0].value.as < Hs::LTypeVar > ();}
#line 4161 "parser.cc"
    break;

  case 252: // tv_bndr: "{" tyvar "}"
#line 1076 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[1].location,Hs::TypeVar(yystack_[1].value.as < std::string > ())};}
#line 4167 "parser.cc"
    break;

  case 253: // tv_bndr: "{" tyvar "::" kind "}"
#line 1077 "parser.y"
                                 {yylhs.value.as < Hs::LTypeVar > () = {yystack_[3].location,Hs::TypeVar(yystack_[3].value.as < std::string > ())};}
#line 4173 "parser.cc"
    break;

  case 254: // tv_bndr_no_braces: tyvar
#line 1080 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[0].value.as < std::string > ())};}
#line 4179 "parser.cc"
    break;

  case 255: // tv_bndr_no_braces: "(" tyvar "::" kind ")"
#line 1081 "parser.y"
                                              {yylhs.value.as < Hs::LTypeVar > () = {yylhs.location,Hs::TypeVar(yystack_[3].value.as < std::string > (),yystack_[1].value.as < Hs::Kind > ())};}
#line 4185 "parser.cc"
    break;

  case 256: // fds: %empty
#line 1085 "parser.y"
                                    { /* default to empty */ }
#line 4191 "parser.cc"
    break;

  case 257: // fds: "|" fds1
#line 1086 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[0].value.as < std::vector<Hs::FunDep> > (); }
#line 4197 "parser.cc"
    break;

  case 258: // fds1: fds1 "," fd
#line 1088 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > () = yystack_[2].value.as < std::vector<Hs::FunDep> > (); yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4203 "parser.cc"
    break;

  case 259: // fds1: fd
#line 1089 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::FunDep> > ().push_back(yystack_[0].value.as < Hs::FunDep > ()); }
#line 4209 "parser.cc"
    break;

  case 260: // fd: varids0 "->" varids0
#line 1092 "parser.y"
                                    { yylhs.value.as < Hs::FunDep > () = Hs::FunDep(yystack_[2].value.as < std::vector<Hs::LTypeVar> > (), yystack_[0].value.as < std::vector<Hs::LTypeVar> > ()); }
#line 4215 "parser.cc"
    break;

  case 261: // varids0: varids0 tyvar
#line 1094 "parser.y"
                                    { yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > (); yylhs.value.as < std::vector<Hs::LTypeVar> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 4221 "parser.cc"
    break;

  case 262: // varids0: %empty
#line 1095 "parser.y"
                                    { /* default to empty */}
#line 4227 "parser.cc"
    break;

  case 263: // kind: ctype
#line 1100 "parser.y"
             {yylhs.value.as < Hs::Kind > () = type_to_kind(yystack_[0].value.as < Hs::LType > ());}
#line 4233 "parser.cc"
    break;

  case 264: // gadt_constrlist: "where" "{" gadt_constrs0 "}"
#line 1106 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4239 "parser.cc"
    break;

  case 265: // gadt_constrlist: "where" "vocurly" gadt_constrs0 close
#line 1107 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = yystack_[1].value.as < Hs::GADTConstructorsDecl > ();}
#line 4245 "parser.cc"
    break;

  case 266: // gadt_constrlist: %empty
#line 1108 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > () = {};}
#line 4251 "parser.cc"
    break;

  case 267: // gadt_constrs0: gadt_constrs
#line 1110 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[0].value.as < Hs::GADTConstructorsDecl > ();}
#line 4257 "parser.cc"
    break;

  case 268: // gadt_constrs0: %empty
#line 1111 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()={};}
#line 4263 "parser.cc"
    break;

  case 269: // gadt_constrs: gadt_constrs ";" gadt_constr
#line 1113 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ()=yystack_[2].value.as < Hs::GADTConstructorsDecl > (); yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4269 "parser.cc"
    break;

  case 270: // gadt_constrs: gadt_constr
#line 1114 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::GADTConstructorDecl > ());}
#line 4275 "parser.cc"
    break;

  case 271: // gadt_constr: optSemi con_list "::" sigtype
#line 1116 "parser.y"
                                                     {yylhs.value.as < Hs::GADTConstructorDecl > () = Hs::GADTConstructorDecl(yystack_[2].value.as < std::vector<Located<std::string>> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4281 "parser.cc"
    break;

  case 272: // constrs: "=" constrs1
#line 1118 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[0].value.as < Hs::ConstructorsDecl > ();}
#line 4287 "parser.cc"
    break;

  case 273: // constrs1: constrs1 "|" constr
#line 1120 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > () = yystack_[2].value.as < Hs::ConstructorsDecl > (); yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4293 "parser.cc"
    break;

  case 274: // constrs1: constr
#line 1121 "parser.y"
                                {yylhs.value.as < Hs::ConstructorsDecl > ().push_back(yystack_[0].value.as < Hs::ConstructorDecl > ());}
#line 4299 "parser.cc"
    break;

  case 275: // constr: forall context_no_ops "=>" constr_stuff
#line 1123 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[3].value.as < std::vector<Hs::LTypeVar> > (),yystack_[2].value.as < Hs::Context > (), yystack_[0].value.as < Hs::LType > ());}
#line 4305 "parser.cc"
    break;

  case 276: // constr: forall constr_stuff
#line 1124 "parser.y"
                                                {yylhs.value.as < Hs::ConstructorDecl > () = make_constructor(yystack_[1].value.as < std::vector<Hs::LTypeVar> > (),{}, yystack_[0].value.as < Hs::LType > ());}
#line 4311 "parser.cc"
    break;

  case 277: // forall: "forall" tv_bndrs "."
#line 1126 "parser.y"
                                {yylhs.value.as < std::vector<Hs::LTypeVar> > () = yystack_[1].value.as < std::vector<Hs::LTypeVar> > ();}
#line 4317 "parser.cc"
    break;

  case 278: // forall: %empty
#line 1127 "parser.y"
                                {}
#line 4323 "parser.cc"
    break;

  case 279: // constr_stuff: btype_no_ops
#line 1129 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ());}
#line 4329 "parser.cc"
    break;

  case 280: // constr_stuff: btype_no_ops conop btype_no_ops
#line 1130 "parser.y"
                                                {yylhs.value.as < Hs::LType > () = Hs::type_apply({
                                                                          {yystack_[1].location,Hs::TypeCon(yystack_[1].value.as < std::string > ())},
                                                                          Hs::type_apply(yystack_[2].value.as < std::vector<Hs::LType> > ()),
                                                                          Hs::type_apply(yystack_[0].value.as < std::vector<Hs::LType> > ())
                                                                       });}
#line 4339 "parser.cc"
    break;

  case 281: // fielddecls: %empty
#line 1136 "parser.y"
                                {}
#line 4345 "parser.cc"
    break;

  case 282: // fielddecls: fielddecls1
#line 1137 "parser.y"
                                {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[0].value.as < std::vector<Hs::FieldDecl> > ();}
#line 4351 "parser.cc"
    break;

  case 283: // fielddecls1: fielddecls1 "," fielddecl
#line 1139 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > () = yystack_[2].value.as < std::vector<Hs::FieldDecl> > (); yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4357 "parser.cc"
    break;

  case 284: // fielddecls1: fielddecl
#line 1140 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::FieldDecl> > ().push_back(yystack_[0].value.as < Hs::FieldDecl > ());}
#line 4363 "parser.cc"
    break;

  case 285: // fielddecl: sig_vars "::" ctype
#line 1142 "parser.y"
                                        {yylhs.value.as < Hs::FieldDecl > () = Hs::FieldDecl(yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ());}
#line 4369 "parser.cc"
    break;

  case 286: // maybe_derivings: %empty
#line 1144 "parser.y"
                            {}
#line 4375 "parser.cc"
    break;

  case 287: // maybe_derivings: derivings
#line 1145 "parser.y"
                            {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4381 "parser.cc"
    break;

  case 288: // derivings: derivings deriving
#line 1147 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[1].value.as < std::vector<Hs::Deriving> > (); yylhs.value.as < std::vector<Hs::Deriving> > ().insert(yylhs.value.as < std::vector<Hs::Deriving> > ().end(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().begin(), yystack_[0].value.as < std::vector<Hs::Deriving> > ().end());}
#line 4387 "parser.cc"
    break;

  case 289: // derivings: deriving
#line 1148 "parser.y"
                                    {yylhs.value.as < std::vector<Hs::Deriving> > () = yystack_[0].value.as < std::vector<Hs::Deriving> > ();}
#line 4393 "parser.cc"
    break;

  case 290: // deriving: "deriving" deriv_clause_types
#line 1151 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving({}, type));
          }
#line 4402 "parser.cc"
    break;

  case 291: // deriving: "deriving" deriv_strategy_no_via deriv_clause_types
#line 1156 "parser.y"
          {
              for(auto& type: yystack_[0].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(yystack_[1].value.as < Hs::DerivingStrategy > (), type));
          }
#line 4411 "parser.cc"
    break;

  case 292: // deriving: "deriving" deriv_clause_types deriv_strategy_via
#line 1161 "parser.y"
          {
              for(auto& type: yystack_[1].value.as < std::vector<Hs::LType> > ())
                  yylhs.value.as < std::vector<Hs::Deriving> > ().push_back(Hs::Deriving(Hs::DerivingStrategy::via, type, yystack_[0].value.as < Hs::LType > ()));
          }
#line 4420 "parser.cc"
    break;

  case 293: // deriv_clause_types: qtycondoc
#line 1166 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > ().push_back({yystack_[0].location,Hs::TypeCon(yystack_[0].value.as < std::string > ())});}
#line 4426 "parser.cc"
    break;

  case 294: // deriv_clause_types: "(" ")"
#line 1167 "parser.y"
                                        {}
#line 4432 "parser.cc"
    break;

  case 295: // deriv_clause_types: "(" deriv_types ")"
#line 1168 "parser.y"
                                        {yylhs.value.as < std::vector<Hs::LType> > () = yystack_[1].value.as < std::vector<Hs::LType> > ();}
#line 4438 "parser.cc"
    break;

  case 296: // decl_no_th: sigdecl
#line 1173 "parser.y"
                              {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 4444 "parser.cc"
    break;

  case 297: // decl_no_th: infixexp rhs
#line 1175 "parser.y"
                              {yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::ValueDecl({yystack_[1].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::MultiGuardedRHS > ())};}
#line 4450 "parser.cc"
    break;

  case 298: // decl: decl_no_th
#line 1177 "parser.y"
                              {yylhs.value.as < Hs::LDecl > () = yystack_[0].value.as < Hs::LDecl > ();}
#line 4456 "parser.cc"
    break;

  case 299: // rhs: "=" exp wherebinds
#line 1181 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Hs::LExp > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4462 "parser.cc"
    break;

  case 300: // rhs: gdrhs wherebinds
#line 1182 "parser.y"
                              {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS{yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ()};}
#line 4468 "parser.cc"
    break;

  case 301: // gdrhs: gdrhs gdrh
#line 1184 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4474 "parser.cc"
    break;

  case 302: // gdrhs: gdrh
#line 1185 "parser.y"
                              {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4480 "parser.cc"
    break;

  case 303: // gdrh: "|" guardquals "=" exp
#line 1189 "parser.y"
                              {yylhs.value.as < Hs::GuardedRHS > () = Hs::GuardedRHS{yystack_[2].value.as < std::vector<Hs::LStmt> > (),yystack_[0].value.as < Hs::LExp > ()};}
#line 4486 "parser.cc"
    break;

  case 304: // sigdecl: sig_vars "::" sigtypedoc
#line 1199 "parser.y"
                                  { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::TypeSigDecl{yystack_[2].value.as < std::vector<Hs::LVar> > (),yystack_[0].value.as < Hs::LType > ()}}; }
#line 4492 "parser.cc"
    break;

  case 305: // sigdecl: infix prec ops
#line 1200 "parser.y"
                         { yylhs.value.as < Hs::LDecl > () = {yylhs.location,Hs::FixityDecl{yystack_[2].value.as < Hs::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<Located<std::string>> > ()}}; }
#line 4498 "parser.cc"
    break;

  case 306: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1202 "parser.y"
                                                    {}
#line 4504 "parser.cc"
    break;

  case 307: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1203 "parser.y"
                                            { yylhs.value.as < Hs::LDecl > () = {yylhs.location, Hs::InlinePragma({yystack_[3].location,yystack_[3].value.as < std::string > ()},{yystack_[1].location,yystack_[1].value.as < std::string > ()})}; }
#line 4510 "parser.cc"
    break;

  case 308: // sigdecl: "{-# SCC" qvar "#-}"
#line 1204 "parser.y"
                              {}
#line 4516 "parser.cc"
    break;

  case 309: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1205 "parser.y"
                                     {}
#line 4522 "parser.cc"
    break;

  case 310: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1206 "parser.y"
                                                               {}
#line 4528 "parser.cc"
    break;

  case 311: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1207 "parser.y"
                                                                      {}
#line 4534 "parser.cc"
    break;

  case 312: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1208 "parser.y"
                                                     {}
#line 4540 "parser.cc"
    break;

  case 317: // exp: infixexp "::" sigtype
#line 1220 "parser.y"
                           { yylhs.value.as < Hs::LExp > () = {yystack_[2].location+yystack_[0].location, Hs::TypedExp({yystack_[2].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[2].value.as < Located<Hs::InfixExp> > ())},yystack_[0].value.as < Hs::LType > ())}; }
#line 4546 "parser.cc"
    break;

  case 318: // exp: infixexp
#line 1221 "parser.y"
                           { yylhs.value.as < Hs::LExp > () = {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())}; }
#line 4552 "parser.cc"
    break;

  case 319: // infixexp: exp10
#line 1225 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = {yystack_[0].location,Hs::InfixExp({yystack_[0].value.as < Hs::LExp > ()})};}
#line 4558 "parser.cc"
    break;

  case 320: // infixexp: infixexp qop exp10
#line 1226 "parser.y"
                                {yylhs.value.as < Located<Hs::InfixExp> > () = yystack_[2].value.as < Located<Hs::InfixExp> > (); yylhs.value.as < Located<Hs::InfixExp> > ().loc = yystack_[2].location+yystack_[0].location; unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back({yystack_[1].location,yystack_[1].value.as < Hs::Exp > ()}); unloc(yylhs.value.as < Located<Hs::InfixExp> > ()).terms.push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4564 "parser.cc"
    break;

  case 321: // exp10: PREFIX_MINUS fexp
#line 1228 "parser.y"
                                        {yylhs.value.as < Hs::LExp > () = {yystack_[1].location+yystack_[0].location,Hs::InfixExp( { {yystack_[1].location,Hs::Neg()}, yystack_[0].value.as < Hs::LExp > ()} )};}
#line 4570 "parser.cc"
    break;

  case 322: // exp10: fexp
#line 1229 "parser.y"
                               {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4576 "parser.cc"
    break;

  case 325: // fexp: fexp aexp
#line 1237 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = make_parsed_app(yylhs.location, yystack_[1].value.as < Hs::LExp > (), yystack_[0].value.as < Hs::LExp > ());}
#line 4582 "parser.cc"
    break;

  case 326: // fexp: fexp "@" atype
#line 1238 "parser.y"
                                 {}
#line 4588 "parser.cc"
    break;

  case 327: // fexp: "static" aexp
#line 1239 "parser.y"
                                 {}
#line 4594 "parser.cc"
    break;

  case 328: // fexp: aexp
#line 1240 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4600 "parser.cc"
    break;

  case 329: // aexp: qvar TIGHT_INFIX_AT aexp
#line 1243 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedAsPattern({yystack_[2].location,Hs::Var(yystack_[2].value.as < std::string > ())},yystack_[0].value.as < Hs::LExp > ())}; }
#line 4606 "parser.cc"
    break;

  case 330: // aexp: PREFIX_TILDE aexp
#line 1244 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedLazyPattern(yystack_[0].value.as < Hs::LExp > ())}; }
#line 4612 "parser.cc"
    break;

  case 331: // aexp: PREFIX_BANG aexp
#line 1245 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedStrictPattern(yystack_[0].value.as < Hs::LExp > ())}; }
#line 4618 "parser.cc"
    break;

  case 332: // aexp: "\\" apats1 "->" exp
#line 1246 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedLambda(yystack_[2].value.as < std::vector<Hs::LExp> > (),yystack_[0].value.as < Hs::LExp > ())}; }
#line 4624 "parser.cc"
    break;

  case 333: // aexp: "let" binds "in" exp
#line 1247 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Let(yystack_[2].value.as < Located<Hs::Binds> > (),yystack_[0].value.as < Hs::LExp > ())}; }
#line 4630 "parser.cc"
    break;

  case 334: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1249 "parser.y"
                                                       {yylhs.value.as < Hs::LExp > () = {yystack_[7].location+yystack_[0].location,Hs::If(yystack_[6].value.as < Hs::LExp > (),yystack_[3].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::LExp > ())}; }
#line 4636 "parser.cc"
    break;

  case 335: // aexp: "if" ifgdpats
#line 1250 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::MultiWayIf(yystack_[0].value.as < std::vector<Hs::GuardedRHS> > ())}; }
#line 4642 "parser.cc"
    break;

  case 336: // aexp: "case" exp "of" altslist
#line 1251 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedCase(yystack_[2].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::ParsedAlts > ())}; }
#line 4648 "parser.cc"
    break;

  case 337: // aexp: "do" stmtlist
#line 1252 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Do(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4654 "parser.cc"
    break;

  case 338: // aexp: "mdo" stmtlist
#line 1253 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::MDo(yystack_[0].value.as < Hs::Stmts > ())}; }
#line 4660 "parser.cc"
    break;

  case 339: // aexp: aexp1
#line 1255 "parser.y"
                                 {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4666 "parser.cc"
    break;

  case 340: // aexp1: aexp1 "{" fbinds "}"
#line 1258 "parser.y"
                                     { yylhs.value.as < Hs::LExp > () = make_record_expression(yylhs.location, yystack_[3].value.as < Hs::LExp > (), yystack_[1].value.as < Located<Hs::FieldBindings> > ()); }
#line 4672 "parser.cc"
    break;

  case 341: // aexp1: aexp1 TIGHT_INFIX_DOT field
#line 1259 "parser.y"
                                     { yylhs.value.as < Hs::LExp > () = make_record_field_selection(yylhs.location, yystack_[2].value.as < Hs::LExp > (), yystack_[0].value.as < std::string > ()); }
#line 4678 "parser.cc"
    break;

  case 342: // aexp1: aexp2
#line 1260 "parser.y"
                                     { yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > (); }
#line 4684 "parser.cc"
    break;

  case 343: // aexp2: qvar
#line 1263 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Var(yystack_[0].value.as < std::string > ())};}
#line 4690 "parser.cc"
    break;

  case 344: // aexp2: qcon
#line 1264 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Con(yystack_[0].value.as < std::string > ())};}
#line 4696 "parser.cc"
    break;

  case 345: // aexp2: literal
#line 1265 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, yystack_[0].value.as < Hs::Exp > ()};}
#line 4702 "parser.cc"
    break;

  case 346: // aexp2: "(" texp ")"
#line 1266 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, unloc(yystack_[1].value.as < Hs::LExp > ())};}
#line 4708 "parser.cc"
    break;

  case 347: // aexp2: "(" tup_exprs ")"
#line 1267 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::Tuple(yystack_[1].value.as < std::vector<Hs::LExp> > ())};}
#line 4714 "parser.cc"
    break;

  case 348: // aexp2: "(" projection ")"
#line 1268 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = make_record_projection(yylhs.location, yystack_[1].value.as < std::vector<std::string> > ());}
#line 4720 "parser.cc"
    break;

  case 349: // aexp2: "[" list "]"
#line 1273 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, yystack_[1].value.as < Hs::Exp > ()};}
#line 4726 "parser.cc"
    break;

  case 350: // aexp2: "_"
#line 1274 "parser.y"
                              {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::ParsedWildcardPattern()};}
#line 4732 "parser.cc"
    break;

  case 351: // projection: projection TIGHT_INFIX_DOT field
#line 1277 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4738 "parser.cc"
    break;

  case 352: // projection: PREFIX_DOT field
#line 1278 "parser.y"
                                              {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 4744 "parser.cc"
    break;

  case 353: // texp: exp
#line 1283 "parser.y"
                      {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4750 "parser.cc"
    break;

  case 354: // texp: infixexp qop
#line 1284 "parser.y"
                      {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::LeftSection ( {yystack_[1].value.as < Located<Hs::InfixExp> > ().loc, unloc(yystack_[1].value.as < Located<Hs::InfixExp> > ())}, {yystack_[0].location,yystack_[0].value.as < Hs::Exp > ()} )}; }
#line 4756 "parser.cc"
    break;

  case 355: // texp: qopm infixexp
#line 1285 "parser.y"
                      {yylhs.value.as < Hs::LExp > () = {yylhs.location, Hs::RightSection( {yystack_[1].location,yystack_[1].value.as < Hs::Exp > ()}, {yystack_[0].value.as < Located<Hs::InfixExp> > ().loc,unloc(yystack_[0].value.as < Located<Hs::InfixExp> > ())} )}; }
#line 4762 "parser.cc"
    break;

  case 356: // tup_exprs: tup_exprs "," texp
#line 1290 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LExp> > () = yystack_[2].value.as < std::vector<Hs::LExp> > (); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4768 "parser.cc"
    break;

  case 357: // tup_exprs: texp "," texp
#line 1291 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[2].value.as < Hs::LExp > ()); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4774 "parser.cc"
    break;

  case 358: // list: texp
#line 1309 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::List({yystack_[0].value.as < Hs::LExp > ()}); }
#line 4780 "parser.cc"
    break;

  case 359: // list: lexps
#line 1310 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::List(yystack_[0].value.as < std::vector<Hs::LExp> > ()); }
#line 4786 "parser.cc"
    break;

  case 360: // list: texp ".."
#line 1311 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFrom(yystack_[1].value.as < Hs::LExp > ()); }
#line 4792 "parser.cc"
    break;

  case 361: // list: texp "," exp ".."
#line 1312 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFromThen(yystack_[3].value.as < Hs::LExp > (),yystack_[1].value.as < Hs::LExp > ()); }
#line 4798 "parser.cc"
    break;

  case 362: // list: texp ".." exp
#line 1313 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFromTo(yystack_[2].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::LExp > ()); }
#line 4804 "parser.cc"
    break;

  case 363: // list: texp "," exp ".." exp
#line 1314 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListFromThenTo(yystack_[4].value.as < Hs::LExp > (), yystack_[2].value.as < Hs::LExp > (), yystack_[0].value.as < Hs::LExp > ()); }
#line 4810 "parser.cc"
    break;

  case 364: // list: texp "|" squals
#line 1315 "parser.y"
                                 { yylhs.value.as < Hs::Exp > () = Hs::ListComprehension(yystack_[2].value.as < Hs::LExp > (), yystack_[0].value.as < std::vector<Hs::LStmt> > ()); }
#line 4816 "parser.cc"
    break;

  case 365: // lexps: lexps "," texp
#line 1317 "parser.y"
                                 { yylhs.value.as < std::vector<Hs::LExp> > () = yystack_[2].value.as < std::vector<Hs::LExp> > (); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4822 "parser.cc"
    break;

  case 366: // lexps: texp "," texp
#line 1318 "parser.y"
                                 { yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[2].value.as < Hs::LExp > ()); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4828 "parser.cc"
    break;

  case 367: // squals: squals "," qual
#line 1331 "parser.y"
                                          {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[2].value.as < std::vector<Hs::LStmt> > (); yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4834 "parser.cc"
    break;

  case 368: // squals: qual
#line 1333 "parser.y"
                                          {yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4840 "parser.cc"
    break;

  case 369: // guardquals: guardquals1
#line 1343 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[0].value.as < std::vector<Hs::LStmt> > ();}
#line 4846 "parser.cc"
    break;

  case 370: // guardquals1: guardquals1 "," qual
#line 1345 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[2].value.as < std::vector<Hs::LStmt> > ();yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4852 "parser.cc"
    break;

  case 371: // guardquals1: qual
#line 1346 "parser.y"
                                   {yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 4858 "parser.cc"
    break;

  case 372: // altslist: "{" alts "}"
#line 1349 "parser.y"
                                 {yylhs.value.as < Hs::ParsedAlts > () = Hs::ParsedAlts{yystack_[1].value.as < std::vector<Located<Hs::ParsedAlt>> > ()};}
#line 4864 "parser.cc"
    break;

  case 373: // altslist: "vocurly" alts close
#line 1350 "parser.y"
                                 {yylhs.value.as < Hs::ParsedAlts > () = Hs::ParsedAlts{yystack_[1].value.as < std::vector<Located<Hs::ParsedAlt>> > ()};}
#line 4870 "parser.cc"
    break;

  case 374: // altslist: "{" "}"
#line 1351 "parser.y"
                                 {}
#line 4876 "parser.cc"
    break;

  case 375: // altslist: "vocurly" close
#line 1352 "parser.y"
                                 {}
#line 4882 "parser.cc"
    break;

  case 376: // alts: alts1
#line 1354 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[0].value.as < std::vector<Located<Hs::ParsedAlt>> > ();}
#line 4888 "parser.cc"
    break;

  case 377: // alts: ";" alts
#line 1355 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[0].value.as < std::vector<Located<Hs::ParsedAlt>> > ();}
#line 4894 "parser.cc"
    break;

  case 378: // alts1: alts1 ";" alt
#line 1357 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[2].value.as < std::vector<Located<Hs::ParsedAlt>> > (); yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > ().push_back(yystack_[0].value.as < Located<Hs::ParsedAlt> > ());}
#line 4900 "parser.cc"
    break;

  case 379: // alts1: alts1 ";"
#line 1358 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > () = yystack_[1].value.as < std::vector<Located<Hs::ParsedAlt>> > ();}
#line 4906 "parser.cc"
    break;

  case 380: // alts1: alt
#line 1359 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Hs::ParsedAlt>> > ().push_back(yystack_[0].value.as < Located<Hs::ParsedAlt> > ());}
#line 4912 "parser.cc"
    break;

  case 381: // alt: pat alt_rhs
#line 1361 "parser.y"
                                 {yylhs.value.as < Located<Hs::ParsedAlt> > () = Located<Hs::ParsedAlt>{yystack_[1].location+yystack_[0].location,{yystack_[1].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::MultiGuardedRHS > ()}};}
#line 4918 "parser.cc"
    break;

  case 382: // alt_rhs: "->" exp wherebinds
#line 1363 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::SimpleRHS(yystack_[1].value.as < Hs::LExp > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4924 "parser.cc"
    break;

  case 383: // alt_rhs: gdpats wherebinds
#line 1364 "parser.y"
                                 {yylhs.value.as < Hs::MultiGuardedRHS > () = Hs::MultiGuardedRHS(yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Hs::Binds>> > ());}
#line 4930 "parser.cc"
    break;

  case 384: // gdpats: gdpats gdpat
#line 1366 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > (); yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4936 "parser.cc"
    break;

  case 385: // gdpats: gdpat
#line 1367 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > ().push_back(yystack_[0].value.as < Hs::GuardedRHS > ());}
#line 4942 "parser.cc"
    break;

  case 386: // ifgdpats: "{" gdpats "}"
#line 1369 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > ();}
#line 4948 "parser.cc"
    break;

  case 387: // ifgdpats: gdpats close
#line 1370 "parser.y"
                                 {yylhs.value.as < std::vector<Hs::GuardedRHS> > () = yystack_[1].value.as < std::vector<Hs::GuardedRHS> > ();}
#line 4954 "parser.cc"
    break;

  case 388: // gdpat: "|" guardquals "->" exp
#line 1372 "parser.y"
                                 {yylhs.value.as < Hs::GuardedRHS > ()=Hs::GuardedRHS{yystack_[2].value.as < std::vector<Hs::LStmt> > (),yystack_[0].value.as < Hs::LExp > ()};}
#line 4960 "parser.cc"
    break;

  case 389: // pat: exp
#line 1374 "parser.y"
              {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4966 "parser.cc"
    break;

  case 390: // bindpat: exp
#line 1376 "parser.y"
              {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4972 "parser.cc"
    break;

  case 391: // apat: aexp
#line 1378 "parser.y"
              {yylhs.value.as < Hs::LExp > () = yystack_[0].value.as < Hs::LExp > ();}
#line 4978 "parser.cc"
    break;

  case 392: // apats1: apats1 apat
#line 1380 "parser.y"
                    {yylhs.value.as < std::vector<Hs::LExp> > () = yystack_[1].value.as < std::vector<Hs::LExp> > (); yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4984 "parser.cc"
    break;

  case 393: // apats1: apat
#line 1381 "parser.y"
                    {yylhs.value.as < std::vector<Hs::LExp> > ().push_back(yystack_[0].value.as < Hs::LExp > ());}
#line 4990 "parser.cc"
    break;

  case 394: // stmtlist: "{" stmts "}"
#line 1384 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Hs::LStmt> > ()};}
#line 4996 "parser.cc"
    break;

  case 395: // stmtlist: "vocurly" stmts close
#line 1385 "parser.y"
                               {yylhs.value.as < Hs::Stmts > () = Hs::Stmts{yystack_[1].value.as < std::vector<Hs::LStmt> > ()};}
#line 5002 "parser.cc"
    break;

  case 396: // stmts: stmts ";" stmt
#line 1387 "parser.y"
                       {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[2].value.as < std::vector<Hs::LStmt> > (); yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 5008 "parser.cc"
    break;

  case 397: // stmts: stmts ";"
#line 1388 "parser.y"
                       {yylhs.value.as < std::vector<Hs::LStmt> > () = yystack_[1].value.as < std::vector<Hs::LStmt> > ();}
#line 5014 "parser.cc"
    break;

  case 398: // stmts: stmt
#line 1389 "parser.y"
                       {yylhs.value.as < std::vector<Hs::LStmt> > ().push_back(yystack_[0].value.as < Hs::LStmt > ());}
#line 5020 "parser.cc"
    break;

  case 399: // stmts: %empty
#line 1390 "parser.y"
                       {}
#line 5026 "parser.cc"
    break;

  case 400: // stmt: qual
#line 1395 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = yystack_[0].value.as < Hs::LStmt > ();}
#line 5032 "parser.cc"
    break;

  case 401: // stmt: "rec" stmtlist
#line 1396 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::RecStmt(yystack_[0].value.as < Hs::Stmts > ())};}
#line 5038 "parser.cc"
    break;

  case 402: // qual: bindpat "<-" exp
#line 1398 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::ParsedPatQual(yystack_[2].value.as < Hs::LExp > (),yystack_[0].value.as < Hs::LExp > ())};}
#line 5044 "parser.cc"
    break;

  case 403: // qual: exp
#line 1399 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::SimpleQual(yystack_[0].value.as < Hs::LExp > ())};}
#line 5050 "parser.cc"
    break;

  case 404: // qual: "let" binds
#line 1400 "parser.y"
                        {yylhs.value.as < Hs::LStmt > () = {yylhs.location, Hs::LetQual(yystack_[0].value.as < Located<Hs::Binds> > ())};}
#line 5056 "parser.cc"
    break;

  case 405: // fbinds: fbinds1
#line 1405 "parser.y"
                        {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > ();}
#line 5062 "parser.cc"
    break;

  case 406: // fbinds: %empty
#line 1406 "parser.y"
                        {}
#line 5068 "parser.cc"
    break;

  case 407: // fbinds1: fbind "," fbinds1
#line 1408 "parser.y"
                            {yylhs.value.as < Located<Hs::FieldBindings> > () = yystack_[0].value.as < Located<Hs::FieldBindings> > (); unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.insert(unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.begin(), *yystack_[2].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5074 "parser.cc"
    break;

  case 408: // fbinds1: fbind
#line 1409 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).fields.push_back(*yystack_[0].value.as < std::optional<Located<Hs::FieldBinding>> > ()); yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5080 "parser.cc"
    break;

  case 409: // fbinds1: ".."
#line 1410 "parser.y"
                            {unloc(yylhs.value.as < Located<Hs::FieldBindings> > ()).dotdot = yylhs.location; yylhs.value.as < Located<Hs::FieldBindings> > ().loc = yylhs.location;}
#line 5086 "parser.cc"
    break;

  case 410: // fbind: qvar "=" texp
#line 1412 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[2].value.as < std::string > ())}, yystack_[0].value.as < Hs::LExp > ())}};}
#line 5092 "parser.cc"
    break;

  case 411: // fbind: qvar
#line 1413 "parser.y"
                            {yylhs.value.as < std::optional<Located<Hs::FieldBinding>> > () = {{yylhs.location, Hs::FieldBinding({yylhs.location,Hs::Var(yystack_[0].value.as < std::string > ())})}};}
#line 5098 "parser.cc"
    break;

  case 412: // fbind: field TIGHT_INFIX_DOT fieldToUpdate "=" texp
#line 1414 "parser.y"
                                                      {}
#line 5104 "parser.cc"
    break;

  case 413: // fbind: field TIGHT_INFIX_DOT fieldToUpdate
#line 1415 "parser.y"
                                                      {}
#line 5110 "parser.cc"
    break;

  case 416: // qcon: gen_qcon
#line 1451 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5116 "parser.cc"
    break;

  case 417: // qcon: sysdcon
#line 1452 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5122 "parser.cc"
    break;

  case 418: // gen_qcon: qconid
#line 1454 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5128 "parser.cc"
    break;

  case 419: // gen_qcon: "(" qconsym ")"
#line 1455 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5134 "parser.cc"
    break;

  case 420: // con: conid
#line 1457 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5140 "parser.cc"
    break;

  case 421: // con: "(" consym ")"
#line 1458 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5146 "parser.cc"
    break;

  case 422: // con: sysdcon
#line 1459 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5152 "parser.cc"
    break;

  case 423: // con_list: con_list "," con
#line 1461 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > () = yystack_[2].value.as < std::vector<Located<std::string>> > (); yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 5158 "parser.cc"
    break;

  case 424: // con_list: con
#line 1462 "parser.y"
                            { yylhs.value.as < std::vector<Located<std::string>> > ().push_back({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 5164 "parser.cc"
    break;

  case 425: // sysdcon_no_list: "(" ")"
#line 1464 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 5170 "parser.cc"
    break;

  case 426: // sysdcon_no_list: "(" commas ")"
#line 1465 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5176 "parser.cc"
    break;

  case 427: // sysdcon_no_list: "(#" "#)"
#line 1466 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 5182 "parser.cc"
    break;

  case 428: // sysdcon_no_list: "(#" commas "#)"
#line 1467 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5188 "parser.cc"
    break;

  case 429: // sysdcon: sysdcon_no_list
#line 1469 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5194 "parser.cc"
    break;

  case 430: // sysdcon: "[" "]"
#line 1470 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 5200 "parser.cc"
    break;

  case 431: // conop: consym
#line 1472 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5206 "parser.cc"
    break;

  case 432: // conop: "`" conid "`"
#line 1473 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5212 "parser.cc"
    break;

  case 433: // qconop: qconsym
#line 1475 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5218 "parser.cc"
    break;

  case 434: // qconop: "`" qconid "`"
#line 1476 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5224 "parser.cc"
    break;

  case 435: // gtycon: ntgtycon
#line 1479 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5230 "parser.cc"
    break;

  case 436: // gtycon: "(" ")"
#line 1480 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 5236 "parser.cc"
    break;

  case 437: // gtycon: "(#" "#)"
#line 1481 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 5242 "parser.cc"
    break;

  case 438: // ntgtycon: oqtycon
#line 1483 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5248 "parser.cc"
    break;

  case 439: // ntgtycon: "(" commas ")"
#line 1484 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 5254 "parser.cc"
    break;

  case 440: // ntgtycon: "(#" commas "#)"
#line 1485 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 5260 "parser.cc"
    break;

  case 441: // ntgtycon: "(" "->" ")"
#line 1486 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 5266 "parser.cc"
    break;

  case 442: // ntgtycon: "[" "]"
#line 1487 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 5272 "parser.cc"
    break;

  case 443: // oqtycon: qtycon
#line 1489 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5278 "parser.cc"
    break;

  case 444: // oqtycon: "(" qtyconsym ")"
#line 1490 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5284 "parser.cc"
    break;

  case 445: // oqtycon_no_varcon: qtycon
#line 1492 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5290 "parser.cc"
    break;

  case 446: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1493 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5296 "parser.cc"
    break;

  case 447: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1494 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5302 "parser.cc"
    break;

  case 448: // oqtycon_no_varcon: "(" ":" ")"
#line 1495 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 5308 "parser.cc"
    break;

  case 449: // qtyconop: qtyconsym
#line 1498 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5314 "parser.cc"
    break;

  case 450: // qtyconop: "`" qtycon "`"
#line 1499 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5320 "parser.cc"
    break;

  case 451: // qtycondoc: qtycon
#line 1501 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5326 "parser.cc"
    break;

  case 452: // qtycon: "QCONID"
#line 1503 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5332 "parser.cc"
    break;

  case 453: // qtycon: tycon
#line 1504 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5338 "parser.cc"
    break;

  case 454: // tycon: "CONID"
#line 1508 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5344 "parser.cc"
    break;

  case 455: // qtyconsym: "QCONSYM"
#line 1510 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5350 "parser.cc"
    break;

  case 456: // qtyconsym: "QVARSYM"
#line 1511 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5356 "parser.cc"
    break;

  case 457: // qtyconsym: tyconsym
#line 1512 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5362 "parser.cc"
    break;

  case 458: // tyconsym: "CONSYM"
#line 1514 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5368 "parser.cc"
    break;

  case 459: // tyconsym: "VARSYM"
#line 1515 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5374 "parser.cc"
    break;

  case 460: // tyconsym: ":"
#line 1516 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5380 "parser.cc"
    break;

  case 461: // tyconsym: "-"
#line 1517 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 5386 "parser.cc"
    break;

  case 462: // op: varop
#line 1522 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5392 "parser.cc"
    break;

  case 463: // op: conop
#line 1523 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5398 "parser.cc"
    break;

  case 464: // varop: varsym
#line 1525 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5404 "parser.cc"
    break;

  case 465: // varop: "`" varid "`"
#line 1526 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5410 "parser.cc"
    break;

  case 466: // qop: qvarop
#line 1528 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5416 "parser.cc"
    break;

  case 467: // qop: qconop
#line 1529 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5422 "parser.cc"
    break;

  case 468: // qopm: qvaropm
#line 1532 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Var(yystack_[0].value.as < std::string > ()); }
#line 5428 "parser.cc"
    break;

  case 469: // qopm: qconop
#line 1533 "parser.y"
                { yylhs.value.as < Hs::Exp > () = Hs::Con(yystack_[0].value.as < std::string > ()); }
#line 5434 "parser.cc"
    break;

  case 470: // qvarop: qvarsym
#line 1538 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5440 "parser.cc"
    break;

  case 471: // qvarop: "`" qvarid "`"
#line 1539 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5446 "parser.cc"
    break;

  case 472: // qvaropm: qvarsym_no_minus
#line 1541 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 5452 "parser.cc"
    break;

  case 473: // qvaropm: "`" qvarid "`"
#line 1542 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5458 "parser.cc"
    break;

  case 474: // tyvar: tyvarid
#line 1546 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5464 "parser.cc"
    break;

  case 475: // tyvarop: "`" tyvarid "`"
#line 1548 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5470 "parser.cc"
    break;

  case 476: // tyvarid: "VARID"
#line 1550 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5476 "parser.cc"
    break;

  case 477: // tyvarid: special_id
#line 1551 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5482 "parser.cc"
    break;

  case 478: // tyvarid: "unsafe"
#line 1552 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 5488 "parser.cc"
    break;

  case 479: // tyvarid: "safe"
#line 1553 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 5494 "parser.cc"
    break;

  case 480: // tyvarid: "interruptible"
#line 1554 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 5500 "parser.cc"
    break;

  case 481: // var: varid
#line 1557 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5506 "parser.cc"
    break;

  case 482: // var: "(" varsym ")"
#line 1558 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5512 "parser.cc"
    break;

  case 483: // qvar: qvarid
#line 1560 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5518 "parser.cc"
    break;

  case 484: // qvar: "(" varsym ")"
#line 1561 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5524 "parser.cc"
    break;

  case 485: // qvar: "(" qvarsym1 ")"
#line 1562 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 5530 "parser.cc"
    break;

  case 486: // field: varid
#line 1564 "parser.y"
       { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5536 "parser.cc"
    break;

  case 487: // qvarid: varid
#line 1566 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5542 "parser.cc"
    break;

  case 488: // qvarid: "QVARID"
#line 1567 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5548 "parser.cc"
    break;

  case 489: // varid: "VARID"
#line 1569 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5554 "parser.cc"
    break;

  case 490: // varid: special_id
#line 1570 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5560 "parser.cc"
    break;

  case 491: // varid: "unsafe"
#line 1571 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 5566 "parser.cc"
    break;

  case 492: // varid: "safe"
#line 1572 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 5572 "parser.cc"
    break;

  case 493: // varid: "interruptible"
#line 1573 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 5578 "parser.cc"
    break;

  case 494: // varid: "forall"
#line 1574 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 5584 "parser.cc"
    break;

  case 495: // varid: "family"
#line 1575 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 5590 "parser.cc"
    break;

  case 496: // varid: "role"
#line 1576 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 5596 "parser.cc"
    break;

  case 497: // qvarsym: varsym
#line 1578 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5602 "parser.cc"
    break;

  case 498: // qvarsym: qvarsym1
#line 1579 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5608 "parser.cc"
    break;

  case 499: // qvarsym_no_minus: varsym_no_minus
#line 1581 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5614 "parser.cc"
    break;

  case 500: // qvarsym_no_minus: qvarsym1
#line 1582 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 5620 "parser.cc"
    break;

  case 501: // qvarsym1: "QVARSYM"
#line 1584 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5626 "parser.cc"
    break;

  case 502: // varsym: varsym_no_minus
#line 1586 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5632 "parser.cc"
    break;

  case 503: // varsym: "-"
#line 1587 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 5638 "parser.cc"
    break;

  case 504: // varsym_no_minus: "VARSYM"
#line 1589 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5644 "parser.cc"
    break;

  case 505: // varsym_no_minus: special_sym
#line 1590 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5650 "parser.cc"
    break;

  case 506: // special_id: "as"
#line 1592 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 5656 "parser.cc"
    break;

  case 507: // special_id: "qualified"
#line 1593 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 5662 "parser.cc"
    break;

  case 508: // special_id: "hiding"
#line 1594 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 5668 "parser.cc"
    break;

  case 509: // special_id: "export"
#line 1595 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 5674 "parser.cc"
    break;

  case 510: // special_id: "label"
#line 1596 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 5680 "parser.cc"
    break;

  case 511: // special_id: "dynamic"
#line 1597 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 5686 "parser.cc"
    break;

  case 512: // special_id: "stdcall"
#line 1598 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 5692 "parser.cc"
    break;

  case 513: // special_id: "ccall"
#line 1599 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 5698 "parser.cc"
    break;

  case 514: // special_id: "capi"
#line 1600 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 5704 "parser.cc"
    break;

  case 515: // special_id: "prim"
#line 1601 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 5710 "parser.cc"
    break;

  case 516: // special_id: "javascript"
#line 1602 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 5716 "parser.cc"
    break;

  case 517: // special_id: "group"
#line 1603 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 5722 "parser.cc"
    break;

  case 518: // special_id: "stock"
#line 1604 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 5728 "parser.cc"
    break;

  case 519: // special_id: "anyclass"
#line 1605 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 5734 "parser.cc"
    break;

  case 520: // special_id: "via"
#line 1606 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 5740 "parser.cc"
    break;

  case 521: // special_id: "unit"
#line 1607 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 5746 "parser.cc"
    break;

  case 522: // special_id: "dependency"
#line 1608 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 5752 "parser.cc"
    break;

  case 523: // special_id: "signature"
#line 1609 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 5758 "parser.cc"
    break;

  case 524: // special_sym: "."
#line 1611 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 5764 "parser.cc"
    break;

  case 525: // special_sym: "*"
#line 1612 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 5770 "parser.cc"
    break;

  case 526: // qconid: conid
#line 1616 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5776 "parser.cc"
    break;

  case 527: // qconid: "QCONID"
#line 1617 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5782 "parser.cc"
    break;

  case 528: // conid: "CONID"
#line 1619 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5788 "parser.cc"
    break;

  case 529: // qconsym: consym
#line 1621 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5794 "parser.cc"
    break;

  case 530: // qconsym: "QCONSYM"
#line 1622 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5800 "parser.cc"
    break;

  case 531: // consym: "CONSYM"
#line 1624 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 5806 "parser.cc"
    break;

  case 532: // consym: ":"
#line 1625 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 5812 "parser.cc"
    break;

  case 533: // literal: "CHAR"
#line 1629 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::Char{yystack_[0].value.as < char32_t > ()});}
#line 5818 "parser.cc"
    break;

  case 534: // literal: "STRING"
#line 1630 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::String{yystack_[0].value.as < std::string > ()});}
#line 5824 "parser.cc"
    break;

  case 535: // literal: "INTEGER"
#line 1631 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::Integer{yystack_[0].value.as < integer > ()});}
#line 5830 "parser.cc"
    break;

  case 536: // literal: "RATIONAL"
#line 1632 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::Floating{yystack_[0].value.as < rational > ()});}
#line 5836 "parser.cc"
    break;

  case 537: // literal: "PRIMINTEGER"
#line 1633 "parser.y"
                     {yylhs.value.as < Hs::Exp > () = Hs::Literal(Hs::BoxedInteger{yystack_[0].value.as < integer > ()});}
#line 5842 "parser.cc"
    break;

  case 539: // close: error
#line 1641 "parser.y"
      {
          if (not drv.accept_layout_parse_error(yystack_[0].location))
              YYABORT;
          yyerrok;
      }
#line 5852 "parser.cc"
    break;

  case 540: // modid: "CONID"
#line 1649 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5858 "parser.cc"
    break;

  case 541: // modid: "QCONID"
#line 1650 "parser.y"
              {yylhs.value.as < Located<std::string> > () = {yylhs.location,yystack_[0].value.as < std::string > ()};}
#line 5864 "parser.cc"
    break;

  case 542: // commas: commas ","
#line 1652 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 5870 "parser.cc"
    break;

  case 543: // commas: ","
#line 1653 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 5876 "parser.cc"
    break;


#line 5880 "parser.cc"

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


  const short parser::yypact_ninf_ = -744;

  const short parser::yytable_ninf_ = -503;

  const short
  parser::yypact_[] =
  {
      34,   157,  -744,    64,  -744,  -744,  -744,  -744,  -744,   417,
      21,    60,  -744,    47,    -9,    -9,    88,  -744,  -744,  -744,
    -744,   267,  -744,  -744,  -744,    85,  -744,   225,   245,  1453,
     310,   361,   275,  -744,   872,  -744,   -53,  -744,  -744,  -744,
     157,  -744,   157,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,  1152,  -744,  -744,  -744,  -744,
    -744,   286,   128,  -744,   249,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,   322,  -744,   157,  -744,   292,  -744,  2727,  4735,
     391,   243,   362,   370,  2175,  -744,  -744,  -744,   406,   397,
    -744,  3739,   423,   370,  3417,  5143,   265,  3417,  3417,  3003,
    3417,  1899,  1761,   325,   339,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,    56,   339,   319,   275,  -744,  -744,  -744,  -744,
    -744,    66,   106,  -744,  -744,   862,  -744,  3141,  -744,   383,
    -744,  -744,  -744,  -744,  -744,  -744,   388,   121,  -744,  -744,
    -744,  -744,   336,  -744,  -744,   378,  -744,  -744,  -744,  -744,
     405,  -744,   416,   427,   430,  -744,  -744,  -744,  4837,  -744,
    4874,  -744,  -744,  -744,  -744,   361,  -744,  1761,   474,  1017,
    -744,  -744,  -744,  4735,  4735,  -744,  5242,  3929,  3531,   418,
    -744,   466,   468,  -744,   678,  -744,  4119,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,  4735,   453,  -744,  4207,  -744,  -744,
    -744,  4735,   543,   555,  2451,  2451,  -744,  2865,   492,   461,
      45,  -744,  -744,   505,   506,   509,   513,  4207,  1318,  1318,
    -744,   577,  4735,  4735,   179,   122,   511,   884,   123,   413,
    -744,  -744,   167,   -15,   484,   119,  -744,   142,  -744,  -744,
    -744,  -744,  3279,  -744,  3141,  -744,  -744,  -744,  5041,  -744,
    -744,  -744,  1017,     9,   485,   477,  -744,  2727,  -744,  -744,
    -744,  -744,  -744,  -744,  5374,  -744,  -744,   308,   140,   206,
     427,   482,   486,   489,   207,  -744,   352,     1,  5143,  -744,
    4207,  5143,  5143,  -744,   674,   292,   526,   464,  4735,  4207,
    5242,  2727,  2865,  5041,  -744,    31,  -744,  -744,  2727,  -744,
    -744,  -744,  -744,  4735,  -744,  5374,  5078,  3417,  -744,  -744,
    -744,  -744,  -744,  -744,  -744,   496,   498,   493,  -744,   508,
      47,   612,   409,  4207,  -744,  -744,   321,   149,   519,   499,
    -744,  -744,  -744,  -744,   517,   550,   541,  -744,  -744,   520,
    -744,  -744,  -744,  -744,  -744,  -744,   527,   522,   529,  -744,
     332,   353,  -744,   613,  4735,  4207,   810,  4735,  -744,  -744,
    -744,  4735,  -744,  -744,   566,  4207,  -744,  -744,  -744,  -744,
    4207,  4207,   397,   370,   567,   571,   112,  -744,  -744,    49,
     575,   538,  -744,   192,  -744,   642,  -744,  -744,  -744,  -744,
    -744,  -744,   641,   144,  -744,  -744,   862,    75,  2727,  -744,
     588,   338,   271,    42,  4207,   179,  4207,  -744,  -744,  -744,
     536,  -744,   597,   562,   248,   265,   599,  2727,  -744,   557,
     558,  2727,  2727,  2865,  2037,  -744,  2037,   262,  -744,  -744,
    5374,  -744,  -744,  2037,  -744,  2037,   154,  -744,  -744,  -744,
    -744,   545,   574,   608,   622,   621,   624,  5180,   586,  -744,
    -744,  -744,  -744,  -744,  4295,    94,   424,  -744,  -744,  -744,
    -744,   682,   628,   595,   397,  -744,  -744,  -744,  -744,  -744,
    -744,   615,  -744,   602,   643,   626,   629,  -744,  -744,  -744,
    4976,  -744,  -744,   157,    41,  2313,  1623,  -744,  -744,   616,
    4207,  -744,  5242,  5411,  -744,  4207,  4207,  -744,  -744,  4207,
    -744,  -744,  -744,   606,  -744,  5524,   415,  -744,  -744,  -744,
     611,   618,   593,  -744,  4207,  -744,  -744,   623,   625,  -744,
    -744,   577,  -744,  2727,  -744,  2451,  -744,  2727,  2865,  -744,
    2727,   422,  -744,  -744,  1318,  -744,  -744,  4207,  4207,  1559,
     656,  -744,  -744,  -744,    42,  -744,  -744,  -744,  -744,  -744,
    5242,  -744,  -744,   632,   455,   354,  -744,  -744,  -744,  -744,
    -744,  -744,  -744,  -744,   633,  -744,   665,  -744,  -744,  -744,
    -744,  -744,   636,  -744,  -744,  -744,  4207,  4207,   627,   631,
     674,  -744,   424,   664,  -744,  -744,   688,  4207,   748,   750,
     770,  -744,  2727,  -744,  -744,  -744,  5078,  2037,  5374,  -744,
    -744,   667,  1494,  -744,  -744,  -744,  2589,  -744,   680,   671,
    -744,   446,    47,  -744,  -744,  -744,  -744,  4207,  5598,  5598,
    -744,  -744,  -744,  -744,  -744,   670,  -744,  -744,  -744,  1036,
    1036,  -744,  -744,  -744,  -744,  -744,  4207,  -744,  -744,  -744,
    -744,   461,  1180,  1180,  -744,  -744,  -744,  -744,  -744,  5598,
     764,  -744,   712,  -744,  -744,  2865,  2727,  -744,  -744,   -22,
      12,  -744,  -744,  -744,  5279,   750,   770,  4735,  -744,  -744,
    -744,   709,  -744,  4735,   440,   770,   109,  -744,   770,  -744,
    -744,  -744,  -744,   -16,  -744,  1494,   157,  -744,   249,   683,
     175,  -744,  -744,  -744,  2727,  2727,  -744,    90,  -744,  -744,
     174,   715,  -744,  -744,  5598,   761,  5242,  4031,  -744,  -744,
     263,  -744,    76,  -744,   791,  -744,   784,  -744,   784,  -744,
     280,  -744,    98,  -744,   719,   448,  -744,  4207,  -744,  -744,
    -744,  4207,  -744,  4735,  4735,   770,  -744,  -744,  5448,   748,
     718,  3635,  -744,  -744,  -744,   290,   100,  -744,  4383,   266,
     752,  -744,  -744,  -744,  2037,  5374,   697,  -744,  -744,  -744,
    4939,  -744,  -744,   682,  -744,  4207,  -744,  4207,  -744,  4735,
     732,  4735,  4735,  -744,   442,  -744,  1036,  -744,  2727,  -744,
    4735,   526,  -744,  1180,  -744,  5598,  4471,  4559,  -744,  -744,
    -744,  -744,   723,   593,  -744,  -744,  -744,  4735,   691,  -744,
    4735,   711,   700,  -744,   265,    47,  -744,  -744,   701,   706,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,   717,   713,   566,
    4207,  -744,   476,  4207,  4647,  -744,  -744,  -744,  -744,  4295,
    -744,  5598,  -744,   720,   302,  -744,    47,   113,  4735,  3827,
    -744,  4735,  -744,   461,   156,  -744,  4735,  -744,  -744,  -744,
    -744,  -744,  5561,  -744,  -744,  3531,   740,   743,   424,  -744,
    -744,  -744,  4735,  -744,  -744,  -744,  -744,  4207,  -744,   715,
    5598,   750,   770,  -744,  -744,  -744,   770,  -744,  -744
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   540,   541,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    70,   539,   538,    12,   200,   196,     0,     0,    20,
       0,    46,    41,    15,    14,   199,     0,     6,     7,   506,
       0,   508,     0,   507,   494,   509,   510,   511,   492,   493,
     491,   495,   496,   512,   513,   514,   515,   516,   517,   518,
     519,   520,   521,   523,   522,     0,   489,   454,   488,   452,
      22,     0,    19,    24,    28,    36,   445,   453,    35,   483,
     487,   490,     0,    45,     0,    38,    42,   350,     0,     0,
     137,   139,     0,     0,     0,    63,    64,    65,   101,     0,
     138,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   313,   528,   527,   533,   534,   535,
     536,   537,   313,   313,    61,    68,    71,    72,    73,    74,
      75,   159,     0,    78,   296,    79,   319,   322,   328,   339,
     342,   344,   416,   429,   417,   206,   343,   487,   418,   526,
     345,   197,     0,    27,    26,     0,   503,   525,   524,   504,
       0,   501,     0,     0,     0,   502,   505,    17,     0,    21,
      31,    25,    40,    40,     3,    46,    37,     0,     0,   318,
     479,   480,   478,     0,     0,   234,   281,     0,     0,     0,
     476,   256,     0,   152,   217,   220,   221,   225,   232,   438,
     443,   233,   474,   477,     0,     0,   140,     0,   104,   102,
     103,     0,     0,     0,   399,   399,   337,     0,     0,   324,
       0,   335,   385,     0,     0,     0,     0,     0,   190,   190,
     193,     0,     0,     0,     0,     0,     0,   217,   438,     0,
     338,   327,     0,     0,     0,     0,   424,   201,   422,   420,
     391,   393,     0,   330,   321,   331,   532,   430,     0,   531,
     530,   353,   318,   358,     0,   359,   469,     0,   468,   472,
     500,   499,   433,   529,     0,   425,   543,     0,     0,     0,
     500,     0,   499,   433,     0,   427,     0,     0,     0,   314,
       0,     0,     0,    62,     0,    69,   159,     0,     0,     0,
       0,     0,     0,     0,   297,   195,   302,   467,     0,   466,
     470,   498,   497,     0,   325,     0,   406,     0,   198,   448,
     447,   446,   485,   484,    23,     0,     0,    32,    34,     0,
       0,    48,     0,     0,   236,   235,     0,     0,     0,   282,
     284,   481,   250,   442,     0,   209,     0,   213,   460,     0,
     461,   238,   459,   458,   456,   455,   247,     0,     0,   457,
       0,     0,   262,   176,     0,     0,     0,     0,   229,   449,
     230,     0,   226,   228,   143,   246,   242,   203,   106,   105,
       0,     0,     0,     0,   403,     0,     0,   398,   400,     0,
       0,   369,   371,     0,   323,     0,   384,   387,    98,    97,
      99,   100,   186,     0,   298,   189,     0,     0,     0,    94,
       0,   145,     0,   160,     0,     0,     0,    80,    81,    82,
       0,   308,     0,     0,     0,     0,     0,     0,   392,     0,
       0,   354,   360,     0,     0,   349,     0,   355,   352,   486,
       0,   348,   346,     0,   347,     0,   484,   419,   426,   542,
     428,     0,     0,     0,     0,     0,     0,     0,   305,   463,
      67,   462,   464,   431,     0,     0,   141,   304,   214,   204,
     205,   195,     0,     0,     0,   300,   301,   320,   326,   341,
     409,     0,   405,   408,   411,     0,   487,   329,    30,    29,
       0,     9,    10,     0,    50,     0,     0,   336,   317,     0,
       0,   237,     0,     0,   240,     0,     0,   441,   241,     0,
     444,   439,   440,   257,   259,     0,     0,    83,   151,   218,
       0,     0,   222,   227,     0,    89,   247,     0,   245,   107,
     108,   404,   401,     0,   394,   397,   395,     0,     0,   386,
       0,     0,    93,   191,   188,   192,   333,     0,     0,     0,
     109,   165,   164,    84,   161,   162,   263,    90,    91,    85,
       0,   309,   421,     0,     0,     0,   202,   435,   423,   306,
     332,   473,   434,   362,   364,   368,   353,   366,   365,   351,
     357,   356,     0,   315,   307,   312,     0,     0,     0,     0,
       0,   250,   141,     0,   156,   158,     0,     0,   278,   266,
     286,   299,     0,   471,   194,   340,     0,     0,     0,    33,
      47,     0,    54,    44,    49,   374,     0,   389,     0,   376,
     380,     0,     0,   375,   482,   285,   283,     0,     0,     0,
     249,   251,   254,   210,   212,   248,   262,   262,   261,   172,
     172,   175,   450,   475,   144,    76,     0,   402,   396,   388,
     370,   324,   182,   182,   185,   187,   124,   146,   147,     0,
     114,   163,     0,   436,   437,     0,   361,   316,   207,     0,
       0,   465,   432,    66,     0,   266,   286,     0,   157,   142,
     250,   272,   274,     0,     0,   286,     0,    86,   287,   289,
     303,   407,   410,   413,   415,    54,     0,    56,    28,     0,
      53,    58,   377,   372,   379,     0,   381,   195,   373,   211,
       0,     0,   239,   258,   260,   130,     0,     0,   166,   171,
       0,   168,     0,   248,     0,   137,   132,   177,   132,   181,
       0,   178,     0,   110,     0,     0,    88,     0,   367,   363,
     310,     0,   311,     0,     0,   286,    95,   155,     0,   278,
       0,   279,   223,   231,   276,   324,   324,    87,     0,     0,
     290,   293,   451,   288,     0,     0,     0,    60,    59,    51,
       0,    55,   378,   195,   383,     0,   252,     0,   131,     0,
       0,     0,     0,   128,   148,   173,   170,   174,     0,   133,
       0,   159,   183,   180,   184,     0,   123,   123,   115,    77,
     208,   154,     0,   215,    96,   277,   273,     0,     0,   224,
       0,     0,   267,   270,     0,     0,   294,   219,   243,     0,
     291,   292,   412,   414,    52,    57,   382,     0,     0,   143,
       0,   129,   148,     0,     0,   126,   169,   334,   134,     0,
     179,   111,   113,     0,     0,   122,     0,     0,     0,   279,
     275,   280,   264,   324,     0,   265,     0,   295,   253,   255,
     125,   167,     0,   127,   149,     0,     0,   233,   141,   112,
     118,   116,   121,   119,   117,   153,   269,     0,   244,   233,
       0,   266,   286,   120,   271,   150,   286,   135,   136
  };

  const short
  parser::yypgoto_[] =
  {
    -744,  -744,  -744,  -744,  -744,  -744,  -744,    39,  -744,  -744,
    -744,  -744,   663,   134,  -744,  -744,   -12,   710,  -744,  -744,
    -744,  -744,   661,  -744,  -744,  -744,   143,  -744,    67,  -744,
    -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,  -744,
    -744,  -744,   153,    86,  -744,  -744,   -33,  -744,  -744,  -744,
      54,  -217,  -744,  -744,   127,  -744,   822,  -744,  -563,    29,
    -744,    27,   564,    24,  -292,  -744,  -744,   306,    83,   230,
    -744,  -744,    80,   221,  -744,  -744,   654,  -744,  -293,  -440,
     871,  -744,  -744,  -298,  -678,  -150,   300,  -160,   324,  -744,
     -88,  -744,   -89,  -744,   -99,  -744,  -314,  -744,  -744,  -744,
    -700,  -122,  -188,    37,  -744,   524,  -519,   346,  -743,  -744,
    -744,   261,   276,  -427,  -641,   161,  -744,    65,  -555,  -744,
     171,  -744,   117,  -744,  -744,   429,  -603,  -744,   237,   173,
     899,  -203,  -744,  -744,   635,  -744,   449,  -744,    30,   -31,
    -267,  -200,   828,    70,  -744,  -744,  -744,   -90,  -744,  -744,
    -744,  -744,   639,  -744,  -744,  -454,  -744,   234,  -744,  -198,
    -744,  -204,  -744,  -744,   693,  -744,   -65,   728,   411,  -174,
    -744,   342,  -744,  -744,  -744,  -744,   525,   138,  -744,   -96,
    -699,   -66,  -744,   530,   -61,  -744,  -744,  -744,   -23,  -744,
    -183,  -744,   363,  -744,   695,  -744,  -744,  -744,  -344,  -744,
    -334,  -282,    18,  -250,  -163,    -4,  -744,  -744,   375,   -42,
     -98,    -1,  -744,  -162,   -97,   -52,  -223,  -744,   -72,   -35,
    -105
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   174,     6,    10,    19,    30,
      71,    72,    73,   171,   326,   327,   698,    86,    11,    20,
      21,    32,    84,   494,   613,   614,   699,   700,   701,   294,
     124,   458,    33,    34,   125,   420,   126,   127,   128,   235,
     129,   227,   212,   213,   130,   660,   733,   841,   736,   798,
     844,   845,   718,   779,   790,   727,   728,   205,   599,   525,
     550,   835,   191,   592,   298,   553,   554,   555,   719,   720,
     641,   517,   729,   730,   654,   542,   403,   230,   231,   475,
      27,    36,   426,   376,   467,   132,   669,   356,   377,   469,
     346,   750,   347,   818,   194,   195,   751,   196,   372,   367,
     752,   197,   378,   819,   527,   357,   503,   630,   631,   363,
     513,   514,   515,   557,   685,   811,   812,   813,   600,   681,
     682,   683,   754,   338,   339,   340,   687,   688,   689,   760,
     404,   721,   304,   305,   306,   134,   288,   289,   261,   179,
     136,   814,   137,   138,   139,   140,   277,   278,   279,   264,
     265,   574,   390,   391,   497,   618,   619,   620,   706,   220,
     221,   222,   621,   385,   251,   252,   216,   386,   387,   388,
     481,   482,   483,   693,   141,   142,   246,   247,   143,   144,
     459,   266,   566,   198,   199,    75,   368,   761,   200,    77,
     358,   359,   460,   461,   308,   267,   309,   268,   201,   370,
     202,   145,   146,   485,    79,    80,   310,   269,   270,   312,
     165,   203,   166,   148,   149,   272,   273,   150,    24,     9,
     284
  };

  const short
  parser::yytable_[] =
  {
     193,   192,   237,   135,   464,   153,    76,   154,   286,   249,
     248,   369,   236,   271,   282,   409,   396,    74,   470,   395,
     393,   263,   423,   164,   438,   405,   405,   344,    81,   675,
     147,   601,   521,    81,   745,   498,   337,   676,   240,   402,
     238,   477,   622,   392,    13,   551,    22,    78,    22,   175,
      22,   809,   810,   611,   369,     1,   740,   151,   474,   799,
     283,   334,   335,   421,    12,   479,   764,   152,   206,   307,
     281,   463,   674,   746,   373,   290,    22,    22,   633,   282,
     262,   262,   757,   360,   361,   296,   765,    81,   432,   531,
     742,   866,   451,    81,   433,   429,   430,   644,   741,    22,
      25,  -268,   454,    81,    81,   237,    81,    81,    81,    81,
      81,    81,   237,   307,    22,   374,   302,   474,   178,   866,
     422,   657,   379,   243,   219,   283,    26,    17,   392,   434,
     217,   208,   741,   237,   237,   164,    81,   452,   297,     2,
     473,   430,   804,   410,   411,    76,   262,    76,   397,   809,
     810,   809,   861,    23,   612,    23,    74,    23,   328,   632,
     209,   748,   702,   210,   477,   287,   552,    81,   535,    81,
     679,   638,   595,   413,   241,   217,    81,   250,   253,    18,
     255,   604,   341,    23,    23,    81,    78,   299,    78,   396,
     579,   478,   529,   530,   544,   786,   307,   406,   406,   256,
     164,    29,  -481,   414,   -92,   632,    23,   314,  -268,   193,
     192,   329,   330,    81,    81,   526,    81,   793,   534,   394,
      35,    23,   758,   424,   147,   147,   300,    81,    81,   596,
     500,   535,   872,   275,    67,  -482,   437,   877,    69,   276,
     886,  -481,   415,   -92,   384,   384,   259,   384,   168,   523,
     543,    81,   462,    81,   442,   775,   559,    81,   492,   575,
     443,   156,   425,   544,   157,   237,    81,   774,   522,   300,
     439,   158,   169,    81,  -482,   518,   425,   217,   662,   887,
     776,    31,     7,   888,   710,   711,     8,    81,   668,   668,
      81,    81,   412,   159,   499,   770,   341,   161,   539,    81,
      81,    81,    81,    37,    67,   881,   453,    81,    69,   455,
     456,   439,   486,   882,    81,    81,    81,   536,   532,   771,
     444,   448,   250,    38,   314,   734,   445,   449,   249,   248,
     632,   471,   384,   826,   484,   545,   271,    82,   271,   369,
     307,   655,   256,   520,   577,   271,   578,   271,   827,   635,
     828,   348,   337,   580,   558,   581,   156,   563,   694,   157,
     589,   564,   170,   565,   650,   350,   158,   463,    67,   785,
     638,   307,    69,    67,   244,   594,   593,    69,   245,   758,
     113,   207,   786,   303,   208,    83,   792,   487,   159,   259,
     115,    67,   161,   260,    85,    69,  -268,   352,   353,   793,
     167,   354,   355,   262,   632,   262,   864,    81,   871,   394,
     440,   176,   262,   209,   262,   156,   210,   211,   157,   548,
     549,   872,   441,   707,   623,   158,    81,   172,   204,   173,
      81,    81,    81,    81,   780,    81,   439,   239,   546,    81,
     163,   285,    81,   800,    81,   276,   511,   159,   287,   731,
     731,   724,   449,   588,   417,   293,    81,   570,   610,   360,
     361,   842,   573,   384,   576,   418,   419,    76,   450,   512,
     664,   318,   449,   449,   276,   214,   317,   215,   609,   223,
     224,   225,   226,    14,    15,   315,   723,   280,   316,    81,
     867,   738,   319,   849,    81,    81,   851,   332,   341,   839,
     783,    81,   228,   396,   229,   597,   598,   869,    78,   271,
     311,   345,   345,   406,   495,   823,   496,   692,   632,   320,
     639,   879,   640,   833,   834,   617,   617,   652,   463,   653,
     321,   217,    81,   705,    81,   348,    81,    81,   276,    81,
     147,   322,   349,    81,   323,   755,   734,   756,   462,   350,
     708,   362,   280,   796,   311,   797,   341,   833,   862,    81,
     364,   753,   380,   647,   831,   384,   375,   649,   384,   663,
     651,   291,   292,   838,   381,   276,   262,   217,   237,   884,
     394,   352,   353,   398,   399,   354,   355,   400,   747,    76,
     731,   401,   408,   416,   257,   435,   446,   436,   297,   465,
    -502,    81,   486,   447,   439,    81,    81,    81,   406,   406,
     488,    81,   489,   490,   491,    81,   493,   163,   237,   502,
     369,   406,   406,   468,   484,   501,   463,   504,   784,   753,
      78,   505,   690,   506,   507,   147,   147,   311,    81,    81,
     516,   508,   509,   510,   237,   803,   617,   524,   147,   147,
     787,    81,    81,  -390,   801,   883,   802,   533,   538,   237,
     794,   767,   537,   762,    81,    81,   271,   540,   541,   817,
     547,   560,    76,   348,   822,   561,   562,   569,   571,   572,
     237,   582,   237,   237,   583,   753,   584,   350,   753,   519,
     829,   237,   410,   832,    81,   384,   739,   237,   237,   345,
     585,   410,   586,    81,    81,   587,   590,   410,   410,   474,
     602,   589,   341,    78,   366,    81,   603,   249,   248,   352,
     353,   605,   606,   354,   355,   607,   636,   753,   608,   753,
     624,  -486,   642,   262,   617,   773,   762,   645,   556,   643,
     345,   659,   343,   855,   666,   646,   667,    76,   671,   237,
     594,   593,   672,   665,   256,   406,   677,   237,   348,   875,
     360,   439,   406,    81,    81,   365,   678,   817,   156,    81,
    -215,   157,   350,   237,   873,   874,   680,   684,   158,   686,
     695,   311,   147,   410,   712,    81,   703,    81,    78,   147,
     704,   735,    81,   737,   749,   457,   777,   769,   778,   366,
     159,   259,   788,   789,   352,   353,   795,   211,   354,   355,
     807,   824,   311,   830,    39,   848,   115,   852,   837,   853,
     857,   856,    41,   858,   625,   880,   870,   859,  -254,   556,
     634,   324,   768,   345,    43,   295,   331,   825,   766,   759,
      45,    46,    47,   180,   181,   182,   821,   885,   556,    53,
      54,   847,    55,    56,    57,   791,   131,    58,   860,   863,
     661,    59,   466,   868,    60,    61,    62,    63,    64,   836,
     722,   656,   556,   840,   732,    87,    39,    88,    89,    90,
      91,    92,    93,   407,    41,    94,    28,   670,    95,    96,
      97,    98,    99,   878,   100,   658,    43,   713,   101,   528,
      44,   102,    45,    46,    47,    48,    49,    50,   103,    51,
      52,    53,    54,   714,    55,    56,    57,   815,   876,    58,
     806,   556,   104,    59,   850,   763,    60,    61,    62,    63,
      64,   626,   820,   133,   190,    67,   105,   254,   772,    69,
     476,   472,   256,   389,   301,   428,   648,   302,   691,   106,
     568,   709,   854,   673,   567,   107,   156,   431,     0,   157,
       0,     0,   108,     0,   348,   109,   158,   110,     0,     0,
     345,   365,     0,     0,     0,     0,     0,     0,   350,     0,
       0,   111,     0,   303,     0,   112,     0,   113,   159,   259,
       0,     0,   161,   260,     0,   114,    66,   115,     0,     0,
      68,   116,     0,     0,     0,   366,   117,   118,   119,   120,
     352,   353,   121,     0,   354,   355,     0,   122,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    87,
      39,    88,     0,   715,   716,     0,    93,     0,    41,    94,
       0,     0,    95,    96,    97,     0,    99,     0,     0,     0,
      43,   468,   717,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,   104,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,   256,   333,   556,
     105,   556,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   156,     0,   106,   157,     0,     0,     0,     0,   107,
       0,   158,     0,     0,     0,     0,   108,     0,     0,   109,
       0,   110,     0,     0,     0,     0,     0,     0,   303,     0,
       0,     0,     0,   159,   259,   111,     0,   161,   260,   112,
       0,   113,     0,     0,   468,     0,     0,   556,   656,   114,
      66,   115,     0,     0,    68,   116,     0,     0,     0,     0,
     117,   118,   119,   120,     0,     0,   121,     0,     0,     0,
       0,   122,   123,    87,    39,    88,     0,   725,     0,   345,
      93,     0,    41,    94,     0,     0,    95,    96,    97,     0,
      99,     0,   100,     0,    43,     0,   726,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   103,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
     104,    59,   155,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,     0,   156,     0,     0,   157,
       0,     0,     0,     0,     0,     0,   158,   106,     0,     0,
       0,     0,     0,   107,     0,     0,     0,     0,     0,     0,
     108,     0,     0,   109,     0,   110,     0,     0,   159,   160,
       0,     0,   161,   162,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   112,     0,   113,     0,     0,     0,     0,
       0,     0,     0,   114,    66,   115,     0,     0,    68,   116,
       0,     0,     0,     0,   117,   118,   119,   120,     0,     0,
     121,    87,    39,    88,     0,   122,   123,     0,    93,     0,
      41,    94,     0,     0,    95,    96,    97,     0,    99,     0,
       0,     0,    43,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   104,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   106,     0,     0,     0,     0,
       0,   107,     0,     0,     0,     0,     0,     0,   108,     0,
       0,   109,     0,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,   112,     0,   113,     0,     0,     0,     0,     0,     0,
       0,   114,    66,   115,     0,     0,    68,   116,     0,     0,
       0,     0,   117,   118,   119,   120,     0,    39,   121,     0,
       0,    40,     0,   122,   123,    41,     0,     0,     0,     0,
       0,     0,     0,     0,    42,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,     0,    55,    56,    57,    39,     0,
      58,     0,     0,     0,    59,     0,    41,    60,    61,    62,
      63,    64,     0,     0,     0,   696,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,    65,     0,     0,     0,
       0,    41,     0,     0,     0,     0,     0,    66,    67,     0,
       0,    68,    69,    43,     0,     0,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,    70,    53,    54,
       0,    55,    56,    57,     0,     0,    58,    65,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,    66,    67,
       0,     0,    68,    69,    22,     0,    87,    39,    88,     0,
       0,     0,     0,    93,     0,    41,    94,     0,   697,     0,
       0,     0,     0,    99,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,   628,    55,    56,    57,     0,     0,
      58,     0,   629,   104,    59,     0,     0,    60,    61,    62,
      63,    64,     0,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    23,   111,     0,     0,     0,   177,     0,   113,     0,
       0,     0,   616,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,     0,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,     0,     0,
       0,    93,     0,    41,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,   104,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   256,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,   156,   110,     0,   157,     0,
       0,     0,     0,     0,   274,   158,     0,     0,     0,     0,
     111,     0,     0,     0,   177,   275,   113,     0,     0,     0,
       0,   276,   258,     0,     0,    66,   115,   159,   259,    68,
     116,   161,   260,     0,     0,   117,   118,   119,   120,     0,
       0,   121,    87,    39,    88,     0,     0,     0,     0,    93,
       0,    41,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   104,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   256,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,   157,     0,     0,     0,
       0,     0,     0,   158,     0,     0,     0,     0,   111,   257,
       0,     0,   177,     0,   113,     0,     0,     0,     0,     0,
     258,     0,     0,    66,   115,   159,   259,    68,   116,   161,
     260,     0,     0,   117,   118,   119,   120,     0,     0,   121,
      87,    39,    88,     0,     0,     0,     0,    93,     0,    41,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   256,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,   157,     0,     0,     0,     0,     0,
       0,   158,     0,     0,     0,     0,   111,     0,     0,     0,
     177,     0,   113,     0,     0,     0,     0,     0,   258,     0,
       0,    66,   115,   159,   259,    68,   116,   161,   260,     0,
       0,   117,   118,   119,   120,     0,     0,   121,    87,    39,
      88,     0,     0,     0,     0,    93,     0,    41,    94,     0,
       0,     0,     0,     0,     0,    99,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,   104,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
     217,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     218,     0,     0,     0,   111,     0,     0,     0,   177,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,     0,   121,    87,    39,    88,     0,
       0,     0,     0,    93,     0,    41,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   104,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   109,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   615,
       0,     0,   111,     0,     0,     0,   177,     0,   113,     0,
       0,     0,   616,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,     0,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,     0,     0,
       0,    93,     0,    41,    94,     0,     0,     0,     0,     0,
       0,   382,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,   383,    58,     0,
       0,   104,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
       0,   108,     0,     0,   109,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   177,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,     0,     0,     0,   117,   118,   119,   120,     0,
       0,   121,    87,    39,    88,     0,     0,     0,     0,    93,
       0,    41,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,   104,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   108,
       0,     0,   109,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   177,     0,   113,     0,     0,     0,   616,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
       0,     0,     0,   117,   118,   119,   120,     0,     0,   121,
      87,    39,    88,     0,     0,     0,     0,    93,     0,    41,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,   104,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     177,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,     0,     0,   121,    87,    39,
      88,     0,     0,     0,     0,    93,     0,    41,    94,     0,
       0,     0,     0,     0,     0,   382,     0,     0,     0,    43,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,   104,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   107,     0,
       0,     0,     0,     0,     0,   108,     0,     0,   109,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   111,     0,     0,     0,   177,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,     0,   121,    87,    39,    88,     0,
       0,     0,     0,    93,     0,    41,    94,     0,     0,     0,
       0,     0,     0,    99,     0,     0,     0,    43,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,     0,    55,    56,    57,     0,     0,
      58,     0,     0,   104,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   111,     0,     0,     0,   177,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,     0,     0,     0,   117,   118,   119,
     120,     0,     0,   121,    87,    39,    88,     0,     0,     0,
       0,    93,     0,    41,    94,     0,     0,     0,     0,     0,
       0,    99,     0,     0,     0,    43,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,     0,     0,     0,     0,     0,
     313,   108,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     111,     0,     0,     0,   177,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,     0,     0,     0,   117,   118,   119,   120,     0,
       0,   121,    87,    39,    88,     0,     0,     0,     0,    93,
       0,    41,    94,     0,     0,     0,     0,     0,     0,    99,
       0,     0,     0,    43,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   107,     0,     0,     0,   427,     0,     0,   108,
       0,     0,     0,     0,   110,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   111,     0,
       0,     0,   177,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
       0,     0,     0,   117,   118,   119,   120,     0,     0,   121,
      87,    39,    88,     0,     0,     0,     0,    93,     0,    41,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    43,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,     0,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     107,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   111,     0,     0,     0,
     177,     0,   113,     0,     0,    39,     0,     0,     0,     0,
       0,    66,   115,    41,     0,    68,   116,     0,     0,     0,
       0,   117,   118,   119,   120,    43,     0,   121,     0,   342,
       0,    45,    46,    47,   180,   181,   182,     0,     0,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   348,     0,     0,     0,     0,     0,     0,   349,     0,
       0,   183,     0,     0,     0,   350,   184,     0,   185,     0,
       0,     0,     0,     0,     0,     0,   186,     0,     0,    39,
     187,     0,     0,     0,   188,   351,   189,    41,     0,     0,
       0,   276,     0,     0,     0,   190,    67,   352,   353,    43,
      69,   354,   355,     0,     0,    45,    46,    47,   180,   181,
     182,     0,     0,     0,    53,    54,     0,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   256,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,  -216,     0,     0,
     184,     0,   185,     0,     0,     0,     0,     0,     0,     0,
     186,     0,     0,    39,   187,     0,     0,     0,   188,     0,
     189,    41,     0,     0,     0,     0,   808,     0,   232,   190,
      67,     0,   259,    43,    69,     0,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,   233,   234,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,    39,     0,     0,   184,     0,   185,     0,     0,    41,
       0,     0,     0,     0,   186,     0,     0,     0,   187,     0,
       0,    43,   188,     0,   189,     0,     0,    45,    46,    47,
     180,   181,   182,   190,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   256,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,     0,
       0,     0,   184,     0,   185,     0,     0,     0,     0,     0,
       0,     0,   186,    39,     0,     0,   187,     0,     0,     0,
     188,    41,   189,     0,     0,     0,     0,     0,   808,     0,
       0,   190,    67,    43,   259,     0,    69,   342,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,     0,     0,     0,   184,     0,   185,     0,     0,     0,
       0,     0,     0,     0,   186,    39,     0,     0,   187,   343,
       0,     0,   188,    41,   189,     0,     0,     0,     0,     0,
     781,     0,     0,   190,    67,    43,     0,     0,    69,     0,
       0,    45,    46,    47,   180,   181,   182,     0,   782,     0,
      53,    54,     0,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,    39,     0,     0,   184,     0,   185,     0,
       0,    41,     0,     0,     0,     0,   186,     0,     0,     0,
     187,     0,     0,    43,   188,     0,   189,     0,     0,    45,
      46,    47,   180,   181,   182,   190,    67,     0,    53,    54,
      69,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,   183,
       0,    39,     0,     0,   184,     0,   185,     0,     0,    41,
       0,     0,     0,     0,   186,     0,     0,     0,   187,     0,
       0,    43,   188,     0,   189,   342,     0,    45,    46,    47,
     180,   181,   182,   190,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,    39,
       0,     0,   184,     0,   185,     0,     0,    41,     0,     0,
       0,     0,   186,     0,     0,     0,   187,     0,     0,    43,
     188,     0,   189,   591,     0,    45,    46,    47,   180,   181,
     182,   190,    67,     0,    53,    54,    69,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,    39,     0,     0,
     184,     0,   185,     0,     0,    41,     0,     0,     0,     0,
     186,     0,     0,     0,   187,     0,     0,    43,   188,     0,
     189,     0,     0,    45,    46,    47,   180,   181,   182,   190,
      67,     0,    53,    54,    69,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,     0,     0,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,     0,    39,     0,     0,   184,     0,
     185,     0,     0,    41,     0,     0,     0,     0,   186,     0,
       0,     0,   187,     0,     0,    43,   188,   816,   189,     0,
       0,    45,    46,    47,   180,   181,   182,   190,    67,     0,
      53,    54,    69,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     843,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,    39,     0,     0,   184,     0,   185,     0,
       0,    41,     0,     0,     0,     0,   186,     0,     0,     0,
     187,     0,     0,    43,   188,     0,   189,     0,     0,    45,
      46,    47,   180,   181,   182,   190,    67,     0,    53,    54,
      69,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   846,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
       0,    39,     0,     0,   184,     0,   185,     0,     0,    41,
       0,     0,     0,     0,   186,     0,     0,     0,   187,     0,
       0,    43,   188,     0,   189,   342,     0,    45,    46,    47,
     180,   181,   182,   190,    67,     0,    53,    54,    69,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,    39,
       0,     0,   184,     0,   185,     0,     0,    41,     0,     0,
       0,     0,   186,     0,     0,     0,   187,     0,     0,    43,
     865,     0,   189,     0,     0,    45,    46,    47,   180,   181,
     182,   190,    67,     0,    53,    54,    69,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    59,     0,     0,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,     0,     0,     0,     0,
     184,     0,   185,     0,     0,     0,     0,     0,     0,     0,
     186,    39,     0,     0,   187,    40,     0,     0,   188,    41,
     189,     0,     0,     0,     0,     0,     0,     0,    42,   190,
      67,    43,     0,     0,    69,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    39,    55,
      56,    57,     0,     0,    58,     0,    41,     0,    59,     0,
       0,    60,    61,    62,    63,    64,     0,     0,    43,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,     0,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
      65,    41,     0,   325,     0,     0,     0,     0,     0,     0,
     696,    66,    67,    43,     0,    68,    69,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      39,    55,    56,    57,     0,     0,    58,    65,    41,     0,
      59,     0,     0,    60,    61,    62,    63,    64,    66,    67,
      43,     0,    68,    69,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,     0,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,     0,     0,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,    65,    41,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    66,    67,    43,     0,    68,    69,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    39,    55,    56,    57,     0,     0,    58,    65,
      41,     0,    59,     0,     0,    60,    61,    62,    63,    64,
      66,    67,    43,     0,    68,    69,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    41,     0,   480,     0,     0,
       0,     0,     0,     0,     0,    66,   115,    43,     0,    68,
     116,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    39,    55,    56,    57,     0,     0,
      58,   242,    41,     0,    59,     0,     0,    60,    61,    62,
      63,    64,    66,     0,    43,     0,    68,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,     0,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,     0,     0,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,     0,    41,     0,   242,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    43,    66,     0,     0,
      44,    68,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    39,    55,    56,    57,     0,     0,    58,
       0,    41,     0,    59,     0,     0,    60,    61,    62,    63,
      64,     0,     0,    43,    66,   115,     0,     0,     0,    45,
      46,    47,   180,   181,   182,     0,     0,     0,    53,    54,
       0,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,     0,     0,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   336,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    66,     0,     0,     0,
       0,   743,     0,     0,     0,     0,     0,     0,    39,     0,
       0,     0,     0,   744,   628,     0,    41,     0,     0,     0,
       0,     0,   629,     0,     0,     0,     0,     0,    43,     0,
       0,     0,    44,   190,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    39,    55,    56,    57,     0,
       0,    58,     0,    41,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,    43,     0,     0,     0,     0,
       0,    45,    46,    47,   180,   181,   182,     0,     0,     0,
      53,    54,    39,    55,    56,    57,     0,     0,    58,     0,
      41,     0,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,    43,     0,     0,     0,     0,     0,    45,    46,
      47,   180,   181,   182,     0,     0,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,    66,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,   627,   628,     0,     0,     0,
       0,     0,     0,     0,   629,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,   190,    41,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    43,     0,
       0,     0,   805,   628,    45,    46,    47,   180,   181,   182,
       0,   629,     0,    53,    54,    39,    55,    56,    57,     0,
       0,    58,   190,    41,     0,    59,     0,     0,    60,    61,
      62,    63,    64,     0,     0,    43,     0,     0,     0,     0,
       0,    45,    46,    47,   180,   181,   182,     0,     0,     0,
      53,    54,    39,    55,    56,    57,     0,     0,    58,     0,
      41,   637,    59,     0,     0,    60,    61,    62,    63,    64,
       0,     0,    43,     0,     0,     0,     0,     0,    45,    46,
      47,   180,   181,   182,     0,     0,     0,    53,    54,     0,
      55,    56,    57,     0,     0,    58,     0,     0,   190,    59,
       0,     0,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   629,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190
  };

  const short
  parser::yycheck_[] =
  {
      89,    89,   101,    34,   296,    40,    29,    42,   113,   106,
     106,   194,   101,   111,   112,   232,   220,    29,   300,   219,
     218,   111,   245,    65,   274,   228,   229,   187,    29,   592,
      34,   471,   366,    34,   675,   333,   186,   592,   103,   227,
     101,   308,   496,   217,     5,     3,     1,    29,     1,    84,
       1,   751,   751,    12,   237,    21,    78,   110,    27,   737,
     112,   183,   184,    78,     0,   315,    82,   120,    91,   135,
     112,   294,   591,   676,   196,    19,     1,     1,   505,   177,
     111,   112,   685,   188,   189,    19,   102,    88,    79,   382,
      78,   834,    91,    94,    85,   258,   258,   524,   120,     1,
     109,     1,   290,   104,   105,   204,   107,   108,   109,   110,
     111,   112,   211,   179,     1,   204,    85,    27,    88,   862,
     135,   548,   211,   105,    94,   177,   135,   106,   302,   120,
      85,    22,   120,   232,   233,   177,   137,   136,    72,   105,
     303,   303,   745,   232,   233,   168,   177,   170,   220,   849,
     849,   851,   830,   108,   113,   108,   168,   108,   170,   503,
      51,   680,   616,    54,   431,   109,   124,   168,   119,   170,
     597,   515,    78,   234,   104,    85,   177,   107,   108,   119,
     110,   474,   186,   108,   108,   186,   168,    81,   170,   393,
     440,   313,   380,   381,   119,   119,   262,   228,   229,    80,
     242,   113,    81,    81,    81,   549,   108,   137,   108,   298,
     298,   172,   173,   214,   215,   375,   217,   119,   106,   119,
     135,   108,   113,    81,   228,   229,   120,   228,   229,   135,
      81,   119,   119,   114,   125,    81,   267,    81,   129,   120,
     881,   120,   120,   120,   214,   215,   127,   217,   120,   371,
     106,   252,   294,   254,   114,    81,   416,   258,   330,   433,
     120,    94,   120,   119,    97,   364,   267,   707,   367,   120,
     274,   104,   144,   274,   120,   364,   120,    85,   560,   882,
     106,    14,   125,   886,   628,   629,   129,   288,   586,   587,
     291,   292,   113,   126,   336,   120,   300,   130,   106,   300,
     301,   302,   303,    78,   125,   868,   288,   308,   129,   291,
     292,   315,   316,   868,   315,   316,   317,   389,   383,   144,
     114,   114,   252,    78,   254,   659,   120,   120,   425,   425,
     674,   301,   302,   773,   316,   407,   434,    27,   436,   522,
     406,   544,    80,   366,   434,   443,   436,   445,   775,   509,
     777,    80,   502,   443,   415,   445,    94,   109,   608,    97,
     457,   113,   113,   115,   538,    94,   104,   590,   125,   106,
     714,   437,   129,   125,   109,   464,   464,   129,   113,   113,
     115,    19,   119,   121,    22,    24,   106,   317,   126,   127,
     125,   125,   130,   131,   119,   129,   106,   126,   127,   119,
     114,   130,   131,   434,   748,   436,   833,   408,   106,   119,
     102,   119,   443,    51,   445,    94,    54,    55,    97,    81,
      82,   119,   114,   621,   496,   104,   427,   105,    37,   107,
     431,   432,   433,   434,   716,   436,   440,    14,   408,   440,
      65,   116,   443,   741,   445,   120,   114,   126,   109,   652,
     653,   651,   120,   457,    41,   136,   457,   427,   493,   564,
     565,   795,   432,   433,   434,    52,    53,   490,   116,   116,
     116,   135,   120,   120,   120,   105,    88,   107,   490,    73,
      74,    75,    76,    66,    67,   102,   646,   112,   105,   490,
     834,   665,   114,   807,   495,   496,   810,    23,   502,   791,
     717,   502,   105,   707,   107,    81,    82,   841,   490,   607,
     135,   187,   188,   544,   105,   765,   107,   607,   862,   114,
     105,   865,   107,    81,    82,   495,   496,   105,   751,   107,
     114,    85,   533,    87,   535,    80,   537,   538,   120,   540,
     544,   114,    87,   544,   114,   105,   880,   107,   590,    94,
     622,    85,   177,   105,   179,   107,   560,    81,    82,   560,
      92,   683,    19,   533,   781,   535,   113,   537,   538,   114,
     540,   122,   123,   790,    19,   120,   607,    85,   677,   877,
     119,   126,   127,    78,    78,   130,   131,    78,   677,   612,
     793,    78,    15,    82,   110,   110,   114,   120,    72,   135,
     114,   602,   606,   114,   608,   606,   607,   608,   639,   640,
     114,   612,   114,   120,   106,   616,     4,   242,   717,   120,
     803,   652,   653,   299,   606,   106,   849,   110,   717,   751,
     612,    81,   602,    92,   114,   639,   640,   262,   639,   640,
      27,   114,   120,   114,   743,   744,   616,    81,   652,   653,
     722,   652,   653,    86,   743,   872,   744,    86,   120,   758,
     732,   696,    87,   686,   665,   666,   764,    25,    27,   758,
      82,   135,   695,    80,   764,    78,   114,    78,   121,   121,
     779,   136,   781,   782,   110,   807,    78,    94,   810,   365,
     779,   790,   781,   782,   695,   665,   666,   796,   797,   375,
      78,   790,    81,   704,   705,    81,   120,   796,   797,    27,
      82,   808,   716,   695,   121,   716,   121,   814,   814,   126,
     127,   106,   120,   130,   131,    82,   120,   849,   102,   851,
     114,   102,   121,   764,   704,   705,   759,   114,   414,   121,
     416,    85,   110,   815,    79,   120,   110,   770,   121,   848,
     839,   839,   121,   120,    80,   786,    92,   856,    80,   848,
     865,   765,   793,   764,   765,    87,    78,   856,    94,   770,
      92,    97,    94,   872,   846,   847,    28,    27,   104,     9,
     113,   406,   786,   872,   114,   786,   106,   788,   770,   793,
     119,    27,   793,    81,    85,   121,    81,   114,    37,   121,
     126,   127,    11,    19,   126,   127,    87,    55,   130,   131,
      92,   114,   437,    81,     4,    92,   125,   106,   788,   119,
     114,   120,    12,   106,   500,    85,   106,   114,    85,   505,
     506,   168,   698,   509,    24,   125,   175,   770,   695,   686,
      30,    31,    32,    33,    34,    35,   760,   880,   524,    39,
      40,   797,    42,    43,    44,   728,    34,    47,   829,   832,
     554,    51,   298,   839,    54,    55,    56,    57,    58,   786,
     640,   547,   548,   793,   653,     3,     4,     5,     6,     7,
       8,     9,    10,   229,    12,    13,    15,   587,    16,    17,
      18,    19,    20,   856,    22,   549,    24,   636,    26,   375,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,   637,    42,    43,    44,   756,   853,    47,
     749,   597,    50,    51,   807,   688,    54,    55,    56,    57,
      58,   502,   759,    34,   124,   125,    64,   109,   704,   129,
     305,   302,    80,   215,    82,   252,   535,    85,   606,    77,
     425,   627,   814,   590,   424,    83,    94,   262,    -1,    97,
      -1,    -1,    90,    -1,    80,    93,   104,    95,    -1,    -1,
     646,    87,    -1,    -1,    -1,    -1,    -1,    -1,    94,    -1,
      -1,   109,    -1,   121,    -1,   113,    -1,   115,   126,   127,
      -1,    -1,   130,   131,    -1,   123,   124,   125,    -1,    -1,
     128,   129,    -1,    -1,    -1,   121,   134,   135,   136,   137,
     126,   127,   140,    -1,   130,   131,    -1,   145,   146,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,    -1,     7,     8,    -1,    10,    -1,    12,    13,
      -1,    -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      24,   737,    26,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    80,    81,   775,
      64,   777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    94,    -1,    77,    97,    -1,    -1,    -1,    -1,    83,
      -1,   104,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,
      -1,    -1,    -1,   126,   127,   109,    -1,   130,   131,   113,
      -1,   115,    -1,    -1,   830,    -1,    -1,   833,   834,   123,
     124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,
     134,   135,   136,   137,    -1,    -1,   140,    -1,    -1,    -1,
      -1,   145,   146,     3,     4,     5,    -1,     7,    -1,   865,
      10,    -1,    12,    13,    -1,    -1,    16,    17,    18,    -1,
      20,    -1,    22,    -1,    24,    -1,    26,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    80,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    64,    -1,    94,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    77,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    -1,    95,    -1,    -1,   126,   127,
      -1,    -1,   130,   131,    -1,    -1,    -1,    -1,    -1,   109,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   123,   124,   125,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,
     140,     3,     4,     5,    -1,   145,   146,    -1,    10,    -1,
      12,    13,    -1,    -1,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   123,   124,   125,    -1,    -1,   128,   129,    -1,    -1,
      -1,    -1,   134,   135,   136,   137,    -1,     4,   140,    -1,
      -1,     8,    -1,   145,   146,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    21,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    -1,    42,    43,    44,     4,    -1,
      47,    -1,    -1,    -1,    51,    -1,    12,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    21,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,   113,    -1,    -1,    -1,
      -1,    12,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,
      -1,   128,   129,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,   144,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,   113,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,   124,   125,
      -1,    -1,   128,   129,     1,    -1,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,   144,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,   105,    42,    43,    44,    -1,    -1,
      47,    -1,   113,    50,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,   124,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    95,    -1,    97,    -1,
      -1,    -1,    -1,    -1,   103,   104,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,   114,   115,    -1,    -1,    -1,
      -1,   120,   121,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    -1,    95,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,   109,   110,
      -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,
     121,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    -1,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,    -1,    -1,    -1,   121,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      85,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
      -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    46,    47,    -1,
      -1,    50,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    -1,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    -1,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,   113,    -1,   115,    -1,    -1,    -1,   119,    -1,
      -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,
      -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,    -1,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    -1,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,     3,     4,     5,    -1,
      -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    -1,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    50,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   109,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,    -1,
      -1,   128,   129,    -1,    -1,    -1,    -1,   134,   135,   136,
     137,    -1,    -1,   140,     3,     4,     5,    -1,    -1,    -1,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,     3,     4,     5,    -1,    -1,    -1,    -1,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
      -1,    -1,   113,    -1,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,    -1,    -1,   128,   129,    -1,
      -1,    -1,    -1,   134,   135,   136,   137,    -1,    -1,   140,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    -1,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      -1,    -1,    95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,    -1,    -1,
     113,    -1,   115,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,   124,   125,    12,    -1,   128,   129,    -1,    -1,    -1,
      -1,   134,   135,   136,   137,    24,    -1,   140,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    90,    -1,    -1,    -1,    94,    95,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,     4,
     109,    -1,    -1,    -1,   113,   114,   115,    12,    -1,    -1,
      -1,   120,    -1,    -1,    -1,   124,   125,   126,   127,    24,
     129,   130,   131,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    -1,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    92,    -1,    -1,
      95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,     4,   109,    -1,    -1,    -1,   113,    -1,
     115,    12,    -1,    -1,    -1,    -1,   121,    -1,    19,   124,
     125,    -1,   127,    24,   129,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
      -1,    24,   113,    -1,   115,    -1,    -1,    30,    31,    32,
      33,    34,    35,   124,   125,    -1,    39,    40,   129,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      -1,    -1,    95,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,     4,    -1,    -1,   109,    -1,    -1,    -1,
     113,    12,   115,    -1,    -1,    -1,    -1,    -1,   121,    -1,
      -1,   124,   125,    24,   127,    -1,   129,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    95,    -1,    97,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,     4,    -1,    -1,   109,   110,
      -1,    -1,   113,    12,   115,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,   124,   125,    24,    -1,    -1,   129,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    -1,
      39,    40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,    -1,    24,   113,    -1,   115,    -1,    -1,    30,
      31,    32,    33,    34,    35,   124,   125,    -1,    39,    40,
     129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    90,
      -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
      -1,    24,   113,    -1,   115,    28,    -1,    30,    31,    32,
      33,    34,    35,   124,   125,    -1,    39,    40,   129,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,     4,
      -1,    -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,
     113,    -1,   115,    28,    -1,    30,    31,    32,    33,    34,
      35,   124,   125,    -1,    39,    40,   129,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,     4,    -1,    -1,
      95,    -1,    97,    -1,    -1,    12,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,   109,    -1,    -1,    24,   113,    -1,
     115,    -1,    -1,    30,    31,    32,    33,    34,    35,   124,
     125,    -1,    39,    40,   129,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,     4,    -1,    -1,    95,    -1,
      97,    -1,    -1,    12,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,   109,    -1,    -1,    24,   113,   114,   115,    -1,
      -1,    30,    31,    32,    33,    34,    35,   124,   125,    -1,
      39,    40,   129,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,     4,    -1,    -1,    95,    -1,    97,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
     109,    -1,    -1,    24,   113,    -1,   115,    -1,    -1,    30,
      31,    32,    33,    34,    35,   124,   125,    -1,    39,    40,
     129,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,     4,    -1,    -1,    95,    -1,    97,    -1,    -1,    12,
      -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,   109,    -1,
      -1,    24,   113,    -1,   115,    28,    -1,    30,    31,    32,
      33,    34,    35,   124,   125,    -1,    39,    40,   129,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,     4,
      -1,    -1,    95,    -1,    97,    -1,    -1,    12,    -1,    -1,
      -1,    -1,   105,    -1,    -1,    -1,   109,    -1,    -1,    24,
     113,    -1,   115,    -1,    -1,    30,    31,    32,    33,    34,
      35,   124,   125,    -1,    39,    40,   129,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,
      95,    -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,     4,    -1,    -1,   109,     8,    -1,    -1,   113,    12,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    21,   124,
     125,    24,    -1,    -1,   129,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,     4,    42,
      43,    44,    -1,    -1,    47,    -1,    12,    -1,    51,    -1,
      -1,    54,    55,    56,    57,    58,    -1,    -1,    24,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    -1,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    12,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      21,   124,   125,    24,    -1,   128,   129,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
       4,    42,    43,    44,    -1,    -1,    47,   113,    12,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,   124,   125,
      24,    -1,   128,   129,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    -1,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      54,    55,    56,    57,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,    24,    -1,   128,   129,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,   113,
      12,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
     124,   125,    24,    -1,   128,   129,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,    24,    -1,   128,
     129,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,     4,    42,    43,    44,    -1,    -1,
      47,   113,    12,    -1,    51,    -1,    -1,    54,    55,    56,
      57,    58,   124,    -1,    24,    -1,   128,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    -1,    -1,    54,    55,    56,    57,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    -1,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    24,   124,    -1,    -1,
      28,   128,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,     4,    42,    43,    44,    -1,    -1,    47,
      -1,    12,    -1,    51,    -1,    -1,    54,    55,    56,    57,
      58,    -1,    -1,    24,   124,   125,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      -1,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,   104,   105,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    28,   124,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,    -1,    12,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    -1,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,   124,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,   124,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,   104,   105,    30,    31,    32,    33,    34,    35,
      -1,   113,    -1,    39,    40,     4,    42,    43,    44,    -1,
      -1,    47,   124,    12,    -1,    51,    -1,    -1,    54,    55,
      56,    57,    58,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,     4,    42,    43,    44,    -1,    -1,    47,    -1,
      12,    87,    51,    -1,    -1,    54,    55,    56,    57,    58,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    -1,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,   124,    51,
      -1,    -1,    54,    55,    56,    57,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124
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
     154,   169,    23,    81,   248,   248,   113,   232,   270,   271,
     272,   352,    28,   110,   234,   235,   237,   239,    80,    87,
      94,   114,   126,   127,   130,   131,   234,   252,   337,   338,
     367,   367,    85,   256,    92,    87,   121,   246,   333,   337,
     346,    89,   245,   248,   239,   113,   230,   235,   249,   239,
      19,    19,    20,    46,   285,   310,   314,   315,   316,   314,
     299,   300,   316,   306,   119,   288,   308,   365,    78,    78,
      78,    78,   249,   223,   277,   278,   286,   223,    15,   198,
     239,   239,   113,   331,    81,   120,    82,    41,    52,    53,
     182,    78,   135,   363,    81,   120,   229,    87,   311,   351,
     360,   341,    79,    85,   120,   110,   120,   286,   350,   352,
     102,   114,   114,   120,   114,   120,   114,   114,   114,   120,
     116,    91,   136,   349,   249,   349,   349,   121,   178,   327,
     339,   340,   356,   363,   211,   135,   209,   231,   235,   236,
     348,   285,   299,   351,    27,   226,   281,   287,   248,   350,
      79,   317,   318,   319,   349,   350,   352,   290,   114,   114,
     120,   106,   365,     4,   170,   105,   107,   301,   230,   356,
      81,   106,   120,   253,   110,    81,    92,   114,   114,   120,
     114,   114,   116,   257,   258,   259,    27,   218,   239,   235,
     335,   347,   241,   248,    81,   206,   234,   251,   252,   249,
     249,   225,   313,    86,   106,   119,   365,    87,   120,   106,
      25,    27,   222,   106,   119,   365,   285,    82,    81,    82,
     207,     3,   124,   212,   213,   214,   235,   260,   331,   234,
     135,    78,   114,   109,   113,   115,   329,   330,   323,    78,
     285,   121,   121,   285,   298,   316,   285,   294,   294,   350,
     294,   294,   136,   110,    78,    78,    81,    81,   352,   361,
     120,    28,   210,   237,   239,    78,   135,    81,    82,   205,
     265,   226,    82,   121,   225,   106,   120,    82,   102,   163,
     366,    12,   113,   171,   172,   106,   119,   285,   302,   303,
     304,   309,   302,   365,   114,   235,   272,   104,   105,   113,
     254,   255,   345,   260,   235,   234,   120,    87,   345,   105,
     107,   217,   121,   121,   260,   114,   120,   285,   315,   285,
     316,   285,   105,   107,   221,   278,   235,   260,   254,    85,
     192,   214,   348,   114,   116,   120,    79,   110,   230,   233,
     233,   121,   121,   339,   253,   205,   265,    92,    78,   260,
      28,   266,   267,   268,    27,   261,     9,   273,   274,   275,
     285,   318,   294,   320,   350,   113,    21,   144,   163,   173,
     174,   175,   302,   106,   119,    87,   305,   306,   365,   235,
     345,   345,   114,   258,   259,     7,     8,    26,   199,   215,
     216,   278,   216,   234,   288,     7,    26,   202,   203,   219,
     220,   278,   220,   193,   347,    27,   195,    81,   316,   285,
      78,   120,    78,    92,   104,   261,   273,   239,   253,    85,
     238,   243,   247,   248,   269,   105,   107,   273,   113,   189,
     276,   334,   335,   275,    82,   102,   173,   366,   160,   114,
     120,   144,   304,   285,   226,    81,   106,    81,    37,   200,
     348,    19,    37,   198,   239,   106,   119,   365,    11,    19,
     201,   201,   106,   119,   365,    87,   105,   107,   196,   231,
     230,   239,   237,   241,   273,   104,   267,    92,   121,   247,
     327,   262,   263,   264,   288,   262,   114,   239,   240,   250,
     276,   190,   294,   350,   114,   175,   226,   260,   260,   239,
      81,   198,   239,    81,    82,   208,   215,   285,   198,   211,
     219,   194,   347,    79,   197,   198,    79,   197,    92,   243,
     269,   243,   106,   119,   324,   365,   120,   114,   106,   114,
     206,   231,    82,   208,   260,   113,   255,   345,   210,   347,
     106,   106,   119,   365,   365,   239,   264,    81,   250,   345,
      85,   205,   265,   198,   230,   193,   261,   273,   273
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
     212,   212,   213,   213,   214,   214,   215,   215,   215,   216,
     216,   216,   216,   217,   217,   218,   218,   219,   219,   220,
     220,   220,   220,   221,   221,   222,   222,   223,   223,   223,
     223,   224,   224,   225,   226,   226,   227,   227,   228,   228,
     228,   229,   229,   230,   231,   232,   232,   233,   233,   234,
     234,   235,   235,   235,   236,   237,   238,   239,   239,   240,
     241,   242,   242,   243,   243,   244,   244,   244,   245,   246,
     246,   247,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   249,   250,   250,   251,   251,   252,   252,   253,
     253,   254,   254,   254,   255,   255,   256,   256,   257,   257,
     258,   259,   259,   260,   261,   261,   261,   262,   262,   263,
     263,   264,   265,   266,   266,   267,   267,   268,   268,   269,
     269,   270,   270,   271,   271,   272,   273,   273,   274,   274,
     275,   275,   275,   276,   276,   276,   277,   277,   278,   279,
     279,   280,   280,   281,   282,   282,   282,   282,   282,   282,
     282,   282,   282,   283,   283,   284,   284,   285,   285,   286,
     286,   287,   287,   288,   288,   289,   289,   289,   289,   290,
     290,   290,   290,   290,   290,   290,   290,   290,   290,   290,
     291,   291,   291,   292,   292,   292,   292,   292,   292,   292,
     292,   293,   293,   294,   294,   294,   295,   295,   296,   296,
     296,   296,   296,   296,   296,   297,   297,   298,   298,   299,
     300,   300,   301,   301,   301,   301,   302,   302,   303,   303,
     303,   304,   305,   305,   306,   306,   307,   307,   308,   309,
     310,   311,   312,   312,   313,   313,   314,   314,   314,   314,
     315,   315,   316,   316,   316,   317,   317,   318,   318,   318,
     319,   319,   319,   319,   320,   320,   321,   321,   322,   322,
     323,   323,   323,   324,   324,   325,   325,   325,   325,   326,
     326,   327,   327,   328,   328,   329,   329,   329,   330,   330,
     330,   330,   330,   331,   331,   332,   332,   332,   332,   333,
     333,   334,   335,   335,   336,   337,   337,   337,   338,   338,
     338,   338,   339,   339,   340,   340,   341,   341,   342,   342,
     343,   343,   344,   344,   345,   346,   347,   347,   347,   347,
     347,   348,   348,   349,   349,   349,   350,   351,   351,   352,
     352,   352,   352,   352,   352,   352,   352,   353,   353,   354,
     354,   355,   356,   356,   357,   357,   358,   358,   358,   358,
     358,   358,   358,   358,   358,   358,   358,   358,   358,   358,
     358,   358,   358,   358,   359,   359,   360,   360,   361,   362,
     362,   363,   363,   364,   364,   364,   364,   364,   365,   365,
     366,   366,   367,   367
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       0,     2,     1,     3,     1,     2,     2,     2,     0,     3,
       3,     0,     1,     3,     1,     1,     1,     2,     1,     2,
       0,     2,     3,     0,     6,     1,     0,     2,     0,     1,
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
       0,     1,     1,     2,     1,     1,     1,     4,     1,     3,
       2,     1,     0,     3,     3,     2,     0,     1,     1,     3,
       2,     1,     0,     3,     3,     2,     0,     3,     2,     1,
       0,     3,     3,     1,     2,     0,     1,     3,     3,     1,
       0,     0,     2,     1,     1,     3,     1,     1,     3,     1,
       3,     4,     3,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     3,     1,     2,     1,     2,     3,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     3,     2,     5,
       3,     3,     1,     1,     3,     1,     0,     1,     3,     2,
       0,     1,     3,     5,     1,     5,     0,     2,     3,     1,
       3,     2,     0,     1,     4,     4,     0,     1,     0,     3,
       1,     4,     2,     3,     1,     4,     2,     3,     0,     1,
       3,     0,     1,     3,     1,     3,     0,     1,     2,     1,
       2,     3,     3,     1,     2,     3,     1,     2,     1,     3,
       2,     2,     1,     4,     3,     3,     4,     4,     3,     4,
       6,     6,     4,     0,     1,     3,     4,     3,     1,     1,
       3,     2,     1,     1,     0,     2,     3,     2,     1,     3,
       2,     2,     4,     4,     8,     2,     4,     2,     2,     1,
       4,     3,     1,     1,     1,     1,     3,     3,     3,     3,
       1,     3,     2,     1,     2,     2,     3,     3,     1,     1,
       2,     4,     3,     5,     3,     3,     3,     3,     1,     1,
       3,     1,     3,     3,     2,     2,     1,     2,     3,     2,
       1,     2,     3,     2,     2,     1,     3,     2,     4,     1,
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
     598,   600,   602,   603,   605,   622,   623,   625,   626,   628,
     629,   633,   634,   636,   637,   638,   639,   641,   642,   644,
     645,   650,   651,   653,   654,   655,   657,   658,   662,   664,
     665,   667,   668,   669,   670,   671,   672,   673,   679,   681,
     685,   686,   687,   689,   692,   693,   694,   696,   697,   698,
     700,   702,   703,   706,   707,   708,   714,   721,   722,   723,
     724,   725,   727,   728,   729,   731,   733,   735,   737,   742,
     743,   745,   747,   748,   752,   753,   755,   756,   757,   758,
     760,   761,   762,   763,   765,   768,   770,   772,   774,   775,
     777,   777,   779,   779,   783,   785,   792,   799,   800,   803,
     804,   808,   809,   811,   812,   814,   815,   816,   818,   819,
     820,   823,   824,   827,   828,   829,   830,   832,   833,   834,
     839,   840,   842,   843,   845,   859,   887,   888,   890,   892,
     893,   894,   895,   897,   898,   900,   901,   903,   904,   906,
     907,   908,   909,   911,   912,   914,   915,   918,   919,   920,
     921,   923,   924,   926,   928,   929,   937,   938,   940,   941,
     942,   955,   956,   965,   967,   969,   970,   972,   973,   982,
     983,   985,   986,   988,   990,   999,  1001,  1003,  1004,  1006,
    1009,  1011,  1012,  1014,  1015,  1017,  1019,  1020,  1022,  1024,
    1025,  1032,  1039,  1040,  1041,  1042,  1043,  1044,  1045,  1046,
    1052,  1053,  1056,  1058,  1059,  1061,  1062,  1064,  1065,  1072,
    1073,  1075,  1076,  1077,  1080,  1081,  1085,  1086,  1088,  1089,
    1092,  1094,  1095,  1100,  1106,  1107,  1108,  1110,  1111,  1113,
    1114,  1116,  1118,  1120,  1121,  1123,  1124,  1126,  1127,  1129,
    1130,  1136,  1137,  1139,  1140,  1142,  1144,  1145,  1147,  1148,
    1150,  1155,  1160,  1166,  1167,  1168,  1173,  1175,  1177,  1181,
    1182,  1184,  1185,  1189,  1199,  1200,  1202,  1203,  1204,  1205,
    1206,  1207,  1208,  1211,  1212,  1214,  1215,  1220,  1221,  1225,
    1226,  1228,  1229,  1231,  1232,  1237,  1238,  1239,  1240,  1243,
    1244,  1245,  1246,  1247,  1249,  1250,  1251,  1252,  1253,  1255,
    1258,  1259,  1260,  1263,  1264,  1265,  1266,  1267,  1268,  1273,
    1274,  1277,  1278,  1283,  1284,  1285,  1290,  1291,  1309,  1310,
    1311,  1312,  1313,  1314,  1315,  1317,  1318,  1331,  1333,  1343,
    1345,  1346,  1349,  1350,  1351,  1352,  1354,  1355,  1357,  1358,
    1359,  1361,  1363,  1364,  1366,  1367,  1369,  1370,  1372,  1374,
    1376,  1378,  1380,  1381,  1384,  1385,  1387,  1388,  1389,  1390,
    1395,  1396,  1398,  1399,  1400,  1405,  1406,  1408,  1409,  1410,
    1412,  1413,  1414,  1415,  1418,  1419,  1451,  1452,  1454,  1455,
    1457,  1458,  1459,  1461,  1462,  1464,  1465,  1466,  1467,  1469,
    1470,  1472,  1473,  1475,  1476,  1479,  1480,  1481,  1483,  1484,
    1485,  1486,  1487,  1489,  1490,  1492,  1493,  1494,  1495,  1498,
    1499,  1501,  1503,  1504,  1508,  1510,  1511,  1512,  1514,  1515,
    1516,  1517,  1522,  1523,  1525,  1526,  1528,  1529,  1532,  1533,
    1538,  1539,  1541,  1542,  1546,  1548,  1550,  1551,  1552,  1553,
    1554,  1557,  1558,  1560,  1561,  1562,  1564,  1566,  1567,  1569,
    1570,  1571,  1572,  1573,  1574,  1575,  1576,  1578,  1579,  1581,
    1582,  1584,  1586,  1587,  1589,  1590,  1592,  1593,  1594,  1595,
    1596,  1597,  1598,  1599,  1600,  1601,  1602,  1603,  1604,  1605,
    1606,  1607,  1608,  1609,  1611,  1612,  1616,  1617,  1619,  1621,
    1622,  1624,  1625,  1629,  1630,  1631,  1632,  1633,  1638,  1641,
    1649,  1650,  1652,  1653
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
#line 8023 "parser.cc"

#line 1662 "parser.y"


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
