// A Bison parser, made by GNU Bison 3.7.5.

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
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
#line 97 "parser.y"

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

  /*---------------.
  | symbol kinds.  |
  `---------------*/



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
        value.YY_MOVE_OR_COPY< Haskell::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.YY_MOVE_OR_COPY< Haskell::Constructor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.YY_MOVE_OR_COPY< Haskell::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.YY_MOVE_OR_COPY< Haskell::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.YY_MOVE_OR_COPY< Haskell::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.YY_MOVE_OR_COPY< Haskell::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.YY_MOVE_OR_COPY< Haskell::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.YY_MOVE_OR_COPY< Haskell::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.YY_MOVE_OR_COPY< Haskell::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.YY_MOVE_OR_COPY< Haskell::Module > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.YY_MOVE_OR_COPY< Haskell::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.YY_MOVE_OR_COPY< Haskell::StrictLazy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.YY_MOVE_OR_COPY< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.YY_MOVE_OR_COPY< Located<Haskell::Decls> > (YY_MOVE (that.value));
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

      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.YY_MOVE_OR_COPY< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.YY_MOVE_OR_COPY< std::optional<Located<Haskell::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.YY_MOVE_OR_COPY< std::pair<Haskell::Context,expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.YY_MOVE_OR_COPY< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
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

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::Constructor> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.YY_MOVE_OR_COPY< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.YY_MOVE_OR_COPY< std::vector<Haskell::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.YY_MOVE_OR_COPY< std::vector<Haskell::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.YY_MOVE_OR_COPY< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.YY_MOVE_OR_COPY< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
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
      case symbol_kind::S_altslist: // altslist
        value.move< Haskell::Alts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_constr: // constr
        value.move< Haskell::Constructor > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Haskell::Context > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Haskell::DataOrNewtype > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.move< Haskell::Decls > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Haskell::FieldDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix: // infix
        value.move< Haskell::Fixity > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Haskell::GuardedRHS > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Haskell::ImpDecl > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_module: // module
        value.move< Haskell::Module > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Haskell::Stmts > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.move< Haskell::StrictLazy > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Decls> > (YY_MOVE (that.value));
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

      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Haskell::Decls>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Haskell::Context,expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
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

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< std::vector<Haskell::Constructor> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Haskell::GuardedRHS> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Haskell::ImpDecl> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
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
      case symbol_kind::S_altslist: // altslist
        value.copy< Haskell::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.copy< Haskell::Constructor > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.copy< Haskell::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.copy< Haskell::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.copy< Haskell::Decls > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.copy< Haskell::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.copy< Haskell::Fixity > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.copy< Haskell::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.copy< Haskell::ImpDecl > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.copy< Haskell::Module > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.copy< Haskell::Stmts > (that.value);
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.copy< Haskell::StrictLazy > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.copy< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.copy< Located<Haskell::Decls> > (that.value);
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

      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.copy< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.copy< std::optional<Located<Haskell::Decls>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.copy< std::pair<Haskell::Context,expression_ref> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.copy< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (that.value);
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
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
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

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.copy< std::vector<Haskell::Constructor> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.copy< std::vector<Haskell::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.copy< std::vector<Haskell::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.copy< std::vector<Haskell::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.copy< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.copy< std::vector<expression_ref> > (that.value);
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
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
      case symbol_kind::S_altslist: // altslist
        value.move< Haskell::Alts > (that.value);
        break;

      case symbol_kind::S_constr: // constr
        value.move< Haskell::Constructor > (that.value);
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        value.move< Haskell::Context > (that.value);
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        value.move< Haskell::DataOrNewtype > (that.value);
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        value.move< Haskell::Decls > (that.value);
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        value.move< Haskell::FieldDecl > (that.value);
        break;

      case symbol_kind::S_infix: // infix
        value.move< Haskell::Fixity > (that.value);
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        value.move< Haskell::GuardedRHS > (that.value);
        break;

      case symbol_kind::S_importdecl: // importdecl
        value.move< Haskell::ImpDecl > (that.value);
        break;

      case symbol_kind::S_module: // module
        value.move< Haskell::Module > (that.value);
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        value.move< Haskell::Stmts > (that.value);
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        value.move< Haskell::StrictLazy > (that.value);
        break;

      case symbol_kind::S_alt: // alt
        value.move< Located<Haskell::Alt> > (that.value);
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        value.move< Located<Haskell::Decls> > (that.value);
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

      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        value.move< expression_ref > (that.value);
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        value.move< int > (that.value);
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        value.move< std::optional<Located<Haskell::Decls>> > (that.value);
        break;

      case symbol_kind::S_prec: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case symbol_kind::S_maybeas: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        value.move< std::pair<Haskell::Context,expression_ref> > (that.value);
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        value.move< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > (that.value);
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
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
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

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        value.move< std::vector<Haskell::Constructor> > (that.value);
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        value.move< std::vector<Haskell::FieldDecl> > (that.value);
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        value.move< std::vector<Haskell::GuardedRHS> > (that.value);
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        value.move< std::vector<Haskell::ImpDecl> > (that.value);
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        value.move< std::vector<Located<Haskell::Alt>> > (that.value);
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        value.move< std::vector<expression_ref> > (that.value);
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
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
  parser::yypop_ (int n)
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
  parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  parser::yy_table_value_is_error_ (int yyvalue)
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
        yylhs.value.emplace< Haskell::Alts > ();
        break;

      case symbol_kind::S_constr: // constr
        yylhs.value.emplace< Haskell::Constructor > ();
        break;

      case symbol_kind::S_context: // context
      case symbol_kind::S_context_no_ops: // context_no_ops
        yylhs.value.emplace< Haskell::Context > ();
        break;

      case symbol_kind::S_data_or_newtype: // data_or_newtype
        yylhs.value.emplace< Haskell::DataOrNewtype > ();
        break;

      case symbol_kind::S_topdecls: // topdecls
      case symbol_kind::S_topdecls_semi: // topdecls_semi
        yylhs.value.emplace< Haskell::Decls > ();
        break;

      case symbol_kind::S_fielddecl: // fielddecl
        yylhs.value.emplace< Haskell::FieldDecl > ();
        break;

      case symbol_kind::S_infix: // infix
        yylhs.value.emplace< Haskell::Fixity > ();
        break;

      case symbol_kind::S_gdrh: // gdrh
      case symbol_kind::S_gdpat: // gdpat
        yylhs.value.emplace< Haskell::GuardedRHS > ();
        break;

      case symbol_kind::S_importdecl: // importdecl
        yylhs.value.emplace< Haskell::ImpDecl > ();
        break;

      case symbol_kind::S_module: // module
        yylhs.value.emplace< Haskell::Module > ();
        break;

      case symbol_kind::S_stmtlist: // stmtlist
        yylhs.value.emplace< Haskell::Stmts > ();
        break;

      case symbol_kind::S_strict_mark: // strict_mark
      case symbol_kind::S_strictness: // strictness
        yylhs.value.emplace< Haskell::StrictLazy > ();
        break;

      case symbol_kind::S_alt: // alt
        yylhs.value.emplace< Located<Haskell::Alt> > ();
        break;

      case symbol_kind::S_decllist: // decllist
      case symbol_kind::S_binds: // binds
        yylhs.value.emplace< Located<Haskell::Decls> > ();
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

      case symbol_kind::S_maybeexports: // maybeexports
      case symbol_kind::S_export: // export
      case symbol_kind::S_qcname_ext_w_wildcard: // qcname_ext_w_wildcard
      case symbol_kind::S_qcname_ext: // qcname_ext
      case symbol_kind::S_qcname: // qcname
      case symbol_kind::S_maybeimpspec: // maybeimpspec
      case symbol_kind::S_impspec: // impspec
      case symbol_kind::S_topdecl: // topdecl
      case symbol_kind::S_cl_decl: // cl_decl
      case symbol_kind::S_ty_decl: // ty_decl
      case symbol_kind::S_inst_decl: // inst_decl
      case symbol_kind::S_opt_tyconsig: // opt_tyconsig
      case symbol_kind::S_sigtype: // sigtype
      case symbol_kind::S_sigtypedoc: // sigtypedoc
      case symbol_kind::S_ctype: // ctype
      case symbol_kind::S_ctypedoc: // ctypedoc
      case symbol_kind::S_type: // type
      case symbol_kind::S_typedoc: // typedoc
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_tyapp: // tyapp
      case symbol_kind::S_atype_docs: // atype_docs
      case symbol_kind::S_atype: // atype
      case symbol_kind::S_inst_type: // inst_type
      case symbol_kind::S_tv_bndr: // tv_bndr
      case symbol_kind::S_kind: // kind
      case symbol_kind::S_forall: // forall
      case symbol_kind::S_constr_stuff: // constr_stuff
      case symbol_kind::S_decl_no_th: // decl_no_th
      case symbol_kind::S_decl: // decl
      case symbol_kind::S_rhs: // rhs
      case symbol_kind::S_sigdecl: // sigdecl
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_exp10_top: // exp10_top
      case symbol_kind::S_exp10: // exp10
      case symbol_kind::S_aexp: // aexp
      case symbol_kind::S_aexp1: // aexp1
      case symbol_kind::S_aexp2: // aexp2
      case symbol_kind::S_texp: // texp
      case symbol_kind::S_list: // list
      case symbol_kind::S_alt_rhs: // alt_rhs
      case symbol_kind::S_pat: // pat
      case symbol_kind::S_bindpat: // bindpat
      case symbol_kind::S_apat: // apat
      case symbol_kind::S_stmt: // stmt
      case symbol_kind::S_qual: // qual
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< expression_ref > ();
        break;

      case symbol_kind::S_PRIMFLOAT: // "PRIMFLOAT"
        yylhs.value.emplace< float > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
      case symbol_kind::S_PRIMINTEGER: // "PRIMINTEGER"
      case symbol_kind::S_PRINTWORD: // "PRIMWORD"
      case symbol_kind::S_commas: // commas
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_wherebinds: // wherebinds
        yylhs.value.emplace< std::optional<Located<Haskell::Decls>> > ();
        break;

      case symbol_kind::S_prec: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case symbol_kind::S_maybeas: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case symbol_kind::S_tycl_hdr: // tycl_hdr
        yylhs.value.emplace< std::pair<Haskell::Context,expression_ref> > ();
        break;

      case symbol_kind::S_body: // body
      case symbol_kind::S_body2: // body2
      case symbol_kind::S_top: // top
      case symbol_kind::S_top1: // top1
        yylhs.value.emplace< std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();
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
      case symbol_kind::S_qop: // qop
      case symbol_kind::S_qopm: // qopm
      case symbol_kind::S_hole_op: // hole_op
      case symbol_kind::S_qvarop: // qvarop
      case symbol_kind::S_qvaropm: // qvaropm
      case symbol_kind::S_tyvar: // tyvar
      case symbol_kind::S_tyvarop: // tyvarop
      case symbol_kind::S_tyvarid: // tyvarid
      case symbol_kind::S_var: // var
      case symbol_kind::S_qvar: // qvar
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

      case symbol_kind::S_constrs: // constrs
      case symbol_kind::S_constrs1: // constrs1
        yylhs.value.emplace< std::vector<Haskell::Constructor> > ();
        break;

      case symbol_kind::S_fielddecls: // fielddecls
      case symbol_kind::S_fielddecls1: // fielddecls1
        yylhs.value.emplace< std::vector<Haskell::FieldDecl> > ();
        break;

      case symbol_kind::S_gdrhs: // gdrhs
      case symbol_kind::S_gdpats: // gdpats
        yylhs.value.emplace< std::vector<Haskell::GuardedRHS> > ();
        break;

      case symbol_kind::S_importdecls: // importdecls
      case symbol_kind::S_importdecls_semi: // importdecls_semi
        yylhs.value.emplace< std::vector<Haskell::ImpDecl> > ();
        break;

      case symbol_kind::S_alts: // alts
      case symbol_kind::S_alts1: // alts1
        yylhs.value.emplace< std::vector<Located<Haskell::Alt>> > ();
        break;

      case symbol_kind::S_exportlist: // exportlist
      case symbol_kind::S_exportlist1: // exportlist1
      case symbol_kind::S_qcnames: // qcnames
      case symbol_kind::S_qcnames1: // qcnames1
      case symbol_kind::S_decls: // decls
      case symbol_kind::S_sigtypes1: // sigtypes1
      case symbol_kind::S_btype_no_ops: // btype_no_ops
      case symbol_kind::S_tyapps: // tyapps
      case symbol_kind::S_comma_types0: // comma_types0
      case symbol_kind::S_comma_types1: // comma_types1
      case symbol_kind::S_tv_bndrs: // tv_bndrs
      case symbol_kind::S_infixexp: // infixexp
      case symbol_kind::S_infixexp_top: // infixexp_top
      case symbol_kind::S_fexp: // fexp
      case symbol_kind::S_tup_exprs: // tup_exprs
      case symbol_kind::S_lexps: // lexps
      case symbol_kind::S_squals: // squals
      case symbol_kind::S_guardquals: // guardquals
      case symbol_kind::S_guardquals1: // guardquals1
      case symbol_kind::S_apats1: // apats1
      case symbol_kind::S_stmts: // stmts
        yylhs.value.emplace< std::vector<expression_ref> > ();
        break;

      case symbol_kind::S_ops: // ops
      case symbol_kind::S_sig_vars: // sig_vars
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
#line 522 "parser.y"
             {drv.result = yystack_[0].value.as < Haskell::Module > ();}
#line 1886 "parser.cc"
    break;

  case 3: // module: "module" modid maybemodwarning maybeexports "where" body
#line 539 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second);}
#line 1892 "parser.cc"
    break;

  case 4: // module: body2
#line 540 "parser.y"
                                                                 {yylhs.value.as < Haskell::Module > () = make_module("Main",{},yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().first, yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ().second);}
#line 1898 "parser.cc"
    break;

  case 5: // missing_module_keyword: %empty
#line 542 "parser.y"
                                                                 {drv.push_module_context();}
#line 1904 "parser.cc"
    break;

  case 9: // body: "{" top "}"
#line 550 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1910 "parser.cc"
    break;

  case 10: // body: "vocurly" top close
#line 551 "parser.y"
                        {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1916 "parser.cc"
    break;

  case 11: // body2: "{" top "}"
#line 553 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1922 "parser.cc"
    break;

  case 12: // body2: missing_module_keyword top close
#line 554 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[1].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1928 "parser.cc"
    break;

  case 13: // top: semis top1
#line 557 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = yystack_[0].value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > ();}
#line 1934 "parser.cc"
    break;

  case 14: // top1: importdecls_semi topdecls_semi
#line 559 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 1940 "parser.cc"
    break;

  case 15: // top1: importdecls_semi topdecls
#line 560 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (),yystack_[0].value.as < Haskell::Decls > ());}
#line 1946 "parser.cc"
    break;

  case 16: // top1: importdecls
#line 561 "parser.y"
                                           {yylhs.value.as < std::pair<std::vector<Haskell::ImpDecl>, std::optional<Haskell::Decls>> > () = make_body(yystack_[0].value.as < std::vector<Haskell::ImpDecl> > (),{});}
#line 1952 "parser.cc"
    break;

  case 17: // maybeexports: "(" exportlist ")"
#line 569 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1958 "parser.cc"
    break;

  case 18: // maybeexports: %empty
#line 570 "parser.y"
                                      {}
#line 1964 "parser.cc"
    break;

  case 19: // exportlist: exportlist1
#line 572 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1970 "parser.cc"
    break;

  case 20: // exportlist1: exportlist1 "," export
#line 574 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1976 "parser.cc"
    break;

  case 21: // exportlist1: export
#line 575 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1982 "parser.cc"
    break;

  case 22: // export: qcname_ext export_subspec
#line 577 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1988 "parser.cc"
    break;

  case 23: // export: "module" modid
#line 578 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1994 "parser.cc"
    break;

  case 26: // qcnames: %empty
#line 584 "parser.y"
                   {}
#line 2000 "parser.cc"
    break;

  case 27: // qcnames: qcnames1
#line 585 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2006 "parser.cc"
    break;

  case 28: // qcnames1: qcnames1 "," qcname_ext_w_wildcard ","
#line 587 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 2012 "parser.cc"
    break;

  case 29: // qcnames1: qcname_ext_w_wildcard
#line 588 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2018 "parser.cc"
    break;

  case 30: // qcname_ext_w_wildcard: qcname_ext
#line 590 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2024 "parser.cc"
    break;

  case 31: // qcname_ext_w_wildcard: ".."
#line 591 "parser.y"
                                     {}
#line 2030 "parser.cc"
    break;

  case 32: // qcname_ext: qcname
#line 593 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2036 "parser.cc"
    break;

  case 33: // qcname_ext: "type" oqtycon
#line 594 "parser.y"
                                     {}
#line 2042 "parser.cc"
    break;

  case 34: // qcname: qvar
#line 596 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 2048 "parser.cc"
    break;

  case 35: // qcname: oqtycon_no_varcon
#line 597 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 2054 "parser.cc"
    break;

  case 40: // importdecls: importdecls_semi importdecl
#line 607 "parser.y"
                                         { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[1].value.as < std::vector<Haskell::ImpDecl> > (), yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[0].value.as < Haskell::ImpDecl > ()); }
#line 2060 "parser.cc"
    break;

  case 41: // importdecls_semi: importdecls_semi importdecl semis1
#line 609 "parser.y"
                                                     { yylhs.value.as < std::vector<Haskell::ImpDecl> > () = yystack_[2].value.as < std::vector<Haskell::ImpDecl> > (); yylhs.value.as < std::vector<Haskell::ImpDecl> > ().push_back(yystack_[1].value.as < Haskell::ImpDecl > ()); }
#line 2066 "parser.cc"
    break;

  case 42: // importdecls_semi: %empty
#line 610 "parser.y"
                         { }
#line 2072 "parser.cc"
    break;

  case 43: // importdecl: "import" optqualified modid maybeas maybeimpspec
#line 612 "parser.y"
                                                                                                        {
    yylhs.value.as < Haskell::ImpDecl > () = Haskell::ImpDecl(yystack_[3].value.as < bool > (),yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<std::string> > (),yystack_[0].value.as < expression_ref > ());
}
#line 2080 "parser.cc"
    break;

  case 44: // optqualified: "qualified"
#line 625 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 2086 "parser.cc"
    break;

  case 45: // optqualified: %empty
#line 626 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 2092 "parser.cc"
    break;

  case 46: // maybeas: "as" modid
#line 628 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 2098 "parser.cc"
    break;

  case 47: // maybeas: %empty
#line 629 "parser.y"
                               { }
#line 2104 "parser.cc"
    break;

  case 48: // maybeimpspec: impspec
#line 631 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 2110 "parser.cc"
    break;

  case 49: // maybeimpspec: %empty
#line 632 "parser.y"
                               { }
#line 2116 "parser.cc"
    break;

  case 50: // impspec: "(" exportlist ")"
#line 634 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2122 "parser.cc"
    break;

  case 51: // impspec: "hiding" "(" exportlist ")"
#line 635 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 2128 "parser.cc"
    break;

  case 52: // prec: %empty
#line 640 "parser.y"
                   { }
#line 2134 "parser.cc"
    break;

  case 53: // prec: "INTEGER"
#line 641 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 2140 "parser.cc"
    break;

  case 54: // infix: "infix"
#line 643 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infix; }
#line 2146 "parser.cc"
    break;

  case 55: // infix: "infixl"
#line 644 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixl; }
#line 2152 "parser.cc"
    break;

  case 56: // infix: "infixr"
#line 645 "parser.y"
                   { yylhs.value.as < Haskell::Fixity > () = Haskell::Fixity::infixr; }
#line 2158 "parser.cc"
    break;

  case 57: // ops: ops "," op
#line 647 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 2164 "parser.cc"
    break;

  case 58: // ops: op
#line 648 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 2170 "parser.cc"
    break;

  case 59: // topdecls: topdecls_semi topdecl
#line 652 "parser.y"
                                 { yylhs.value.as < Haskell::Decls > () = yystack_[1].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 2176 "parser.cc"
    break;

  case 60: // topdecls_semi: topdecls_semi topdecl semis1
#line 654 "parser.y"
                                            { yylhs.value.as < Haskell::Decls > () = yystack_[2].value.as < Haskell::Decls > (); yylhs.value.as < Haskell::Decls > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 2182 "parser.cc"
    break;

  case 61: // topdecls_semi: %empty
#line 655 "parser.y"
                                            { }
#line 2188 "parser.cc"
    break;

  case 62: // topdecl: cl_decl
#line 657 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2194 "parser.cc"
    break;

  case 63: // topdecl: ty_decl
#line 658 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2200 "parser.cc"
    break;

  case 64: // topdecl: inst_decl
#line 659 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2206 "parser.cc"
    break;

  case 65: // topdecl: "default" "(" comma_types0 ")"
#line 662 "parser.y"
                                               {}
#line 2212 "parser.cc"
    break;

  case 66: // topdecl: decl_no_th
#line 669 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2218 "parser.cc"
    break;

  case 67: // topdecl: infixexp_top
#line 671 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2224 "parser.cc"
    break;

  case 68: // topdecl: "builtin" var "INTEGER" "STRING" "STRING"
#line 672 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2230 "parser.cc"
    break;

  case 69: // topdecl: "builtin" var "INTEGER" "STRING"
#line 673 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2236 "parser.cc"
    break;

  case 70: // topdecl: "builtin" varop "INTEGER" "STRING" "STRING"
#line 674 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 2242 "parser.cc"
    break;

  case 71: // topdecl: "builtin" varop "INTEGER" "STRING"
#line 675 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 2248 "parser.cc"
    break;

  case 72: // cl_decl: "class" tycl_hdr wherebinds
#line 677 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_class_decl(yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[1].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2254 "parser.cc"
    break;

  case 73: // ty_decl: "type" type "=" ctypedoc
#line 679 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_type_synonym({yystack_[2].location,yystack_[2].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2260 "parser.cc"
    break;

  case 74: // ty_decl: data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
#line 680 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < Haskell::DataOrNewtype > (),yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().first,yystack_[2].value.as < std::pair<Haskell::Context,expression_ref> > ().second,yystack_[1].value.as < std::vector<Haskell::Constructor> > ());}
#line 2266 "parser.cc"
    break;

  case 75: // ty_decl: data_or_newtype capi_ctype tycl_hdr opt_kind_sig
#line 681 "parser.y"
                                                                           {}
#line 2272 "parser.cc"
    break;

  case 76: // inst_decl: "instance" overlap_pragma inst_type wherebinds
#line 686 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_instance_decl({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2278 "parser.cc"
    break;

  case 86: // data_or_newtype: "data"
#line 741 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::data;}
#line 2284 "parser.cc"
    break;

  case 87: // data_or_newtype: "newtype"
#line 742 "parser.y"
                           {yylhs.value.as < Haskell::DataOrNewtype > ()=Haskell::DataOrNewtype::newtype;}
#line 2290 "parser.cc"
    break;

  case 90: // tycl_hdr: context "=>" type
#line 754 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ()};}
#line 2296 "parser.cc"
    break;

  case 91: // tycl_hdr: type
#line 755 "parser.y"
                             {yylhs.value.as < std::pair<Haskell::Context,expression_ref> > () = {{},yystack_[0].value.as < expression_ref > ()};}
#line 2302 "parser.cc"
    break;

  case 95: // decls: decls ";" decl
#line 803 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2308 "parser.cc"
    break;

  case 96: // decls: decls ";"
#line 804 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2314 "parser.cc"
    break;

  case 97: // decls: decl
#line 805 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2320 "parser.cc"
    break;

  case 98: // decls: %empty
#line 806 "parser.y"
                        {}
#line 2326 "parser.cc"
    break;

  case 99: // decllist: "{" decls "}"
#line 808 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = make_decls(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2332 "parser.cc"
    break;

  case 100: // decllist: "vocurly" decls close
#line 809 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = make_decls(yystack_[1].location,yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2338 "parser.cc"
    break;

  case 101: // binds: decllist
#line 811 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Decls> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2344 "parser.cc"
    break;

  case 102: // wherebinds: "where" binds
#line 813 "parser.y"
                                 {yylhs.value.as < std::optional<Located<Haskell::Decls>> > () = yystack_[0].value.as < Located<Haskell::Decls> > ();}
#line 2350 "parser.cc"
    break;

  case 103: // wherebinds: %empty
#line 814 "parser.y"
                                 {}
#line 2356 "parser.cc"
    break;

  case 109: // opt_tyconsig: %empty
#line 840 "parser.y"
                     {}
#line 2362 "parser.cc"
    break;

  case 110: // opt_tyconsig: "::" gtycon
#line 841 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2368 "parser.cc"
    break;

  case 111: // sigtype: ctype
#line 843 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2374 "parser.cc"
    break;

  case 112: // sigtypedoc: ctypedoc
#line 845 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2380 "parser.cc"
    break;

  case 113: // sig_vars: sig_vars "," var
#line 847 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2386 "parser.cc"
    break;

  case 114: // sig_vars: var
#line 848 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2392 "parser.cc"
    break;

  case 115: // sigtypes1: sigtype
#line 850 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2398 "parser.cc"
    break;

  case 116: // sigtypes1: sigtypes1 "," sigtype
#line 851 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2404 "parser.cc"
    break;

  case 117: // strict_mark: strictness
#line 855 "parser.y"
                                            {yylhs.value.as < Haskell::StrictLazy > () = yystack_[0].value.as < Haskell::StrictLazy > ();}
#line 2410 "parser.cc"
    break;

  case 118: // strictness: "!"
#line 861 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::strict;}
#line 2416 "parser.cc"
    break;

  case 119: // strictness: "~"
#line 862 "parser.y"
                {yylhs.value.as < Haskell::StrictLazy > () = Haskell::StrictLazy::lazy;}
#line 2422 "parser.cc"
    break;

  case 120: // ctype: "forall" tv_bndrs "." ctype
#line 869 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_forall_type(yystack_[2].value.as < std::vector<expression_ref> > (), yystack_[0].value.as < expression_ref > ());}
#line 2428 "parser.cc"
    break;

  case 121: // ctype: context "=>" ctype
#line 870 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_constrained_type(yystack_[2].value.as < Haskell::Context > (),yystack_[0].value.as < expression_ref > ());}
#line 2434 "parser.cc"
    break;

  case 122: // ctype: type
#line 872 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2440 "parser.cc"
    break;

  case 123: // ctypedoc: ctype
#line 874 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2446 "parser.cc"
    break;

  case 124: // context: btype
#line 883 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(yystack_[0].value.as < expression_ref > ());}
#line 2452 "parser.cc"
    break;

  case 125: // context_no_ops: btype_no_ops
#line 885 "parser.y"
                                   {yylhs.value.as < Haskell::Context > () = make_context(make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2458 "parser.cc"
    break;

  case 126: // type: btype
#line 887 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2464 "parser.cc"
    break;

  case 127: // type: btype "->" ctype
#line 888 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2470 "parser.cc"
    break;

  case 128: // typedoc: type
#line 890 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2476 "parser.cc"
    break;

  case 129: // btype: tyapps
#line 893 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2482 "parser.cc"
    break;

  case 130: // btype_no_ops: atype_docs
#line 895 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2488 "parser.cc"
    break;

  case 131: // btype_no_ops: btype_no_ops atype_docs
#line 896 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2494 "parser.cc"
    break;

  case 132: // tyapps: tyapp
#line 898 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2500 "parser.cc"
    break;

  case 133: // tyapps: tyapps tyapp
#line 899 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2506 "parser.cc"
    break;

  case 134: // tyapp: atype
#line 901 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2512 "parser.cc"
    break;

  case 135: // tyapp: qtyconop
#line 902 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2518 "parser.cc"
    break;

  case 136: // tyapp: tyvarop
#line 903 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2524 "parser.cc"
    break;

  case 137: // atype_docs: atype
#line 909 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2530 "parser.cc"
    break;

  case 138: // atype: ntgtycon
#line 916 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2536 "parser.cc"
    break;

  case 139: // atype: tyvar
#line 917 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2542 "parser.cc"
    break;

  case 140: // atype: "*"
#line 918 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("*");}
#line 2548 "parser.cc"
    break;

  case 141: // atype: strict_mark atype
#line 919 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_strict_lazy_type(yystack_[1].value.as < Haskell::StrictLazy > (),yystack_[0].value.as < expression_ref > ());}
#line 2554 "parser.cc"
    break;

  case 142: // atype: "{" fielddecls "}"
#line 920 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_field_decls(yystack_[1].value.as < std::vector<Haskell::FieldDecl> > ());}
#line 2560 "parser.cc"
    break;

  case 143: // atype: "(" ")"
#line 921 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_var("()");}
#line 2566 "parser.cc"
    break;

  case 144: // atype: "(" comma_types1 "," ctype ")"
#line 922 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = make_tuple_type(ts);}
#line 2572 "parser.cc"
    break;

  case 145: // atype: "[" ctype "]"
#line 928 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_list_type(yystack_[1].value.as < expression_ref > ());}
#line 2578 "parser.cc"
    break;

  case 146: // atype: "(" ctype ")"
#line 929 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2584 "parser.cc"
    break;

  case 147: // atype: "(" ctype "::" kind ")"
#line 930 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_of_kind(yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ());}
#line 2590 "parser.cc"
    break;

  case 148: // inst_type: sigtype
#line 933 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2596 "parser.cc"
    break;

  case 151: // comma_types0: comma_types1
#line 938 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2602 "parser.cc"
    break;

  case 152: // comma_types0: %empty
#line 939 "parser.y"
                                       { /* default construction OK */ }
#line 2608 "parser.cc"
    break;

  case 153: // comma_types1: ctype
#line 941 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2614 "parser.cc"
    break;

  case 154: // comma_types1: comma_types1 "," ctype
#line 942 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2620 "parser.cc"
    break;

  case 155: // tv_bndrs: tv_bndrs tv_bndr
#line 949 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2626 "parser.cc"
    break;

  case 156: // tv_bndrs: %empty
#line 950 "parser.y"
                               { /* default construction OK */}
#line 2632 "parser.cc"
    break;

  case 157: // tv_bndr: tyvar
#line 952 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var(yystack_[0].value.as < std::string > ());}
#line 2638 "parser.cc"
    break;

  case 158: // tv_bndr: "(" tyvar "::" kind ")"
#line 953 "parser.y"
                                    {yylhs.value.as < expression_ref > () = make_type_var_of_kind(yystack_[3].value.as < std::string > (),yystack_[1].value.as < expression_ref > ());}
#line 2644 "parser.cc"
    break;

  case 159: // kind: ctype
#line 971 "parser.y"
             {yylhs.value.as < expression_ref > () = make_kind(yystack_[0].value.as < expression_ref > ());}
#line 2650 "parser.cc"
    break;

  case 160: // constrs: "=" constrs1
#line 977 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[0].value.as < std::vector<Haskell::Constructor> > ();}
#line 2656 "parser.cc"
    break;

  case 161: // constrs1: constrs1 "|" constr
#line 979 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > () = yystack_[2].value.as < std::vector<Haskell::Constructor> > (); yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2662 "parser.cc"
    break;

  case 162: // constrs1: constr
#line 980 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::Constructor> > ().push_back(yystack_[0].value.as < Haskell::Constructor > ());}
#line 2668 "parser.cc"
    break;

  case 163: // constr: forall context_no_ops "=>" constr_stuff
#line 982 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[3].value.as < expression_ref > (),yystack_[2].value.as < Haskell::Context > (), yystack_[0].value.as < expression_ref > ());}
#line 2674 "parser.cc"
    break;

  case 164: // constr: forall constr_stuff
#line 983 "parser.y"
                                                {yylhs.value.as < Haskell::Constructor > () = make_constructor(yystack_[1].value.as < expression_ref > (),{}, yystack_[0].value.as < expression_ref > ());}
#line 2680 "parser.cc"
    break;

  case 165: // forall: "forall" tv_bndrs "."
#line 985 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2686 "parser.cc"
    break;

  case 166: // forall: %empty
#line 986 "parser.y"
                                {}
#line 2692 "parser.cc"
    break;

  case 167: // constr_stuff: btype_no_ops
#line 988 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2698 "parser.cc"
    break;

  case 168: // constr_stuff: btype_no_ops conop btype_no_ops
#line 989 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({make_type_var(yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2704 "parser.cc"
    break;

  case 169: // fielddecls: %empty
#line 991 "parser.y"
                                {}
#line 2710 "parser.cc"
    break;

  case 170: // fielddecls: fielddecls1
#line 992 "parser.y"
                                {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[0].value.as < std::vector<Haskell::FieldDecl> > ();}
#line 2716 "parser.cc"
    break;

  case 171: // fielddecls1: fielddecls1 "," fielddecl
#line 994 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > () = yystack_[2].value.as < std::vector<Haskell::FieldDecl> > (); yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2722 "parser.cc"
    break;

  case 172: // fielddecls1: fielddecl
#line 995 "parser.y"
                                        {yylhs.value.as < std::vector<Haskell::FieldDecl> > ().push_back(yystack_[0].value.as < Haskell::FieldDecl > ());}
#line 2728 "parser.cc"
    break;

  case 173: // fielddecl: sig_vars "::" ctype
#line 997 "parser.y"
                                        {yylhs.value.as < Haskell::FieldDecl > () = make_field_decl(yystack_[2].value.as < std::vector<std::string> > (),yystack_[0].value.as < expression_ref > ());}
#line 2734 "parser.cc"
    break;

  case 184: // decl_no_th: sigdecl
#line 1016 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2740 "parser.cc"
    break;

  case 185: // decl_no_th: "!" aexp rhs
#line 1018 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2746 "parser.cc"
    break;

  case 186: // decl_no_th: infixexp_top rhs
#line 1024 "parser.y"
                                  {yylhs.value.as < expression_ref > () = make_value_decl(make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ());}
#line 2752 "parser.cc"
    break;

  case 187: // decl: decl_no_th
#line 1029 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2758 "parser.cc"
    break;

  case 188: // rhs: "=" exp wherebinds
#line 1033 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2764 "parser.cc"
    break;

  case 189: // rhs: gdrhs wherebinds
#line 1034 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 2770 "parser.cc"
    break;

  case 190: // gdrhs: gdrhs gdrh
#line 1036 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2776 "parser.cc"
    break;

  case 191: // gdrhs: gdrh
#line 1037 "parser.y"
                              {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 2782 "parser.cc"
    break;

  case 192: // gdrh: "|" guardquals "=" exp
#line 1041 "parser.y"
                              {yylhs.value.as < Haskell::GuardedRHS > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2788 "parser.cc"
    break;

  case 193: // sigdecl: infixexp_top "::" sigtypedoc
#line 1043 "parser.y"
                                             { yylhs.value.as < expression_ref > () = expression_ref(AST_node("Decl:sigtype"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2794 "parser.cc"
    break;

  case 194: // sigdecl: var "," sig_vars "::" sigtypedoc
#line 1044 "parser.y"
                                          {}
#line 2800 "parser.cc"
    break;

  case 195: // sigdecl: infix prec ops
#line 1045 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_fixity_decl(yystack_[2].value.as < Haskell::Fixity > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2806 "parser.cc"
    break;

  case 196: // sigdecl: "{-# COMPLETE" con_list opt_tyconsig "#-}"
#line 1047 "parser.y"
                                                    {}
#line 2812 "parser.cc"
    break;

  case 197: // sigdecl: "{-# INLINE" activation qvar "#-}"
#line 1048 "parser.y"
                                            {}
#line 2818 "parser.cc"
    break;

  case 198: // sigdecl: "{-# SCC" qvar "#-}"
#line 1049 "parser.y"
                              {}
#line 2824 "parser.cc"
    break;

  case 199: // sigdecl: "{-# SCC" qvar "STRING" "#-}"
#line 1050 "parser.y"
                                     {}
#line 2830 "parser.cc"
    break;

  case 200: // sigdecl: "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
#line 1051 "parser.y"
                                                               {}
#line 2836 "parser.cc"
    break;

  case 201: // sigdecl: "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
#line 1052 "parser.y"
                                                                      {}
#line 2842 "parser.cc"
    break;

  case 202: // sigdecl: "{-# SPECIALISE" "instance" inst_type "#-}"
#line 1053 "parser.y"
                                                     {}
#line 2848 "parser.cc"
    break;

  case 207: // exp: infixexp "::" sigtype
#line 1064 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2854 "parser.cc"
    break;

  case 208: // exp: infixexp
#line 1065 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2860 "parser.cc"
    break;

  case 209: // infixexp: exp10
#line 1067 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2866 "parser.cc"
    break;

  case 210: // infixexp: infixexp qop exp10
#line 1068 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2872 "parser.cc"
    break;

  case 211: // infixexp_top: exp10_top
#line 1070 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2878 "parser.cc"
    break;

  case 212: // infixexp_top: infixexp_top qop exp10_top
#line 1071 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()})); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2884 "parser.cc"
    break;

  case 213: // exp10_top: "-" fexp
#line 1073 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2890 "parser.cc"
    break;

  case 214: // exp10_top: "{-# CORE" "STRING" "#-}"
#line 1074 "parser.y"
                                   {}
#line 2896 "parser.cc"
    break;

  case 215: // exp10_top: fexp
#line 1075 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2902 "parser.cc"
    break;

  case 216: // exp10: exp10_top
#line 1077 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2908 "parser.cc"
    break;

  case 217: // exp10: scc_annot exp
#line 1078 "parser.y"
                                 {}
#line 2914 "parser.cc"
    break;

  case 222: // fexp: fexp aexp
#line 1089 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2920 "parser.cc"
    break;

  case 223: // fexp: fexp "TYPEAPP" atype
#line 1090 "parser.y"
                                 {}
#line 2926 "parser.cc"
    break;

  case 224: // fexp: "static" aexp
#line 1091 "parser.y"
                                 {}
#line 2932 "parser.cc"
    break;

  case 225: // fexp: aexp
#line 1092 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2938 "parser.cc"
    break;

  case 226: // aexp: qvar "@" aexp
#line 1094 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(make_var({yystack_[2].location,yystack_[2].value.as < std::string > ()}),yystack_[0].value.as < expression_ref > ());}
#line 2944 "parser.cc"
    break;

  case 227: // aexp: "~" aexp
#line 1095 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2950 "parser.cc"
    break;

  case 228: // aexp: "\\" apats1 "->" exp
#line 1096 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambdaexp(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2956 "parser.cc"
    break;

  case 229: // aexp: "let" binds "in" exp
#line 1097 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < Located<Haskell::Decls> > (),{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2962 "parser.cc"
    break;

  case 230: // aexp: "if" exp optSemi "then" exp optSemi "else" exp
#line 1099 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if({yystack_[6].location,yystack_[6].value.as < expression_ref > ()},{yystack_[3].location,yystack_[3].value.as < expression_ref > ()},{yystack_[0].location,yystack_[0].value.as < expression_ref > ()});}
#line 2968 "parser.cc"
    break;

  case 231: // aexp: "case" exp "of" altslist
#line 1101 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < Haskell::Alts > ());}
#line 2974 "parser.cc"
    break;

  case 232: // aexp: "do" stmtlist
#line 1102 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2980 "parser.cc"
    break;

  case 233: // aexp: "mdo" stmtlist
#line 1103 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < Haskell::Stmts > ());}
#line 2986 "parser.cc"
    break;

  case 234: // aexp: aexp1
#line 1105 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2992 "parser.cc"
    break;

  case 235: // aexp1: aexp1 "{" fbinds "}"
#line 1107 "parser.y"
                              {}
#line 2998 "parser.cc"
    break;

  case 236: // aexp1: aexp2
#line 1108 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3004 "parser.cc"
    break;

  case 237: // aexp2: qvar
#line 1110 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3010 "parser.cc"
    break;

  case 238: // aexp2: qcon
#line 1111 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()});}
#line 3016 "parser.cc"
    break;

  case 239: // aexp2: literal
#line 1112 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3022 "parser.cc"
    break;

  case 240: // aexp2: "(" texp ")"
#line 1113 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3028 "parser.cc"
    break;

  case 241: // aexp2: "(" tup_exprs ")"
#line 1114 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3034 "parser.cc"
    break;

  case 242: // aexp2: "[" list "]"
#line 1119 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 3040 "parser.cc"
    break;

  case 243: // aexp2: "_"
#line 1120 "parser.y"
                              {yylhs.value.as < expression_ref > () = Haskell::WildcardPattern();}
#line 3046 "parser.cc"
    break;

  case 244: // texp: exp
#line 1125 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3052 "parser.cc"
    break;

  case 245: // texp: infixexp qop
#line 1126 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_var({yystack_[0].location,yystack_[0].value.as < std::string > ()})});}
#line 3058 "parser.cc"
    break;

  case 246: // texp: qopm infixexp
#line 1127 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_var({yystack_[1].location,yystack_[1].value.as < std::string > ()}),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 3064 "parser.cc"
    break;

  case 247: // tup_exprs: tup_exprs "," texp
#line 1132 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3070 "parser.cc"
    break;

  case 248: // tup_exprs: texp "," texp
#line 1133 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3076 "parser.cc"
    break;

  case 249: // list: texp
#line 1151 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list({yystack_[0].value.as < expression_ref > ()}); }
#line 3082 "parser.cc"
    break;

  case 250: // list: lexps
#line 1152 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 3088 "parser.cc"
    break;

  case 251: // list: texp ".."
#line 1153 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 3094 "parser.cc"
    break;

  case 252: // list: texp "," exp ".."
#line 1154 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 3100 "parser.cc"
    break;

  case 253: // list: texp ".." exp
#line 1155 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3106 "parser.cc"
    break;

  case 254: // list: texp "," exp ".." exp
#line 1156 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 3112 "parser.cc"
    break;

  case 255: // list: texp "|" squals
#line 1157 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 3118 "parser.cc"
    break;

  case 256: // lexps: lexps "," texp
#line 1159 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3124 "parser.cc"
    break;

  case 257: // lexps: texp "," texp
#line 1160 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3130 "parser.cc"
    break;

  case 258: // squals: squals "," qual
#line 1173 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3136 "parser.cc"
    break;

  case 259: // squals: qual
#line 1175 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3142 "parser.cc"
    break;

  case 260: // guardquals: guardquals1
#line 1185 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3148 "parser.cc"
    break;

  case 261: // guardquals1: guardquals1 "," qual
#line 1187 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3154 "parser.cc"
    break;

  case 262: // guardquals1: qual
#line 1188 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3160 "parser.cc"
    break;

  case 263: // altslist: "{" alts "}"
#line 1191 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ());}
#line 3166 "parser.cc"
    break;

  case 264: // altslist: "vocurly" alts close
#line 1192 "parser.y"
                                 {yylhs.value.as < Haskell::Alts > () = make_alts(yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ());}
#line 3172 "parser.cc"
    break;

  case 265: // altslist: "{" "}"
#line 1193 "parser.y"
                                 {}
#line 3178 "parser.cc"
    break;

  case 266: // altslist: "vocurly" close
#line 1194 "parser.y"
                                 {}
#line 3184 "parser.cc"
    break;

  case 267: // alts: alts1
#line 1196 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3190 "parser.cc"
    break;

  case 268: // alts: ";" alts
#line 1197 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[0].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3196 "parser.cc"
    break;

  case 269: // alts1: alts1 ";" alt
#line 1199 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[2].value.as < std::vector<Located<Haskell::Alt>> > (); yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3202 "parser.cc"
    break;

  case 270: // alts1: alts1 ";"
#line 1200 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > () = yystack_[1].value.as < std::vector<Located<Haskell::Alt>> > ();}
#line 3208 "parser.cc"
    break;

  case 271: // alts1: alt
#line 1201 "parser.y"
                                 {yylhs.value.as < std::vector<Located<Haskell::Alt>> > ().push_back(yystack_[0].value.as < Located<Haskell::Alt> > ());}
#line 3214 "parser.cc"
    break;

  case 272: // alt: pat alt_rhs
#line 1203 "parser.y"
                                 {yylhs.value.as < Located<Haskell::Alt> > () = yy_make_alt(yystack_[1].location+yystack_[0].location,yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3220 "parser.cc"
    break;

  case 273: // alt_rhs: "->" exp wherebinds
#line 1205 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs({yystack_[1].location,yystack_[1].value.as < expression_ref > ()},yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 3226 "parser.cc"
    break;

  case 274: // alt_rhs: gdpats wherebinds
#line 1206 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (),yystack_[0].value.as < std::optional<Located<Haskell::Decls>> > ());}
#line 3232 "parser.cc"
    break;

  case 275: // gdpats: gdpats gdpat
#line 1208 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > () = yystack_[1].value.as < std::vector<Haskell::GuardedRHS> > (); yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3238 "parser.cc"
    break;

  case 276: // gdpats: gdpat
#line 1209 "parser.y"
                                 {yylhs.value.as < std::vector<Haskell::GuardedRHS> > ().push_back(yystack_[0].value.as < Haskell::GuardedRHS > ());}
#line 3244 "parser.cc"
    break;

  case 277: // gdpat: "|" guardquals "->" exp
#line 1218 "parser.y"
                                 {yylhs.value.as < Haskell::GuardedRHS > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3250 "parser.cc"
    break;

  case 278: // pat: exp
#line 1220 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3256 "parser.cc"
    break;

  case 279: // pat: "!" aexp
#line 1221 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3262 "parser.cc"
    break;

  case 280: // bindpat: exp
#line 1223 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3268 "parser.cc"
    break;

  case 281: // bindpat: "!" aexp
#line 1224 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3274 "parser.cc"
    break;

  case 282: // apat: aexp
#line 1226 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3280 "parser.cc"
    break;

  case 283: // apat: "!" aexp
#line 1227 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3286 "parser.cc"
    break;

  case 284: // apats1: apats1 apat
#line 1229 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3292 "parser.cc"
    break;

  case 285: // apats1: apat
#line 1230 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3298 "parser.cc"
    break;

  case 286: // stmtlist: "{" stmts "}"
#line 1233 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3304 "parser.cc"
    break;

  case 287: // stmtlist: "vocurly" stmts close
#line 1234 "parser.y"
                               {yylhs.value.as < Haskell::Stmts > () = make_stmts(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 3310 "parser.cc"
    break;

  case 288: // stmts: stmts ";" stmt
#line 1236 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3316 "parser.cc"
    break;

  case 289: // stmts: stmts ";"
#line 1237 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3322 "parser.cc"
    break;

  case 290: // stmts: stmt
#line 1238 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3328 "parser.cc"
    break;

  case 291: // stmts: %empty
#line 1239 "parser.y"
                       {}
#line 3334 "parser.cc"
    break;

  case 292: // stmt: qual
#line 1244 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3340 "parser.cc"
    break;

  case 293: // stmt: "rec" stmtlist
#line 1245 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::RecStmt(yystack_[0].value.as < Haskell::Stmts > ());}
#line 3346 "parser.cc"
    break;

  case 294: // qual: bindpat "<-" exp
#line 1247 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::PatQual(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3352 "parser.cc"
    break;

  case 295: // qual: exp
#line 1248 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::SimpleQual(yystack_[0].value.as < expression_ref > ());}
#line 3358 "parser.cc"
    break;

  case 296: // qual: "let" binds
#line 1249 "parser.y"
                        {yylhs.value.as < expression_ref > () = Haskell::LetQual(yystack_[0].value.as < Located<Haskell::Decls> > ());}
#line 3364 "parser.cc"
    break;

  case 304: // qcon: gen_qcon
#line 1294 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3370 "parser.cc"
    break;

  case 305: // qcon: sysdcon
#line 1295 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3376 "parser.cc"
    break;

  case 306: // gen_qcon: qconid
#line 1297 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3382 "parser.cc"
    break;

  case 307: // gen_qcon: "(" qconsym ")"
#line 1298 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3388 "parser.cc"
    break;

  case 308: // con: conid
#line 1300 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3394 "parser.cc"
    break;

  case 309: // con: "(" consym ")"
#line 1301 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3400 "parser.cc"
    break;

  case 310: // con: sysdcon
#line 1302 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3406 "parser.cc"
    break;

  case 313: // sysdcon_no_list: "(" ")"
#line 1307 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3412 "parser.cc"
    break;

  case 314: // sysdcon_no_list: "(" commas ")"
#line 1308 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3418 "parser.cc"
    break;

  case 315: // sysdcon_no_list: "(#" "#)"
#line 1309 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3424 "parser.cc"
    break;

  case 316: // sysdcon_no_list: "(#" commas "#)"
#line 1310 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3430 "parser.cc"
    break;

  case 317: // sysdcon: sysdcon_no_list
#line 1312 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3436 "parser.cc"
    break;

  case 318: // sysdcon: "[" "]"
#line 1313 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3442 "parser.cc"
    break;

  case 319: // conop: consym
#line 1315 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3448 "parser.cc"
    break;

  case 320: // conop: "`" conid "`"
#line 1316 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3454 "parser.cc"
    break;

  case 321: // qconop: qconsym
#line 1318 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3460 "parser.cc"
    break;

  case 322: // qconop: "`" qconid "`"
#line 1319 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3466 "parser.cc"
    break;

  case 323: // gtycon: ntgtycon
#line 1322 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3472 "parser.cc"
    break;

  case 324: // gtycon: "(" ")"
#line 1323 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3478 "parser.cc"
    break;

  case 325: // gtycon: "(#" "#)"
#line 1324 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3484 "parser.cc"
    break;

  case 326: // ntgtycon: oqtycon
#line 1326 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3490 "parser.cc"
    break;

  case 327: // ntgtycon: "(" commas ")"
#line 1327 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3496 "parser.cc"
    break;

  case 328: // ntgtycon: "(#" commas "#)"
#line 1328 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3502 "parser.cc"
    break;

  case 329: // ntgtycon: "(" "->" ")"
#line 1329 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3508 "parser.cc"
    break;

  case 330: // ntgtycon: "[" "]"
#line 1330 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3514 "parser.cc"
    break;

  case 331: // oqtycon: qtycon
#line 1332 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3520 "parser.cc"
    break;

  case 332: // oqtycon: "(" qtyconsym ")"
#line 1333 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3526 "parser.cc"
    break;

  case 333: // oqtycon: "(" "~" ")"
#line 1334 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3532 "parser.cc"
    break;

  case 334: // oqtycon_no_varcon: qtycon
#line 1336 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3538 "parser.cc"
    break;

  case 335: // oqtycon_no_varcon: "(" "QCONSYM" ")"
#line 1337 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3544 "parser.cc"
    break;

  case 336: // oqtycon_no_varcon: "(" "CONSYM" ")"
#line 1338 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3550 "parser.cc"
    break;

  case 337: // oqtycon_no_varcon: "(" ":" ")"
#line 1339 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3556 "parser.cc"
    break;

  case 338: // oqtycon_no_varcon: "(" "~" ")"
#line 1340 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3562 "parser.cc"
    break;

  case 339: // qtyconop: qtyconsym
#line 1343 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3568 "parser.cc"
    break;

  case 340: // qtyconop: "`" qtycon "`"
#line 1344 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3574 "parser.cc"
    break;

  case 341: // qtycondoc: qtycon
#line 1346 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3580 "parser.cc"
    break;

  case 342: // qtycon: "QCONID"
#line 1348 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3586 "parser.cc"
    break;

  case 343: // qtycon: tycon
#line 1349 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3592 "parser.cc"
    break;

  case 344: // tycon: "CONID"
#line 1353 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3598 "parser.cc"
    break;

  case 345: // qtyconsym: "QCONSYM"
#line 1355 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3604 "parser.cc"
    break;

  case 346: // qtyconsym: "QVARSYM"
#line 1356 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3610 "parser.cc"
    break;

  case 347: // qtyconsym: tyconsym
#line 1357 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3616 "parser.cc"
    break;

  case 348: // tyconsym: "CONSYM"
#line 1359 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3622 "parser.cc"
    break;

  case 349: // tyconsym: "VARSYM"
#line 1360 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3628 "parser.cc"
    break;

  case 350: // tyconsym: ":"
#line 1361 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3634 "parser.cc"
    break;

  case 351: // tyconsym: "-"
#line 1362 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3640 "parser.cc"
    break;

  case 352: // op: varop
#line 1367 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3646 "parser.cc"
    break;

  case 353: // op: conop
#line 1368 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3652 "parser.cc"
    break;

  case 354: // varop: varsym
#line 1370 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3658 "parser.cc"
    break;

  case 355: // varop: "`" varid "`"
#line 1371 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3664 "parser.cc"
    break;

  case 356: // qop: qvarop
#line 1373 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3670 "parser.cc"
    break;

  case 357: // qop: qconop
#line 1374 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3676 "parser.cc"
    break;

  case 358: // qop: hole_op
#line 1375 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3682 "parser.cc"
    break;

  case 359: // qopm: qvaropm
#line 1377 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3688 "parser.cc"
    break;

  case 360: // qopm: qconop
#line 1378 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3694 "parser.cc"
    break;

  case 361: // qopm: hole_op
#line 1379 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3700 "parser.cc"
    break;

  case 362: // hole_op: "`" "_" "`"
#line 1381 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3706 "parser.cc"
    break;

  case 363: // qvarop: qvarsym
#line 1383 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3712 "parser.cc"
    break;

  case 364: // qvarop: "`" qvarid "`"
#line 1384 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3718 "parser.cc"
    break;

  case 365: // qvaropm: qvarsym_no_minus
#line 1386 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3724 "parser.cc"
    break;

  case 366: // qvaropm: "`" qvarid "`"
#line 1387 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3730 "parser.cc"
    break;

  case 367: // tyvar: tyvarid
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3736 "parser.cc"
    break;

  case 368: // tyvarop: "`" tyvarid "`"
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3742 "parser.cc"
    break;

  case 369: // tyvarid: "VARID"
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3748 "parser.cc"
    break;

  case 370: // tyvarid: special_id
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3754 "parser.cc"
    break;

  case 371: // tyvarid: "unsafe"
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3760 "parser.cc"
    break;

  case 372: // tyvarid: "safe"
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3766 "parser.cc"
    break;

  case 373: // tyvarid: "interruptible"
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3772 "parser.cc"
    break;

  case 374: // var: varid
#line 1402 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3778 "parser.cc"
    break;

  case 375: // var: "(" varsym ")"
#line 1403 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3784 "parser.cc"
    break;

  case 376: // qvar: qvarid
#line 1405 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3790 "parser.cc"
    break;

  case 377: // qvar: "(" varsym ")"
#line 1406 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3796 "parser.cc"
    break;

  case 378: // qvar: "(" qvarsym1 ")"
#line 1407 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3802 "parser.cc"
    break;

  case 379: // qvarid: varid
#line 1409 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3808 "parser.cc"
    break;

  case 380: // qvarid: "QVARID"
#line 1410 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3814 "parser.cc"
    break;

  case 381: // varid: "VARID"
#line 1412 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3820 "parser.cc"
    break;

  case 382: // varid: special_id
#line 1413 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3826 "parser.cc"
    break;

  case 383: // varid: "unsafe"
#line 1414 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3832 "parser.cc"
    break;

  case 384: // varid: "safe"
#line 1415 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3838 "parser.cc"
    break;

  case 385: // varid: "interruptible"
#line 1416 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3844 "parser.cc"
    break;

  case 386: // varid: "forall"
#line 1417 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3850 "parser.cc"
    break;

  case 387: // varid: "family"
#line 1418 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3856 "parser.cc"
    break;

  case 388: // varid: "role"
#line 1419 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3862 "parser.cc"
    break;

  case 389: // qvarsym: varsym
#line 1421 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3868 "parser.cc"
    break;

  case 390: // qvarsym: qvarsym1
#line 1422 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3874 "parser.cc"
    break;

  case 391: // qvarsym_no_minus: varsym_no_minus
#line 1424 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3880 "parser.cc"
    break;

  case 392: // qvarsym_no_minus: qvarsym1
#line 1425 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3886 "parser.cc"
    break;

  case 393: // qvarsym1: "QVARSYM"
#line 1427 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3892 "parser.cc"
    break;

  case 394: // varsym: varsym_no_minus
#line 1429 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3898 "parser.cc"
    break;

  case 395: // varsym: "-"
#line 1430 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3904 "parser.cc"
    break;

  case 396: // varsym_no_minus: "VARSYM"
#line 1432 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3910 "parser.cc"
    break;

  case 397: // varsym_no_minus: special_sym
#line 1433 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3916 "parser.cc"
    break;

  case 398: // special_id: "as"
#line 1435 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3922 "parser.cc"
    break;

  case 399: // special_id: "qualified"
#line 1436 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3928 "parser.cc"
    break;

  case 400: // special_id: "hiding"
#line 1437 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3934 "parser.cc"
    break;

  case 401: // special_id: "export"
#line 1438 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3940 "parser.cc"
    break;

  case 402: // special_id: "label"
#line 1439 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3946 "parser.cc"
    break;

  case 403: // special_id: "dynamic"
#line 1440 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3952 "parser.cc"
    break;

  case 404: // special_id: "stdcall"
#line 1441 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3958 "parser.cc"
    break;

  case 405: // special_id: "ccall"
#line 1442 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3964 "parser.cc"
    break;

  case 406: // special_id: "capi"
#line 1443 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3970 "parser.cc"
    break;

  case 407: // special_id: "prim"
#line 1444 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3976 "parser.cc"
    break;

  case 408: // special_id: "javascript"
#line 1445 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3982 "parser.cc"
    break;

  case 409: // special_id: "group"
#line 1446 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3988 "parser.cc"
    break;

  case 410: // special_id: "stock"
#line 1447 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3994 "parser.cc"
    break;

  case 411: // special_id: "anyclass"
#line 1448 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 4000 "parser.cc"
    break;

  case 412: // special_id: "via"
#line 1449 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 4006 "parser.cc"
    break;

  case 413: // special_id: "unit"
#line 1450 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 4012 "parser.cc"
    break;

  case 414: // special_id: "dependency"
#line 1451 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 4018 "parser.cc"
    break;

  case 415: // special_id: "signature"
#line 1452 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 4024 "parser.cc"
    break;

  case 416: // special_sym: "!"
#line 1454 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 4030 "parser.cc"
    break;

  case 417: // special_sym: "."
#line 1455 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 4036 "parser.cc"
    break;

  case 418: // special_sym: "*"
#line 1456 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 4042 "parser.cc"
    break;

  case 419: // qconid: conid
#line 1460 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4048 "parser.cc"
    break;

  case 420: // qconid: "QCONID"
#line 1461 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4054 "parser.cc"
    break;

  case 421: // conid: "CONID"
#line 1463 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4060 "parser.cc"
    break;

  case 422: // qconsym: consym
#line 1465 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4066 "parser.cc"
    break;

  case 423: // qconsym: "QCONSYM"
#line 1466 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4072 "parser.cc"
    break;

  case 424: // consym: "CONSYM"
#line 1468 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 4078 "parser.cc"
    break;

  case 425: // consym: ":"
#line 1469 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 4084 "parser.cc"
    break;

  case 426: // literal: "CHAR"
#line 1473 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 4090 "parser.cc"
    break;

  case 427: // literal: "STRING"
#line 1474 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 4096 "parser.cc"
    break;

  case 428: // literal: "INTEGER"
#line 1475 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 4102 "parser.cc"
    break;

  case 429: // literal: "RATIONAL"
#line 1476 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 4108 "parser.cc"
    break;

  case 431: // close: error
#line 1484 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 4114 "parser.cc"
    break;

  case 432: // modid: "CONID"
#line 1488 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4120 "parser.cc"
    break;

  case 433: // modid: "QCONID"
#line 1489 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4126 "parser.cc"
    break;

  case 434: // commas: commas ","
#line 1491 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4132 "parser.cc"
    break;

  case 435: // commas: ","
#line 1492 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4138 "parser.cc"
    break;


#line 4142 "parser.cc"

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

    int yyn = yypact_[+yyparser_.yystack_[0].state];
    if (!yy_pact_value_is_default_ (yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative indexes in
           YYCHECK.  In other words, skip the first -YYN actions for
           this state because they are default actions.  */
        int yyxbegin = yyn < 0 ? -yyn : 0;
        // Stay within bounds of both yycheck and yytname.
        int yychecklim = yylast_ - yyn + 1;
        int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
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


  const short parser::yypact_ninf_ = -585;

  const short parser::yytable_ninf_ = -395;

  const short
  parser::yypact_[] =
  {
      36,    37,  -585,    78,  -585,  -585,  -585,  -585,  -585,   115,
     -15,    -6,  -585,    46,   -38,   -38,    23,  -585,  -585,  -585,
    -585,   142,  -585,  -585,  -585,    33,  -585,   111,   121,  3551,
     202,   215,   167,  -585,   612,  -585,    72,  -585,  -585,  -585,
    -585,    37,  -585,    67,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,   155,  -585,  -585,  -585,  -585,
     145,   184,  -585,   194,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,   139,  -585,    37,  -585,   189,  -585,  1981,  3213,
    -585,   200,   207,  1981,  -585,  -585,  -585,   240,   257,  -585,
    3213,  3872,   207,  2707,   213,   182,  3827,   263,  2344,  2707,
    2465,  2707,  1120,   992,    98,  -585,  -585,  -585,  -585,  -585,
    -585,    41,   213,   195,   167,  -585,  -585,  -585,   262,  -585,
    -585,   180,  -585,  2586,  -585,   238,  -585,  -585,  -585,  -585,
    -585,   226,   264,   254,  -585,  -585,  -585,  -585,   229,  -585,
     332,  -585,  -585,   268,   269,  -585,  -585,  -585,  -585,  -585,
     271,  -585,   274,   275,   276,  -585,  -585,  -585,  3551,  3584,
    -585,  -585,  -585,  -585,   376,  -585,   -44,   992,   362,   593,
    -585,  -585,  1981,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  3994,  2910,  2809,   273,  3735,  -585,  -585,  -585,  -585,
    -585,   359,  3643,  -585,   299,  -585,   133,  3213,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  3011,
    1497,  1497,  -585,   278,   312,   315,   316,   321,  3011,   741,
     741,  -585,   384,   319,   320,   102,  4051,   279,   281,  -585,
    -585,  -585,  -585,   -20,  3827,  -585,   325,   166,   -18,   308,
      59,   300,   333,  -585,  -585,  2707,  -585,  -585,  2223,  -585,
    2586,   206,  -585,  -585,  3314,  -585,  -585,  -585,   593,    86,
     313,   309,  -585,  1981,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  2465,  -585,  -585,   -32,   118,   275,   317,   326,   329,
     123,  -585,   140,  3011,  3827,  3827,  -585,   607,   189,   310,
    3213,  3011,  1981,  1739,  3314,  -585,    35,  -585,  -585,  2102,
    -585,  -585,  -585,  -585,  -585,  3643,  -585,  3768,  3994,  2707,
    -585,   331,   334,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,   336,   314,  -585,  -585,   327,    46,    37,    29,   364,
     370,   272,  3011,  1981,  -585,   -19,   349,   340,  -585,  -585,
    -585,  -585,   358,   374,  -585,   363,   331,  -585,    -5,   356,
     334,   134,   152,   361,   365,   257,  -585,  -585,  3213,  3011,
    -585,  -585,   371,   373,   257,   207,  2707,   388,   393,   -21,
    -585,  -585,    44,  -585,   463,  -585,  -585,  -585,  -585,  -585,
    -585,   359,    84,  -585,  -585,   180,    45,  1981,  3011,   382,
     377,   372,   378,   375,   389,   422,  -585,  -585,   428,   399,
     263,   243,   431,  -585,  1981,  -585,  -585,   401,   403,   410,
    1981,  1981,  1739,  1248,  -585,  1248,   722,  -585,  1248,  -585,
    1248,   413,  -585,  -585,  -585,  -585,   451,   449,   458,  3961,
     427,  -585,  -585,  -585,  -585,   -11,   129,  -585,  -585,  -585,
     359,   461,   429,  -585,   430,  -585,  -585,  -585,  -585,  -585,
     444,  -585,   432,   466,    17,  -585,  -585,  -585,  -585,  3584,
    -585,  -585,  -585,   441,  3551,  -585,  -585,  -585,  -585,  1376,
     871,  -585,  -585,  -585,  3011,  3994,  -585,  3994,   479,  -585,
    3011,  -585,  3011,  -585,  3011,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  3011,   384,  -585,  -585,  1981,  -585,  1497,
    -585,  1981,  -585,  -585,   741,  -585,  -585,  -585,  -585,  -585,
     421,   423,   446,  -585,  -585,  -585,  -585,  -585,   448,   345,
     178,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,   439,
    -585,   476,  -585,  -585,  -585,  -585,  -585,  3011,  3011,   443,
     607,  -585,   484,  3011,   527,  -585,   553,  -585,  1981,  1739,
    -585,  -585,  3768,  1248,  3011,   450,  3551,   455,  2707,  -585,
    1618,  -585,   465,   457,  -585,   181,    46,  -585,  -585,  -585,
    -585,  3011,  4110,  -585,  -585,  -585,  -585,   464,   467,  -585,
    -585,  -585,   278,  -585,  -585,  -585,  -585,  -585,  -585,  1739,
    1981,  -585,    49,    73,  -585,  -585,  -585,  -585,  -585,   489,
    -585,  3643,    47,  -585,   553,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,   469,  -585,  -585,  -585,  -585,  1860,  1739,  1981,
    -585,    40,  -585,  -585,  -585,   494,  -585,  -585,   567,  -585,
    -585,  -585,  3011,  -585,  4084,   527,   491,  3359,  -585,  -585,
    -585,  -585,  -585,  -585,  3112,    89,   528,  -585,  -585,  -585,
    -585,  -585,   496,   359,  -585,  -585,  3011,  1981,  -585,  -585,
    -585,  3643,   470,  -585,  3643,  -585,  -585,   475,   482,  -585,
    3213,  -585,  1981,  -585,   483,  -585,  3451,  -585,  3643,  3213,
    -585,  -585,  -585,  -585,  -585
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    39,     0,     2,    39,     4,   432,   433,     8,
       0,    42,     1,     0,     0,     0,    18,    11,    38,    13,
      16,    61,   431,   430,    12,   108,   104,     0,     0,     0,
       0,    45,    40,    15,    14,   107,     0,     6,     7,   398,
     400,     0,   399,     0,   386,   401,   402,   403,   384,   385,
     383,   387,   388,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   415,   414,     0,   381,   344,   380,   342,
       0,    19,    21,    24,    32,    35,   334,   343,    34,   376,
     379,   382,     0,    44,     0,    37,    41,   243,     0,     0,
      86,     0,     0,     0,    54,    55,    56,    81,     0,    87,
       0,     0,     0,     0,   203,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   421,   420,   426,   427,   428,
     429,   203,   203,    52,    59,    62,    63,    64,    94,    66,
     184,    67,   211,   215,   225,   234,   236,   238,   304,   317,
     305,     0,   237,   379,   306,   419,   239,   105,     0,    23,
       0,    33,   331,     0,     0,   395,   416,   418,   417,   396,
       0,   393,     0,     0,     0,   394,   397,    17,     0,    26,
      22,    39,    39,     3,    47,    36,     0,     0,     0,   208,
     216,   209,     0,   372,   373,   371,   350,   119,   351,   118,
     140,   169,     0,     0,     0,     0,   369,   349,   348,   346,
     345,   103,     0,   117,     0,    91,   126,   129,   132,   134,
     138,   326,   135,   339,   347,   139,   136,   367,   370,   152,
     291,   291,   232,   219,     0,     0,     0,     0,     0,    98,
      98,   101,     0,     0,   126,     0,     0,     0,     0,   374,
     354,   233,   224,     0,     0,   204,     0,     0,     0,     0,
       0,   311,   109,   310,   308,     0,   282,   285,     0,   227,
     213,     0,   425,   318,     0,   424,   423,   244,   208,   249,
       0,   250,   360,     0,   361,   359,   365,   392,   391,   321,
     422,   395,   313,   435,     0,     0,   392,     0,   391,   321,
       0,   315,     0,     0,     0,     0,    53,     0,    60,     0,
       0,     0,     0,     0,     0,   186,   103,   191,   357,     0,
     358,   356,   363,   390,   389,     0,   222,   298,     0,     0,
     106,     0,     0,   337,   338,   336,   335,   378,   377,    20,
      31,     0,    27,    29,    30,     0,     0,     0,    49,     0,
       0,     0,     0,     0,   217,     0,     0,   170,   172,   114,
     156,   330,     0,     0,   122,     0,   119,   143,   153,     0,
     339,     0,     0,     0,     0,     0,    72,   141,     0,     0,
     133,   153,     0,   151,     0,     0,     0,   295,     0,     0,
     290,   292,     0,   218,     0,    78,    77,    79,    80,   148,
     111,   103,     0,   187,    97,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   214,   198,     0,     0,
       0,     0,     0,   283,     0,   284,   185,     0,     0,     0,
     245,   251,     0,     0,   242,     0,   246,   240,     0,   241,
       0,   377,   307,   314,   434,   316,     0,     0,     0,     0,
     195,   353,    58,   352,   319,     0,    88,   193,   123,   112,
     103,     0,   260,   262,     0,   189,   190,   212,   223,   301,
       0,   297,   300,   303,     0,   226,   333,   332,    25,     0,
       9,    10,    46,     0,     0,    43,    48,   221,   220,     0,
       0,   231,   207,   210,     0,     0,   142,     0,     0,   145,
       0,   329,     0,   146,     0,   327,   328,   340,   368,   102,
      90,   127,    65,     0,   296,   293,   281,     0,   286,   289,
     287,     0,    76,    99,    96,   100,   229,    73,   375,   355,
      71,    69,     0,   205,   197,   199,   309,   312,     0,     0,
       0,   110,   323,   196,   228,   362,   366,   322,   253,   255,
     259,   244,   257,   256,   248,   247,   202,     0,     0,     0,
       0,    93,     0,     0,   166,    75,   174,   188,     0,     0,
     364,   235,     0,     0,     0,     0,     0,     0,     0,   265,
       0,   278,     0,   267,   271,     0,     0,   266,   173,   113,
     171,     0,     0,   155,   157,   121,   159,     0,   154,   154,
     294,   288,   219,    95,    70,    68,   206,   324,   325,     0,
     252,   115,     0,     0,   320,    57,    92,    89,   156,   160,
     162,     0,     0,    74,   175,   177,   192,   261,   299,   302,
     194,    28,     0,    50,   279,   268,   263,   270,     0,     0,
     272,   103,   276,   264,   120,     0,   147,   144,     0,   258,
     254,   200,     0,   201,     0,   166,     0,   167,   130,   137,
     164,    84,    82,    83,     0,     0,   178,   181,   341,   176,
      51,   269,     0,   103,   274,   275,     0,     0,   116,   165,
     161,     0,     0,   131,     0,   182,   128,   149,     0,   179,
       0,   180,     0,   273,     0,   230,   167,   163,   168,     0,
     183,    85,   277,   158,   150
  };

  const short
  parser::yypgoto_[] =
  {
    -585,  -585,  -585,  -585,  -585,  -585,  -585,    31,  -585,  -585,
    -432,  -585,   424,  -585,  -585,  -585,   128,  -147,  -585,   474,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,   303,  -585,   379,  -585,  -291,  -268,   585,  -585,
    -585,  -323,    43,   287,    60,  -585,  -585,  -149,   212,   -56,
    -585,   -85,  -585,   -95,  -377,  -585,   404,  -584,  -190,   330,
     -77,  -585,   394,     6,  -585,  -513,  -585,  -585,   -12,  -585,
     -36,  -585,  -585,   150,  -585,  -585,    13,   -16,   608,   143,
     397,  -585,   354,  -585,   253,  -585,   -61,  -105,   627,   -25,
    -287,    79,  -585,   -86,   -83,  -585,  -585,   -81,  -585,  -585,
    -585,  -585,    42,  -585,  -585,  -425,  -585,    50,  -585,  -585,
      48,  -585,  -585,   418,  -585,   -79,   459,   169,  -273,  -585,
     119,  -585,  -585,  -585,  -585,   277,  -585,   -89,  -558,   -78,
    -585,   280,   639,  -585,  -585,  -585,   -29,  -585,  -139,  -585,
     146,   588,  -150,  -585,   -67,  -585,  -585,  -453,  -585,   497,
     -91,   -26,  -193,   -13,  -585,  -585,   -17,   -64,   -96,   -87,
    -585,  -177,  -101,   -55,  -211,  -585,  -245,    -7,  -100
  };

  const short
  parser::yydefgoto_[] =
  {
       0,     3,     4,     5,    16,   173,     6,    10,    19,    30,
      70,    71,    72,   170,   331,   332,   333,    73,    74,    86,
      11,    20,    21,    32,    84,   338,   475,   476,   297,   123,
     440,    33,    34,   124,   125,   126,   127,   228,   655,   681,
     128,   555,   201,   300,   392,   231,   232,   366,    27,    36,
     412,   389,   447,   345,   602,   202,   203,   390,   449,   353,
     646,   354,   677,   206,   647,   207,   208,   648,   209,   391,
     678,   372,   359,   488,   583,   587,   556,   609,   610,   611,
     650,   346,   347,   348,   613,   614,   615,   656,   393,   394,
     305,   306,   307,   130,   244,   245,   377,   179,   395,   180,
     181,   384,   182,   133,   134,   135,   136,   284,   285,   270,
     271,   539,   451,   452,   481,   572,   573,   574,   630,   631,
     632,   575,   378,   257,   258,   222,   379,   380,   381,   460,
     461,   462,   137,   138,   251,   252,   139,   140,   441,   272,
     531,   210,   211,    75,   212,   657,   152,    77,   213,   214,
     442,   443,   309,   273,   274,   311,   275,   215,   216,   217,
     141,   142,    79,    80,   312,   276,   277,   314,   165,    81,
     166,   144,   145,   279,   280,   146,    24,     9,   290
  };

  const short
  parser::yytable_[] =
  {
      76,   164,   218,    78,   205,   234,   254,   268,   268,   132,
     238,   322,   367,   218,   292,   233,   278,   288,   253,   482,
     242,   143,   334,   241,   260,   256,   259,   178,   261,   343,
     453,   269,   223,   204,   149,   584,    13,   240,   455,   409,
     607,   473,   567,   352,   358,    22,    22,    22,   163,   287,
     316,   267,   267,   308,   360,   576,   483,     1,   289,   407,
     293,   484,   365,   673,   310,    25,   551,   365,   403,   651,
     371,   418,   268,   339,   499,   492,   427,   174,    12,   508,
     248,   288,   428,   504,   340,    17,   444,   419,   239,   674,
      26,   471,   509,   361,   362,   485,   286,   564,   652,   653,
     349,   308,   673,   493,   673,   218,   218,    18,   218,   404,
     408,   454,   310,   164,   313,   218,   267,   552,   420,   303,
     218,   344,   289,   512,   628,   458,   641,   419,   674,   635,
      29,   485,   218,   483,   622,     2,   474,   510,   262,    76,
      76,   218,    78,    78,   243,   625,    23,    23,    23,   540,
     643,   515,   448,   684,   654,     7,    31,   509,   514,     8,
     286,    35,   313,   642,   421,    67,   363,   282,   426,    69,
     422,   399,   413,   283,   150,   256,   147,   316,   239,   265,
      14,    15,   557,   164,   513,    67,   148,   642,    37,    69,
     308,   584,   155,   156,   157,   260,   654,   514,    38,   158,
     423,   310,   335,   336,   132,   132,   218,    67,   291,   553,
     554,    69,   283,   218,   218,   205,   143,   143,   405,   369,
     501,   159,  -124,   400,   601,   601,   429,   349,   218,    82,
     163,   433,   430,   240,   153,   577,   465,   434,   171,    83,
     172,   450,   495,   154,   204,   155,   156,   157,   434,   448,
     435,   313,   158,   167,   434,   218,   155,   156,   157,   262,
     301,   302,   496,   158,   303,   628,   434,   629,   437,   438,
     155,   156,   157,   234,   159,   160,   343,   158,   161,   162,
      85,   218,   218,   500,   457,   159,   617,   302,   598,   161,
     303,   463,   283,   506,   686,   304,   505,   688,   168,   159,
     265,   169,   175,   161,   266,   239,   220,   219,   221,   254,
     246,   218,   224,   225,   226,   227,   243,   308,   268,   668,
     268,   253,   334,   268,   296,   268,   639,   278,   310,   278,
     472,   633,   278,   299,   278,   578,   516,   317,   549,   444,
     318,   585,   542,   586,   543,   588,   528,   544,   308,   545,
     529,   319,   530,   534,   589,   453,   229,   320,   230,   310,
     538,    67,   541,   664,   267,    69,   249,   267,  -374,   267,
     250,   479,   114,   480,   294,   295,   323,   324,   313,   325,
     337,   115,   326,   327,   328,   341,   365,   283,   368,   385,
     322,   383,   386,   387,   579,   683,   349,   218,   388,   397,
     398,   218,   406,   218,   586,   218,   369,   218,   401,   313,
     402,   186,   263,   411,   410,   448,   218,   424,   571,   571,
     321,   649,   188,   425,   186,   431,   400,   470,   469,   361,
     362,   355,   634,   321,  -394,   188,   444,   432,   445,   466,
      76,   477,   467,    78,   468,    76,   590,   478,    78,   486,
     592,   197,   198,   597,   487,   199,   200,   649,   268,   283,
     218,   218,   489,   490,   197,   198,   218,   278,   199,   200,
     494,   491,   239,  -280,   239,   444,   497,   218,   507,   502,
     498,   649,   619,    39,   649,   624,   240,   503,   511,   132,
     518,    40,   519,   523,   218,   218,   649,   616,   649,   524,
     520,   143,   267,    42,   522,   525,   521,   526,   533,   571,
      45,    46,    47,   183,   184,   185,   535,   586,   536,    53,
      54,    55,    56,    57,   218,   537,    58,  -375,   546,   547,
      59,    60,    61,    62,    63,    64,   463,    76,   548,   640,
      78,   550,   558,   559,   561,   560,   562,   563,   566,   594,
     596,   595,   351,   599,   600,   218,   608,   218,   604,   234,
     218,   606,   612,   623,   621,   626,   571,   218,   663,   676,
     627,   549,   636,   645,   666,   637,   581,   660,   667,   218,
     671,   680,   682,   658,   218,   234,   582,   218,   115,   689,
     690,   693,   329,   218,   234,   691,   196,   565,   298,   218,
      28,   218,   218,   446,   676,   464,   685,   620,   603,   396,
     517,   370,   694,   373,   644,    87,    39,    88,    89,    90,
      91,   692,    92,   436,    40,    93,   658,   659,    94,    95,
      96,    97,    98,   670,    99,   687,    42,   580,   100,   679,
     101,    44,   129,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   593,   416,    58,
     456,   131,   103,    59,    60,    61,    62,    63,    64,   104,
     662,   638,   262,   342,   105,   106,   415,   661,   591,   665,
     382,   618,   151,   155,   156,   157,   262,   527,   107,   237,
     158,   532,   364,     0,   108,     0,   605,   155,   156,   157,
     109,     0,   110,   111,   158,     0,     0,     0,   304,     0,
       0,     0,   159,   265,     0,   112,   161,   266,     0,   113,
       0,   114,   439,     0,     0,     0,   159,   265,     0,    66,
     115,     0,     0,    68,   116,     0,     0,     0,     0,   117,
     118,   119,   120,     0,    87,    39,    88,     0,     0,   121,
     122,    92,     0,    40,    93,     0,     0,    94,    95,    96,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,   104,     0,
       0,   262,     0,   105,   106,     0,     0,     0,     0,     0,
       0,     0,   155,   156,   157,     0,     0,   107,     0,   158,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,   111,     0,     0,     0,     0,   304,     0,     0,
       0,   159,   265,     0,   112,   161,   266,     0,   113,     0,
     114,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,     0,     0,     0,   117,   118,
     119,   120,    22,     0,    87,    39,    88,     0,   121,   122,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,   176,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,   568,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    23,   112,     0,     0,     0,   177,     0,
     114,     0,     0,     0,   570,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,   176,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   262,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   281,   156,   157,     0,     0,     0,     0,   158,
       0,     0,     0,     0,     0,   112,     0,     0,     0,   177,
     282,   114,     0,     0,     0,     0,   283,   264,     0,    66,
     115,   159,   265,    68,   116,   161,   266,     0,     0,   117,
     118,   119,   120,    87,    39,    88,     0,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,   176,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   262,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
     110,   156,   157,     0,     0,     0,     0,   158,     0,     0,
       0,     0,     0,   112,   263,     0,     0,   177,     0,   114,
       0,     0,     0,     0,     0,   264,     0,    66,   115,   159,
     265,    68,   116,   161,   266,     0,     0,   117,   118,   119,
     120,    87,    39,    88,     0,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     105,   176,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   262,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,   110,   156,
     157,     0,     0,     0,     0,   158,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   177,     0,   114,     0,     0,
       0,     0,     0,   264,     0,    66,   115,   159,   265,    68,
     116,   161,   266,     0,     0,   117,   118,   119,   120,    87,
      39,    88,     0,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,   103,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,   105,   176,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,   110,   568,     0,     0,
       0,     0,     0,     0,     0,     0,   569,     0,     0,   112,
       0,     0,     0,   177,     0,   114,     0,     0,     0,   570,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
      87,    39,    88,   117,   118,   119,   120,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,   374,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,     0,   375,    58,     0,     0,   103,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,   105,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,   110,   376,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,     0,   177,     0,   114,     0,     0,     0,
       0,     0,     0,     0,    66,   115,     0,     0,    68,   116,
       0,    87,    39,    88,   117,   118,   119,   120,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,   103,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
     105,   176,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,   109,     0,   110,   568,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,     0,     0,   177,     0,   114,     0,     0,
       0,   570,     0,     0,     0,    66,   115,     0,     0,    68,
     116,     0,    87,    39,    88,   117,   118,   119,   120,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,   374,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,   103,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,   105,   176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   109,     0,   110,
     376,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   112,     0,     0,     0,   177,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    66,   115,     0,     0,
      68,   116,     0,    87,    39,    88,   117,   118,   119,   120,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
     103,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,   105,   176,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,     0,   109,     0,
     110,   568,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   112,     0,     0,     0,   177,     0,   114,
       0,     0,     0,     0,     0,     0,     0,    66,   115,     0,
       0,    68,   116,     0,    87,    39,    88,   117,   118,   119,
     120,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,   103,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,   105,   176,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   109,
       0,   110,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,   177,     0,
     114,     0,     0,     0,     0,     0,     0,     0,    66,   115,
       0,     0,    68,   116,     0,    87,    39,    88,   117,   118,
     119,   120,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,   103,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
     109,     0,   110,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,   177,
       0,   114,     0,     0,     0,     0,     0,     0,     0,    66,
     115,     0,     0,    68,   116,     0,    87,    39,    88,   117,
     118,   119,   120,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,   414,
       0,   109,     0,     0,   255,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,     0,     0,
     177,     0,   114,     0,     0,     0,     0,     0,     0,     0,
      66,   115,     0,     0,    68,   116,     0,    87,    39,    88,
     117,   118,   119,   120,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   109,     0,     0,   255,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
       0,   177,     0,   114,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,    87,    39,
      88,   117,   118,   119,   120,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,   103,    59,    60,    61,    62,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,     0,
       0,     0,   177,     0,   114,     0,     0,     0,     0,     0,
       0,     0,    66,   115,     0,     0,    68,   116,     0,    87,
      39,    88,   117,   118,   119,   120,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,    59,    60,    61,
      62,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   109,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   315,     0,     0,     0,     0,   112,
       0,     0,     0,   177,     0,   114,     0,     0,     0,     0,
       0,     0,     0,    66,   115,     0,     0,    68,   116,     0,
      87,    39,    88,   117,   118,   119,   120,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,    59,    60,
      61,    62,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,     0,     0,    39,   177,     0,   114,     0,     0,     0,
       0,    40,     0,     0,    66,   115,     0,     0,    68,   116,
       0,     0,     0,    42,   117,   118,   119,   120,   350,     0,
      45,    46,    47,   183,   184,   185,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   186,     0,
       0,     0,     0,     0,     0,   355,     0,   356,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,   191,     0,
       0,     0,   192,     0,    39,     0,   193,   357,   194,     0,
       0,     0,    40,   283,   195,     0,   196,    67,   197,   198,
       0,    69,   199,   200,    42,     0,     0,     0,     0,   350,
       0,    45,    46,    47,   183,   184,   185,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   186,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,   191,
       0,     0,     0,   192,   351,    39,     0,   193,     0,   194,
       0,     0,     0,    40,     0,   195,     0,   196,    67,   197,
     198,     0,    69,   199,   200,    42,     0,     0,     0,     0,
     350,     0,    45,    46,    47,   183,   184,   185,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     186,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,   192,     0,    39,     0,   193,     0,
     194,     0,     0,     0,    40,     0,   195,     0,   196,    67,
     197,   198,     0,    69,   199,   200,    42,     0,     0,     0,
       0,     0,     0,    45,    46,    47,   183,   184,   185,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,     0,     0,     0,     0,     0,     0,     0,     0,
     187,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,   191,     0,     0,     0,   192,     0,    39,     0,   193,
     675,   194,     0,     0,     0,    40,     0,   195,     0,   196,
      67,   197,   198,     0,    69,   199,   200,    42,     0,     0,
       0,     0,     0,     0,    45,    46,    47,   183,   184,   185,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    59,    60,    61,    62,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   186,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,   191,     0,     0,     0,   192,   417,    39,     0,
     193,     0,   194,     0,     0,     0,    40,     0,   195,     0,
     196,    67,   197,   198,     0,    69,   199,   200,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,    39,     0,    59,    60,    61,    62,    63,
      64,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,     0,     0,     0,     0,     0,
      45,    46,    47,   183,   184,   185,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
      59,    60,    61,    62,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    66,   115,     0,     0,    68,   116,     0,   262,     0,
       0,     0,     0,     0,     0,     0,     0,   187,  -125,     0,
     189,   190,     0,     0,     0,    39,     0,     0,   191,     0,
       0,     0,   192,    40,     0,     0,   193,     0,   194,     0,
       0,     0,     0,     0,   672,    42,   196,    67,     0,   265,
       0,    69,    45,    46,    47,   183,   184,   185,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     262,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,     0,   189,   190,     0,     0,     0,     0,     0,     0,
     191,     0,     0,     0,   192,    39,     0,     0,   193,     0,
     194,     0,     0,    40,     0,     0,   672,     0,   196,    67,
       0,   265,    41,    69,     0,    42,     0,    43,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,    39,    51,
      52,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    42,     0,
      43,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,     0,     0,     0,     0,    40,     0,     0,    65,     0,
       0,     0,   330,     0,     0,     0,     0,    42,    66,    67,
       0,     0,    68,    69,    45,    46,    47,   183,   184,   185,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,    65,     0,     0,    59,    60,    61,    62,    63,    64,
       0,    66,    67,     0,     0,    68,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   187,     0,     0,   189,   190,     0,     0,     0,    39,
       0,     0,   191,     0,     0,     0,   192,    40,     0,     0,
     193,     0,   194,     0,     0,     0,     0,     0,     0,    42,
     196,    67,     0,     0,     0,    69,    45,    46,    47,   183,
     184,   185,    39,     0,     0,    53,    54,    55,    56,    57,
      40,     0,    58,     0,     0,     0,    59,    60,    61,    62,
      63,    64,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    59,
      60,    61,    62,    63,    64,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,     0,     0,   459,     0,     0,     0,
       0,    42,   196,    67,     0,     0,    44,    69,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,   247,    39,     0,    59,    60,
      61,    62,    63,    64,    40,    66,     0,     0,     0,    68,
       0,     0,     0,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,    59,    60,    61,    62,    63,    64,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    66,     0,     0,     0,    68,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   155,   156,   157,    39,     0,     0,     0,   158,
       0,     0,     0,    40,     0,     0,     0,     0,     0,   235,
       0,     0,     0,     0,     0,    42,     0,   236,     0,    66,
      44,   159,    45,    46,    47,    48,    49,    50,    39,    51,
      52,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,     0,    59,    60,    61,    62,    63,    64,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    59,    60,    61,    62,    63,
      64,     0,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,    66,   115,
      44,     0,    45,    46,    47,    48,    49,    50,    39,    51,
      52,    53,    54,    55,    56,    57,    40,     0,    58,     0,
       0,   235,    59,    60,    61,    62,    63,    64,    42,     0,
       0,    66,     0,     0,    39,    45,    46,    47,   183,   184,
     185,     0,    40,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,    42,    59,    60,    61,    62,    63,
      64,    45,    46,    47,   183,   184,   185,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    59,    60,    61,    62,    63,    64,     0,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   669,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   582,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   196,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   196
  };

  const short
  parser::yycheck_[] =
  {
      29,    65,    89,    29,    89,   100,   107,   112,   113,    34,
     101,   150,   202,   100,   114,   100,   112,   113,   107,   342,
     103,    34,   169,   102,   110,   108,   109,    88,   111,   179,
     303,   112,    93,    89,    41,   488,     5,   101,   306,   250,
     553,    12,   474,   192,   193,     1,     1,     1,    65,   113,
     133,   112,   113,   131,   193,   480,   343,    21,   113,    77,
      19,    80,    27,   647,   131,   103,    77,    27,    88,    22,
     219,   264,   177,   117,   365,    80,   108,    84,     0,   100,
     106,   177,   114,   374,   128,   100,   297,   264,   101,   647,
     128,   336,   113,   193,   194,   114,   113,    80,    51,    52,
     191,   179,   686,   108,   688,   192,   193,   113,   195,   129,
     128,   304,   179,   177,   131,   202,   177,   128,   268,    84,
     207,   182,   177,   391,    84,   315,    77,   304,   686,   582,
     107,   114,   219,   420,   566,    99,   107,   382,    79,   168,
     169,   228,   168,   169,   103,   570,   102,   102,   102,   422,
      77,   396,   301,   666,   107,   118,    14,   113,   113,   122,
     177,   128,   179,   114,    78,   118,   195,   108,   273,   122,
      84,   235,   255,   114,   107,   258,   104,   260,   191,   120,
      65,    66,   450,   247,   100,   118,   114,   114,    77,   122,
     268,   644,    90,    91,    92,   281,   107,   113,    77,    97,
     114,   268,   171,   172,   229,   230,   293,   118,   110,    80,
      81,   122,   114,   300,   301,   300,   229,   230,   244,    86,
     369,   119,    89,   236,   547,   548,   108,   318,   315,    27,
     247,   108,   114,   297,    79,   480,   319,   114,    99,    24,
     101,   302,   108,    88,   300,    90,    91,    92,   114,   398,
     110,   268,    97,   108,   114,   342,    90,    91,    92,    79,
      80,    81,   110,    97,    84,    84,   114,    86,   294,   295,
      90,    91,    92,   368,   119,   120,   426,    97,   123,   124,
     113,   368,   369,   368,   309,   119,   559,    81,   110,   123,
      84,   317,   114,   376,   671,   115,   375,   674,   114,   119,
     120,   107,   113,   123,   124,   318,    99,   107,   101,   410,
     128,   398,    72,    73,    74,    75,   103,   395,   423,   642,
     425,   410,   469,   428,   129,   430,   599,   423,   395,   425,
     337,   576,   428,    71,   430,   484,   397,    99,   439,   550,
     114,   490,   423,   492,   425,   494,   103,   428,   426,   430,
     107,    87,   109,   414,   503,   628,    99,   128,   101,   426,
     421,   118,   423,   631,   425,   122,   103,   428,   114,   430,
     107,    99,   109,   101,   121,   122,   108,   108,   395,   108,
       4,   118,   108,   108,   108,    23,    27,   114,    89,    77,
     529,   113,    77,    77,   485,   663,   487,   484,    77,    15,
      81,   488,    77,   490,   553,   492,    86,   494,   129,   426,
     129,    79,   104,    80,   114,   564,   503,   104,   479,   480,
      88,   611,    90,   114,    79,   108,   439,   100,   114,   529,
     530,    86,   581,    88,   108,    90,   647,   108,   128,   108,
     469,    77,   108,   469,   108,   474,   507,    77,   474,   100,
     511,   119,   120,   108,   114,   123,   124,   647,   563,   114,
     547,   548,   104,    89,   119,   120,   553,   563,   123,   124,
     114,   108,   485,    85,   487,   686,   115,   564,    85,   108,
     115,   671,   563,     4,   674,   568,   550,   114,    25,   514,
     108,    12,   115,   104,   581,   582,   686,   558,   688,    77,
     128,   514,   563,    24,   129,    77,   128,   108,    77,   570,
      31,    32,    33,    34,    35,    36,   115,   666,   115,    40,
      41,    42,    43,    44,   611,   115,    47,   114,    77,    80,
      51,    52,    53,    54,    55,    56,   562,   566,    80,   600,
     566,   114,    81,   114,   100,   115,   114,    81,   107,   128,
     104,   128,   104,   114,    78,   642,    29,   644,   115,   654,
     647,    77,     9,   108,   114,   100,   627,   654,   629,   654,
     113,   672,   108,    84,    80,   108,    97,   108,    11,   666,
      89,    53,    86,   612,   671,   680,   107,   674,   118,   114,
     108,   108,   168,   680,   689,   680,   117,   469,   124,   686,
      15,   688,   689,   300,   689,   318,   667,   564,   548,   230,
     398,   207,   689,   219,   608,     3,     4,     5,     6,     7,
       8,   682,    10,   293,    12,    13,   655,   614,    16,    17,
      18,    19,    20,   645,    22,   671,    24,   487,    26,   655,
      28,    29,    34,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,   514,   261,    47,
     306,    34,    50,    51,    52,    53,    54,    55,    56,    57,
     628,   592,    79,    80,    62,    63,   258,   627,   509,   631,
     221,   562,    43,    90,    91,    92,    79,   410,    76,   101,
      97,   411,   195,    -1,    82,    -1,   550,    90,    91,    92,
      88,    -1,    90,    91,    97,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,   120,    -1,   103,   123,   124,    -1,   107,
      -1,   109,   115,    -1,    -1,    -1,   119,   120,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,     3,     4,     5,    -1,    -1,   137,
     138,    10,    -1,    12,    13,    -1,    -1,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    79,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,    -1,    -1,    76,    -1,    97,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,   120,    -1,   103,   123,   124,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,     1,    -1,     3,     4,     5,    -1,   137,   138,
      -1,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,   113,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
     108,   109,    -1,    -1,    -1,    -1,   114,   115,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,    -1,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   103,   104,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,   115,    -1,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,   115,    -1,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,   113,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    47,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,     3,     4,     5,   127,   128,   129,   130,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,   113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,     3,     4,     5,   127,   128,   129,   130,    10,
      -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,     3,     4,     5,   127,   128,   129,   130,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,     3,     4,     5,   127,   128,   129,
     130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,     3,     4,     5,   127,   128,
     129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,     3,     4,     5,   127,
     128,   129,   130,    10,    -1,    12,    13,    -1,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    86,
      -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,     3,     4,     5,
     127,   128,   129,   130,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,     4,
       5,   127,   128,   129,   130,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,     3,
       4,     5,   127,   128,   129,   130,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
       3,     4,     5,   127,   128,   129,   130,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,     4,   107,    -1,   109,    -1,    -1,    -1,
      -1,    12,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    24,   127,   128,   129,   130,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,    -1,     4,    -1,   107,   108,   109,    -1,
      -1,    -1,    12,   114,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,   104,     4,    -1,   107,    -1,   109,
      -1,    -1,    -1,    12,    -1,   115,    -1,   117,   118,   119,
     120,    -1,   122,   123,   124,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,     4,    -1,   107,    -1,
     109,    -1,    -1,    -1,    12,    -1,   115,    -1,   117,   118,
     119,   120,    -1,   122,   123,   124,    24,    -1,    -1,    -1,
      -1,    -1,    -1,    31,    32,    33,    34,    35,    36,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,     4,    -1,   107,
     108,   109,    -1,    -1,    -1,    12,    -1,   115,    -1,   117,
     118,   119,   120,    -1,   122,   123,   124,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,     3,     4,    -1,
     107,    -1,   109,    -1,    -1,    -1,    12,    -1,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,     4,    -1,    51,    52,    53,    54,    55,
      56,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,
      91,    92,    -1,    -1,    -1,     4,    -1,    -1,    99,    -1,
      -1,    -1,   103,    12,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,   115,    24,   117,   118,    -1,   120,
      -1,   122,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,     4,    -1,    -1,   107,    -1,
     109,    -1,    -1,    12,    -1,    -1,   115,    -1,   117,   118,
      -1,   120,    21,   122,    -1,    24,    -1,    26,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,     4,    38,
      39,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
      26,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,   107,    -1,
      -1,    -1,    78,    -1,    -1,    -1,    -1,    24,   117,   118,
      -1,    -1,   121,   122,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,   107,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,     4,
      -1,    -1,    99,    -1,    -1,    -1,   103,    12,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    24,
     117,   118,    -1,    -1,    -1,   122,    31,    32,    33,    34,
      35,    36,     4,    -1,    -1,    40,    41,    42,    43,    44,
      12,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    24,   117,   118,    -1,    -1,    29,   122,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,   107,     4,    -1,    51,    52,
      53,    54,    55,    56,    12,   117,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,    -1,    -1,    -1,   121,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,     4,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    24,    -1,   115,    -1,   117,
      29,   119,    31,    32,    33,    34,    35,    36,     4,    38,
      39,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,   117,   118,
      29,    -1,    31,    32,    33,    34,    35,    36,     4,    38,
      39,    40,    41,    42,    43,    44,    12,    -1,    47,    -1,
      -1,   107,    51,    52,    53,    54,    55,    56,    24,    -1,
      -1,   117,    -1,    -1,     4,    31,    32,    33,    34,    35,
      36,    -1,    12,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    24,    51,    52,    53,    54,    55,
      56,    31,    32,    33,    34,    35,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117
  };

  const short
  parser::yystos_[] =
  {
       0,    21,    99,   140,   141,   142,   145,   118,   122,   316,
     146,   159,     0,   146,    65,    66,   143,   100,   113,   147,
     160,   161,     1,   102,   315,   103,   128,   187,   187,   107,
     148,    14,   162,   170,   171,   128,   188,    77,    77,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    51,
      52,    53,    54,    55,    56,   107,   117,   118,   121,   122,
     149,   150,   151,   156,   157,   282,   285,   286,   300,   301,
     302,   308,    27,    24,   163,   113,   158,     3,     5,     6,
       7,     8,    10,    13,    16,    17,    18,    19,    20,    22,
      26,    28,    37,    50,    57,    62,    63,    76,    82,    88,
      90,    91,   103,   107,   109,   118,   122,   127,   128,   129,
     130,   137,   138,   168,   172,   173,   174,   175,   179,   227,
     232,   237,   238,   242,   243,   244,   245,   271,   272,   275,
     276,   299,   300,   302,   310,   311,   314,   104,   114,   316,
     107,   281,   285,    79,    88,    90,    91,    92,    97,   119,
     120,   123,   124,   305,   306,   307,   309,   108,   114,   107,
     152,    99,   101,   144,   316,   113,    63,   107,   235,   236,
     238,   239,   241,    34,    35,    36,    79,    88,    90,    91,
      92,    99,   103,   107,   109,   115,   117,   119,   120,   123,
     124,   181,   194,   195,   198,   200,   202,   204,   205,   207,
     280,   281,   283,   287,   288,   296,   297,   298,   308,   107,
      99,   101,   264,   235,    72,    73,    74,    75,   176,    99,
     101,   184,   185,   200,   202,   107,   115,   290,   299,   302,
     306,   264,   243,   103,   233,   234,   128,   107,   300,   103,
     107,   273,   274,   276,   311,    91,   243,   262,   263,   243,
     242,   243,    79,   104,   115,   120,   124,   235,   236,   246,
     248,   249,   278,   292,   293,   295,   304,   305,   307,   312,
     313,    90,   108,   114,   246,   247,   305,   306,   307,   312,
     317,   110,   317,    19,   233,   233,   129,   167,   158,    71,
     182,    80,    81,    84,   115,   229,   230,   231,   278,   291,
     293,   294,   303,   305,   306,    98,   243,    99,   114,    87,
     128,    88,   287,   108,   108,   108,   108,   108,   108,   151,
      78,   153,   154,   155,   156,   146,   146,     4,   164,   117,
     128,    23,    80,   291,   235,   192,   220,   221,   222,   299,
      29,   104,   196,   198,   200,    86,    88,   108,   196,   211,
     287,   317,   317,   285,   298,    27,   186,   207,    89,    86,
     205,   196,   210,   211,    20,    46,    91,   235,   261,   265,
     266,   267,   265,   113,   240,    77,    77,    77,    77,   190,
     196,   208,   183,   227,   228,   237,   183,    15,    81,   306,
     302,   129,   129,    88,   129,   300,    77,    77,   128,   313,
     114,    80,   189,   243,    86,   262,   229,     3,   301,   310,
     291,    78,    84,   114,   104,   114,   236,   108,   114,   108,
     114,   108,   108,   108,   114,   110,   208,   300,   300,   115,
     169,   277,   289,   290,   313,   128,   181,   191,   196,   197,
     235,   251,   252,   267,   301,   186,   231,   238,   207,    78,
     268,   269,   270,   300,   192,   243,   108,   108,   108,   114,
     100,   315,   316,    12,   107,   165,   166,    77,    77,    99,
     101,   253,   190,   239,    80,   114,   100,   114,   212,   104,
      89,   108,    80,   108,   114,   108,   110,   115,   115,   185,
     200,   196,   108,   114,   185,   264,   243,    85,   100,   113,
     315,    25,   186,   100,   113,   315,   235,   197,   108,   115,
     128,   128,   129,   104,    77,    77,   108,   274,   103,   107,
     109,   279,   280,    77,   235,   115,   115,   115,   235,   250,
     267,   235,   246,   246,   246,   246,    77,    80,    80,   311,
     114,    77,   128,    80,    81,   180,   215,   186,    81,   114,
     115,   100,   114,    81,    80,   155,   107,   149,    91,   100,
     113,   235,   254,   255,   256,   260,   254,   315,   196,   299,
     222,    97,   107,   213,   296,   196,   196,   214,   196,   196,
     235,   266,   235,   228,   128,   128,   104,   108,   110,   114,
      78,   190,   193,   193,   115,   289,    77,   214,    29,   216,
     217,   218,     9,   223,   224,   225,   235,   267,   269,   246,
     191,   114,   149,   108,   243,   254,   100,   113,    84,    86,
     257,   258,   259,   315,   196,   296,   108,   108,   240,   267,
     235,    77,   114,    77,   212,    84,   199,   203,   206,   207,
     219,    22,    51,    52,   107,   177,   226,   284,   285,   225,
     108,   256,   251,   235,   186,   259,    80,    11,   190,    97,
     217,    89,   115,   206,   277,   108,   200,   201,   209,   226,
      53,   178,    86,   186,   214,   235,   203,   219,   203,   114,
     108,   200,   235,   108,   209
  };

  const short
  parser::yyr1_[] =
  {
       0,   139,   140,   141,   141,   142,   143,   143,   143,   144,
     144,   145,   145,   146,   147,   147,   147,   148,   148,   149,
     150,   150,   151,   151,   152,   152,   153,   153,   154,   154,
     155,   155,   156,   156,   157,   157,   158,   158,   159,   159,
     160,   161,   161,   162,   163,   163,   164,   164,   165,   165,
     166,   166,   167,   167,   168,   168,   168,   169,   169,   170,
     171,   171,   172,   172,   172,   172,   172,   172,   172,   172,
     172,   172,   173,   174,   174,   174,   175,   176,   176,   176,
     176,   176,   177,   177,   177,   178,   179,   179,   180,   180,
     181,   181,   182,   182,   182,   183,   183,   183,   183,   184,
     184,   185,   186,   186,   187,   187,   188,   188,   188,   189,
     189,   190,   191,   192,   192,   193,   193,   194,   195,   195,
     196,   196,   196,   197,   198,   199,   200,   200,   201,   202,
     203,   203,   204,   204,   205,   205,   205,   206,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   208,   209,
     209,   210,   210,   211,   211,   212,   212,   213,   213,   214,
     215,   216,   216,   217,   217,   218,   218,   219,   219,   220,
     220,   221,   221,   222,   223,   223,   224,   224,   225,   225,
     225,   226,   226,   226,   227,   227,   227,   228,   229,   229,
     230,   230,   231,   232,   232,   232,   232,   232,   232,   232,
     232,   232,   232,   233,   233,   234,   234,   235,   235,   236,
     236,   237,   237,   238,   238,   238,   239,   239,   240,   240,
     241,   241,   242,   242,   242,   242,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   244,   244,   245,   245,   245,
     245,   245,   245,   245,   246,   246,   246,   247,   247,   248,
     248,   248,   248,   248,   248,   248,   249,   249,   250,   250,
     251,   252,   252,   253,   253,   253,   253,   254,   254,   255,
     255,   255,   256,   257,   257,   258,   258,   259,   260,   260,
     261,   261,   262,   262,   263,   263,   264,   264,   265,   265,
     265,   265,   266,   266,   267,   267,   267,   268,   268,   269,
     269,   269,   270,   270,   271,   271,   272,   272,   273,   273,
     273,   274,   274,   275,   275,   275,   275,   276,   276,   277,
     277,   278,   278,   279,   279,   279,   280,   280,   280,   280,
     280,   281,   281,   281,   282,   282,   282,   282,   282,   283,
     283,   284,   285,   285,   286,   287,   287,   287,   288,   288,
     288,   288,   289,   289,   290,   290,   291,   291,   291,   292,
     292,   292,   293,   294,   294,   295,   295,   296,   297,   298,
     298,   298,   298,   298,   299,   299,   300,   300,   300,   301,
     301,   302,   302,   302,   302,   302,   302,   302,   302,   303,
     303,   304,   304,   305,   306,   306,   307,   307,   308,   308,
     308,   308,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   308,   308,   308,   308,   308,   309,   309,   309,   310,
     310,   311,   312,   312,   313,   313,   314,   314,   314,   314,
     315,   315,   316,   316,   317,   317
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     0,     3,     0,     1,     4,     1,
       1,     1,     1,     2,     1,     1,     2,     1,     2,     0,
       2,     3,     0,     5,     1,     0,     2,     0,     1,     0,
       3,     4,     0,     1,     1,     1,     1,     3,     1,     2,
       3,     0,     1,     1,     1,     4,     1,     1,     5,     4,
       5,     4,     3,     4,     5,     4,     4,     2,     2,     2,
       2,     0,     1,     1,     1,     2,     1,     1,     0,     2,
       3,     1,     4,     3,     0,     3,     2,     1,     0,     3,
       3,     1,     2,     0,     1,     3,     3,     1,     0,     0,
       2,     1,     1,     3,     1,     1,     3,     1,     1,     1,
       4,     3,     1,     1,     1,     1,     1,     3,     1,     1,
       1,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     2,     3,     2,     5,     3,     3,     5,     1,     1,
       3,     1,     0,     1,     3,     2,     0,     1,     5,     1,
       2,     3,     1,     4,     2,     3,     0,     1,     3,     0,
       1,     3,     1,     3,     0,     1,     2,     1,     2,     3,
       3,     1,     2,     3,     1,     3,     2,     1,     3,     2,
       2,     1,     4,     3,     5,     3,     4,     4,     3,     4,
       6,     6,     4,     0,     1,     3,     4,     3,     1,     1,
       3,     1,     3,     2,     3,     1,     1,     2,     1,     0,
       3,     3,     2,     3,     2,     1,     3,     2,     4,     4,
       8,     4,     2,     2,     1,     4,     1,     1,     1,     1,
       3,     3,     3,     1,     1,     2,     2,     3,     3,     1,
       1,     2,     4,     3,     5,     3,     3,     3,     3,     1,
       1,     3,     1,     3,     3,     2,     2,     1,     2,     3,
       2,     1,     2,     3,     2,     2,     1,     4,     1,     2,
       1,     2,     1,     2,     2,     1,     3,     3,     3,     2,
       1,     0,     1,     2,     3,     1,     2,     1,     0,     3,
       1,     1,     3,     1,     1,     1,     1,     3,     1,     3,
       1,     1,     3,     2,     3,     2,     3,     1,     2,     1,
       3,     1,     3,     1,     2,     2,     1,     3,     3,     3,
       2,     1,     3,     3,     1,     3,     3,     3,     3,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     1,     3,     1,     3,     3,     1,
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
  "\"type\"", "\"where\"", "\"builtin\"", "\"forall\"", "\"foreign\"",
  "\"export\"", "\"label\"", "\"dynamic\"", "\"safe\"",
  "\"interruptible\"", "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"",
  "\"stdcall\"", "\"ccall\"", "\"capi\"", "\"prim\"", "\"javascript\"",
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"static\"",
  "\"stock\"", "\"anyclass\"", "\"via\"", "\"unit\"", "\"signature\"",
  "\"dependency\"", "\"{-# INLINE\"", "\"{-# SPECIALIZE\"",
  "\"{-# SPECIALIZE_INLINE\"", "\"{-# SOURCE\"", "\"{-# RULES\"",
  "\"{-# CORE\"", "\"{-# SCC\"", "\"{-# GENERATED\"", "\"{-# DEPRECATED\"",
  "\"{-# WARNING\"", "\"{-# UNPACK\"", "\"{-# NOUNPACK\"", "\"{-# ANN\"",
  "\"{-# MINIMAL\"", "\"{-# CTYPE\"", "\"{-# OVERLAPPING\"",
  "\"{-# OVERLAPPABLE\"", "\"{-# OVERLAPS\"", "\"{-# INCOHERENT\"",
  "\"{-# COMPLETE\"", "\"#-}\"", "\"..\"", "\":\"", "\"::\"", "\"=\"",
  "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"", "\"@\"", "\"~\"",
  "\"=>\"", "\"-\"", "\"!\"", "\"*\"", "\"-<\"", "\">-\"", "\"-<<\"",
  "\">>-\"", "\".\"", "\"TYPEAPP\"", "\"{\"", "\"}\"", "\"vocurly\"",
  "\"vccurly\"", "\"[\"", "\"]\"", "\"[:\"", "\":]\"", "\"(\"", "\")\"",
  "\"(#\"", "\"#)\"", "\"(|\"", "\"|)\"", "\";\"", "\",\"", "\"`\"",
  "\"'\"", "\"VARID\"", "\"CONID\"", "\"VARSYM\"", "\"CONSYM\"",
  "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"", "\"QCONSYM\"",
  "\"IPDUPVARID\"", "\"LABELVARID\"", "\"CHAR\"", "\"STRING\"",
  "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"", "\"PRIMSTRING\"",
  "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"", "\"PRIMDOUBLE\"",
  "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"", "$accept", "unit",
  "module", "missing_module_keyword", "maybemodwarning", "body", "body2",
  "top", "top1", "maybeexports", "exportlist", "exportlist1", "export",
  "export_subspec", "qcnames", "qcnames1", "qcname_ext_w_wildcard",
  "qcname_ext", "qcname", "semis1", "semis", "importdecls",
  "importdecls_semi", "importdecl", "optqualified", "maybeas",
  "maybeimpspec", "impspec", "prec", "infix", "ops", "topdecls",
  "topdecls_semi", "topdecl", "cl_decl", "ty_decl", "inst_decl",
  "overlap_pragma", "deriv_strategy_no_via", "deriv_strategy_via",
  "data_or_newtype", "opt_kind_sig", "tycl_hdr", "capi_ctype", "decls",
  "decllist", "binds", "wherebinds", "strings", "stringlist",
  "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars", "sigtypes1",
  "strict_mark", "strictness", "ctype", "ctypedoc", "context",
  "context_no_ops", "type", "typedoc", "btype", "btype_no_ops", "tyapps",
  "tyapp", "atype_docs", "atype", "inst_type", "deriv_types",
  "comma_types0", "comma_types1", "tv_bndrs", "tv_bndr", "kind", "constrs",
  "constrs1", "constr", "forall", "constr_stuff", "fielddecls",
  "fielddecls1", "fielddecl", "maybe_derivings", "derivings", "deriving",
  "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs", "gdrh",
  "sigdecl", "activation", "explicit_activation", "exp", "infixexp",
  "infixexp_top", "exp10_top", "exp10", "optSemi", "scc_annot", "fexp",
  "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps", "squals",
  "guardquals", "guardquals1", "altslist", "alts", "alts1", "alt",
  "alt_rhs", "gdpats", "gdpat", "pat", "bindpat", "apat", "apats1",
  "stmtlist", "stmts", "stmt", "qual", "fbinds", "fbinds1", "fbind",
  "qcon", "gen_qcon", "con", "con_list", "sysdcon_no_list", "sysdcon",
  "conop", "qconop", "gtycon", "ntgtycon", "oqtycon", "oqtycon_no_varcon",
  "qtyconop", "qtycondoc", "qtycon", "tycon", "qtyconsym", "tyconsym",
  "op", "varop", "qop", "qopm", "hole_op", "qvarop", "qvaropm", "tyvar",
  "tyvarop", "tyvarid", "var", "qvar", "qvarid", "varid", "qvarsym",
  "qvarsym_no_minus", "qvarsym1", "varsym", "varsym_no_minus",
  "special_id", "special_sym", "qconid", "conid", "qconsym", "consym",
  "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   522,   522,   539,   540,   542,   546,   547,   548,   550,
     551,   553,   554,   557,   559,   560,   561,   569,   570,   572,
     574,   575,   577,   578,   581,   582,   584,   585,   587,   588,
     590,   591,   593,   594,   596,   597,   601,   602,   604,   605,
     607,   609,   610,   612,   625,   626,   628,   629,   631,   632,
     634,   635,   640,   641,   643,   644,   645,   647,   648,   652,
     654,   655,   657,   658,   659,   662,   669,   671,   672,   673,
     674,   675,   677,   679,   680,   681,   686,   691,   692,   693,
     694,   695,   697,   698,   699,   701,   741,   742,   744,   745,
     754,   755,   757,   758,   759,   803,   804,   805,   806,   808,
     809,   811,   813,   814,   822,   823,   825,   826,   827,   840,
     841,   843,   845,   847,   848,   850,   851,   855,   861,   862,
     869,   870,   872,   874,   883,   885,   887,   888,   890,   893,
     895,   896,   898,   899,   901,   902,   903,   909,   916,   917,
     918,   919,   920,   921,   922,   928,   929,   930,   933,   935,
     936,   938,   939,   941,   942,   949,   950,   952,   953,   971,
     977,   979,   980,   982,   983,   985,   986,   988,   989,   991,
     992,   994,   995,   997,   999,  1000,  1002,  1003,  1005,  1006,
    1007,  1009,  1010,  1011,  1016,  1018,  1024,  1029,  1033,  1034,
    1036,  1037,  1041,  1043,  1044,  1045,  1047,  1048,  1049,  1050,
    1051,  1052,  1053,  1056,  1057,  1059,  1060,  1064,  1065,  1067,
    1068,  1070,  1071,  1073,  1074,  1075,  1077,  1078,  1081,  1082,
    1084,  1085,  1089,  1090,  1091,  1092,  1094,  1095,  1096,  1097,
    1099,  1101,  1102,  1103,  1105,  1107,  1108,  1110,  1111,  1112,
    1113,  1114,  1119,  1120,  1125,  1126,  1127,  1132,  1133,  1151,
    1152,  1153,  1154,  1155,  1156,  1157,  1159,  1160,  1173,  1175,
    1185,  1187,  1188,  1191,  1192,  1193,  1194,  1196,  1197,  1199,
    1200,  1201,  1203,  1205,  1206,  1208,  1209,  1218,  1220,  1221,
    1223,  1224,  1226,  1227,  1229,  1230,  1233,  1234,  1236,  1237,
    1238,  1239,  1244,  1245,  1247,  1248,  1249,  1254,  1255,  1257,
    1258,  1259,  1261,  1262,  1294,  1295,  1297,  1298,  1300,  1301,
    1302,  1304,  1305,  1307,  1308,  1309,  1310,  1312,  1313,  1315,
    1316,  1318,  1319,  1322,  1323,  1324,  1326,  1327,  1328,  1329,
    1330,  1332,  1333,  1334,  1336,  1337,  1338,  1339,  1340,  1343,
    1344,  1346,  1348,  1349,  1353,  1355,  1356,  1357,  1359,  1360,
    1361,  1362,  1367,  1368,  1370,  1371,  1373,  1374,  1375,  1377,
    1378,  1379,  1381,  1383,  1384,  1386,  1387,  1391,  1393,  1395,
    1396,  1397,  1398,  1399,  1402,  1403,  1405,  1406,  1407,  1409,
    1410,  1412,  1413,  1414,  1415,  1416,  1417,  1418,  1419,  1421,
    1422,  1424,  1425,  1427,  1429,  1430,  1432,  1433,  1435,  1436,
    1437,  1438,  1439,  1440,  1441,  1442,  1443,  1444,  1445,  1446,
    1447,  1448,  1449,  1450,  1451,  1452,  1454,  1455,  1456,  1460,
    1461,  1463,  1465,  1466,  1468,  1469,  1473,  1474,  1475,  1476,
    1481,  1484,  1488,  1489,  1491,  1492
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
#line 5871 "parser.cc"

#line 1501 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

Haskell::Module make_module(const std::string& name, const expression_ref& exports, const std::vector<Haskell::ImpDecl>& impdecls, const std::optional<Haskell::Decls>& topdecls)
{
    return {name, exports, impdecls, topdecls};
}

pair<vector<Haskell::ImpDecl>, optional<Haskell::Decls>> make_body(const std::vector<Haskell::ImpDecl>& imports, const std::optional<Haskell::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Haskell::Decls(*topdecls, true);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

expression_ref make_exports(const vector<expression_ref>& exports)
{
    return new expression(AST_node("Exports"),exports);
}

expression_ref make_builtin_expr(const string& name, int args, const string& s1, const string& s2)
{
    return new expression(AST_node("Builtin"),{String(name), args, String(s1), String(s2)});
}

expression_ref make_builtin_expr(const string& name, int args, const string& s1)
{
    return new expression(AST_node("Builtin"),{String(name), args, String(s1)});
}

vector<expression_ref> make_String_vec(const vector<string>& strings)
{
    vector<expression_ref> Strings;
    for(auto& string: strings)
        Strings.push_back(String(string));
    return Strings;
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<expression_ref>>
check_type_or_class_header(expression_ref type)
{
    auto [type_head, type_args] = Haskell::decompose_type_apps(type);

    // FIXME -- add location!
    if (not type_head.is_a<Haskell::TypeVar>())
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    auto name = type_head.as_<Haskell::TypeVar>().name;

    return {name, type_args};
}

Haskell::FieldDecl make_field_decl(const std::vector<std::string>& field_names, const Haskell::Type& type)
{
    return {field_names, type};
}

Haskell::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, type_args, rhs_type};
}

Haskell::DataOrNewtypeDecl make_data_or_newtype(const Haskell::DataOrNewtype& d_or_n, const Haskell::Context&  context,
                                                const expression_ref& header, const vector<Haskell::Constructor>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Haskell::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, type_args, context, constrs};
}

Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& type, const optional<Located<Haskell::Decls>>& decls)
{
    return {type, decls};
}

Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const optional<Located<Haskell::Decls>>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name,type_args,context,decls};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Haskell::Context make_context(const expression_ref& context)
{
    vector<Haskell::Type> constraints;
    if (context.is_a<Haskell::TupleType>())
    {
        constraints = context.as_<Haskell::TupleType>().element_types;
    }
    else
        constraints.push_back(context);

    return {constraints};
}

expression_ref make_tv_bndrs(const vector<expression_ref>& tv_bndrs)
{
    return new expression(AST_node("tv_bndrs"),tv_bndrs);
}

expression_ref make_tyapps(const std::vector<expression_ref>& tyapps)
{
    assert(not tyapps.empty());
    expression_ref E = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	E = make_type_app(E,tyapps[i]);
    return E;
}

Haskell::Var make_var(const Located<string>& v)
{
    return {v};
}

bool check_kind(const Haskell::Type& kind)
{
    auto [kind_head, kind_args] = Haskell::decompose_type_apps(kind);

    if (not kind_head.is_a<Haskell::TypeVar>()) return false;

    auto V = kind_head.as_<Haskell::TypeVar>();
    if (kind_args.empty())
    {
        return (V.name == "*");
    }
    else if (kind_args.size() == 2)
    {
        return (V.name == "->") and check_kind(kind_args[0]) and check_kind(kind_args[1]);
    }
    else
        return false;
}

Haskell::Type make_kind(const Haskell::Type& kind)
{
    if (not check_kind(kind))
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return kind;
}

Haskell::TypeVar make_type_var(const string& id)
{
    return Haskell::TypeVar(id);
}

Haskell::TypeVarOfKind make_type_var_of_kind(const string& id, const Haskell::Type& kind)
{
    return {id, kind};
}

Haskell::TypeOfKind make_type_of_kind(const Haskell::Type& type, const Haskell::Type& kind)
{
    return {type, kind};
}

Haskell::TupleType make_tuple_type(const std::vector<Haskell::Type>& types)
{
    return {types};
}

Haskell::ListType make_list_type(const Haskell::Type& type)
{
    return {type};
}

Haskell::TypeApp make_type_app(const Haskell::Type& head, const Haskell::Type& arg)
{
    return {head, arg};
}

optional<pair<string, Haskell::FieldDecls>> is_record_con(const expression_ref& typeish)
{
    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (args.size() != 1) return {};

    if (not head.is_a<Haskell::TypeVar>()) return {};

    if (not args[0].is_a<Haskell::FieldDecls>()) return {};

    return {{head.as_<Haskell::TypeVar>().name, args[0].as_<Haskell::FieldDecls>()}};
}

optional<pair<string, std::vector<expression_ref>>> is_normal_con(const expression_ref& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = Haskell::decompose_type_apps(typeish);

    if (not head.is_a<Haskell::TypeVar>())
        return {};

    return {{head.as_<Haskell::TypeVar>().name, args}};
}

Haskell::Constructor make_constructor(const expression_ref& forall, const std::optional<Haskell::Context>& c, const expression_ref& typeish)
{
    if (auto constr = is_record_con(typeish))
    {
        auto [name, fields] = *constr;
        return {forall, c, name, fields};
    }
    else if (auto constr = is_normal_con(typeish))
    {
        auto [name, fields] = *constr;
        return {forall, c, name, fields};
    }
    else
        throw myexception()<<"constructor '"<<typeish<<"' does not make sense";
}

Haskell::FieldDecls make_field_decls(const std::vector<Haskell::FieldDecl>& ds)
{
    return {ds};
}

Haskell::StrictLazyType make_strict_lazy_type(const Haskell::StrictLazy& sl, const Haskell::Type& t)
{
    return {sl, t};
}

expression_ref make_forall_type(const std::vector<expression_ref>& tv_bndrs, const Haskell::Type& t)
{
    if (tv_bndrs.empty())
        return t;
    else
        return Haskell::ForallType(tv_bndrs, t);
}

expression_ref make_constrained_type(const Haskell::Context& context, const Haskell::Type& t)
{
    if (context.constraints.empty())
        return t;
    else
        return Haskell::ConstrainedType(context, t);
}

expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type)
{
    return new expression(AST_node("typed_exp"),{exp,type});
}

Haskell::SimpleRHS make_rhs(const Located<expression_ref>& exp, const optional<Located<Haskell::Decls>>& wherebinds)
{
    return {exp, wherebinds};
}

expression_ref make_infixexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else
	return new expression(AST_node("infixexp"),args);
}


expression_ref make_minus(const expression_ref& exp)
{
    return new expression(AST_node("infixexp"),{AST_node("neg"),exp});
}

expression_ref make_fexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else {
	expression_ref f = args[0];
	for(int i=1;i<args.size();i++)
	    f = {f,args[i]};
	return f;
    }
}

Haskell::AsPattern make_as_pattern(const Haskell::Var& x, const expression_ref& pat)
{
    return Haskell::AsPattern(x,pat);
}

Haskell::LazyPattern make_lazy_pattern(const expression_ref& pat)
{
    return { pat };
}

Haskell::StrictPattern make_strict_pattern(const expression_ref& pat)
{
    return { pat };
}

Located<Haskell::Decls> make_decls(const yy::location& loc, std::vector<expression_ref>& decls)
{
    return {loc, Haskell::Decls(decls)};
}

Haskell::ValueDecl make_value_decl(const expression_ref& lhs,  const expression_ref& rhs)
{
    return {lhs, rhs};
}

Haskell::LambdaExp make_lambdaexp(const vector<expression_ref>& pats, const expression_ref& body)
{
    return { pats, body };
}

Haskell::LetExp make_let(const Located<Haskell::Decls>& binds, const Located<expression_ref>& body)
{
    return { binds, body };
}

Haskell::IfExp make_if(const Located<expression_ref>& cond, const Located<expression_ref>& alt_true, const Located<expression_ref>& alt_false)
{
    return {cond, alt_true, alt_false};
}

Haskell::CaseExp make_case(const expression_ref& obj, const Haskell::Alts& alts)
{
    return {obj, alts};
}

Haskell::Do make_do(const Haskell::Stmts& stmts)
{
    return {stmts};
}

Haskell::MDo make_mdo(const Haskell::Stmts& stmts)
{
    return {stmts};
}

Haskell::Tuple yy_make_tuple(const vector<expression_ref>& elements)
{
    return Haskell::Tuple(elements);
}

Haskell::List make_list(const vector<expression_ref>& elements)
{
    return Haskell::List(elements);
}

Haskell::Alts make_alts(const vector<Located<Haskell::Alt>>& alts)
{
    return {alts};
}

Located<Haskell::Alt> yy_make_alt(const yy::location& loc, const expression_ref& pat, const expression_ref& alt_rhs)
{
    return {loc, {pat, alt_rhs}};
}

Haskell::MultiGuardedRHS make_gdrhs(const vector<Haskell::GuardedRHS>& guards, const optional<Located<Haskell::Decls>>& wherebinds)
{
    return {guards, wherebinds};
}

Haskell::GuardedRHS make_gdrh(const vector<expression_ref>& guardquals, const expression_ref& exp)
{
    return {guardquals, exp};
}

Haskell::Stmts make_stmts(const vector<expression_ref>& stmts)
{
    return {stmts};
}

Haskell::FixityDecl make_fixity_decl(const Haskell::Fixity& fixity, optional<int>& prec, vector<string>& op_names)
{
    return {fixity, prec, op_names};
}

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return make_list(chars);
}

