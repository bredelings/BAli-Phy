// A Bison parser, made by GNU Bison 3.6.3.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2020 Free Software Foundation, Inc.

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
#line 78 "parser.y"

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
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE (Symbol)
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
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.YY_MOVE_OR_COPY< bool > (YY_MOVE (that.value));
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.YY_MOVE_OR_COPY< char > (YY_MOVE (that.value));
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.YY_MOVE_OR_COPY< expression_ref > (YY_MOVE (that.value));
        break;

      case 136: // "PRIMFLOAT"
        value.YY_MOVE_OR_COPY< float > (YY_MOVE (that.value));
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case 172: // prec
        value.YY_MOVE_OR_COPY< std::optional<int> > (YY_MOVE (that.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.YY_MOVE_OR_COPY< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.YY_MOVE_OR_COPY< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case 174: // ops
      case 204: // sig_vars
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
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (YY_MOVE (that.value));
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.move< char > (YY_MOVE (that.value));
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
        value.move< double > (YY_MOVE (that.value));
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.move< expression_ref > (YY_MOVE (that.value));
        break;

      case 136: // "PRIMFLOAT"
        value.move< float > (YY_MOVE (that.value));
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.move< int > (YY_MOVE (that.value));
        break;

      case 172: // prec
        value.move< std::optional<int> > (YY_MOVE (that.value));
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (YY_MOVE (that.value));
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
        value.move< std::string > (YY_MOVE (that.value));
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.move< std::vector<expression_ref> > (YY_MOVE (that.value));
        break;

      case 174: // ops
      case 204: // sig_vars
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
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (that.value);
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.copy< char > (that.value);
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
        value.copy< double > (that.value);
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 136: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.copy< int > (that.value);
        break;

      case 172: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< std::optional<std::string> > (that.value);
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
        value.copy< std::string > (that.value);
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.copy< std::vector<expression_ref> > (that.value);
        break;

      case 174: // ops
      case 204: // sig_vars
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
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (that.value);
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        value.move< char > (that.value);
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
        value.move< double > (that.value);
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        value.move< expression_ref > (that.value);
        break;

      case 136: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        value.move< int > (that.value);
        break;

      case 172: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (that.value);
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
        value.move< std::string > (that.value);
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        value.move< std::vector<expression_ref> > (that.value);
        break;

      case 174: // ops
      case 204: // sig_vars
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
    YYUSE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " ("
            << yysym.location << ": ";
        YYUSE (yykind);
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
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        yylhs.value.emplace< bool > ();
        break;

      case 128: // "CHAR"
      case 132: // "PRIMCHAR"
        yylhs.value.emplace< char > ();
        break;

      case 131: // "RATIONAL"
      case 137: // "PRIMDOUBLE"
        yylhs.value.emplace< double > ();
        break;

      case 143: // module
      case 146: // body
      case 147: // body2
      case 148: // top
      case 149: // top1
      case 150: // maybeexports
      case 153: // export
      case 157: // qcname_ext_w_wildcard
      case 158: // qcname_ext
      case 159: // qcname
      case 164: // importdecl
      case 170: // maybeimpspec
      case 171: // impspec
      case 177: // topdecl
      case 178: // cl_decl
      case 179: // ty_decl
      case 180: // inst_decl
      case 186: // tycl_hdr
      case 195: // decllist
      case 196: // binds
      case 197: // wherebinds
      case 200: // opt_sig
      case 201: // opt_tyconsig
      case 202: // sigtype
      case 203: // sigtypedoc
      case 209: // ctype
      case 210: // ctypedoc
      case 211: // context
      case 212: // context_no_ops
      case 213: // type
      case 214: // typedoc
      case 215: // btype
      case 218: // tyapp
      case 219: // atype_docs
      case 220: // atype
      case 221: // inst_type
      case 227: // tv_bndr
      case 228: // kind
      case 231: // constr
      case 232: // forall
      case 233: // constr_stuff
      case 236: // fielddecl
      case 241: // decl_no_th
      case 242: // decl
      case 243: // rhs
      case 245: // gdrh
      case 246: // sigdecl
      case 249: // exp
      case 252: // exp10_top
      case 253: // exp10
      case 257: // aexp
      case 258: // aexp1
      case 259: // aexp2
      case 260: // texp
      case 262: // list
      case 265: // transformqual
      case 271: // alt
      case 272: // alt_rhs
      case 274: // ifgdpats
      case 275: // gdpat
      case 276: // pat
      case 277: // bindpat
      case 278: // apat
      case 282: // stmt
      case 283: // qual
      case 330: // literal
        yylhs.value.emplace< expression_ref > ();
        break;

      case 136: // "PRIMFLOAT"
        yylhs.value.emplace< float > ();
        break;

      case 130: // "INTEGER"
      case 134: // "PRIMINTEGER"
      case 135: // "PRIMWORD"
      case 333: // commas
        yylhs.value.emplace< int > ();
        break;

      case 172: // prec
        yylhs.value.emplace< std::optional<int> > ();
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        yylhs.value.emplace< std::optional<std::string> > ();
        break;

      case 118: // "VARID"
      case 119: // "CONID"
      case 120: // "VARSYM"
      case 121: // "CONSYM"
      case 122: // "QVARID"
      case 123: // "QCONID"
      case 124: // "QVARSYM"
      case 125: // "QCONSYM"
      case 126: // "IPDUPVARID"
      case 127: // "LABELVARID"
      case 129: // "STRING"
      case 133: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 206: // strict_mark
      case 207: // strictness
      case 287: // qcon
      case 288: // gen_qcon
      case 289: // con
      case 291: // sysdcon_no_list
      case 292: // sysdcon
      case 293: // conop
      case 294: // qconop
      case 295: // gtycon
      case 296: // ntgtycon
      case 297: // oqtycon
      case 298: // oqtycon_no_varcon
      case 299: // qtyconop
      case 300: // qtycondoc
      case 301: // qtycon
      case 302: // tycon
      case 303: // qtyconsym
      case 304: // tyconsym
      case 305: // op
      case 306: // varop
      case 307: // qop
      case 308: // qopm
      case 309: // hole_op
      case 310: // qvarop
      case 311: // qvaropm
      case 312: // tyvar
      case 313: // tyvarop
      case 314: // tyvarid
      case 315: // var
      case 316: // qvar
      case 317: // qvarid
      case 318: // varid
      case 319: // qvarsym
      case 320: // qvarsym_no_minus
      case 321: // qvarsym1
      case 322: // varsym
      case 323: // varsym_no_minus
      case 324: // special_id
      case 325: // special_sym
      case 326: // qconid
      case 327: // conid
      case 328: // qconsym
      case 329: // consym
      case 332: // modid
        yylhs.value.emplace< std::string > ();
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 194: // decls
      case 205: // sigtypes1
      case 216: // btype_no_ops
      case 217: // tyapps
      case 223: // comma_types0
      case 224: // comma_types1
      case 226: // tv_bndrs
      case 229: // constrs
      case 230: // constrs1
      case 234: // fielddecls
      case 235: // fielddecls1
      case 244: // gdrhs
      case 250: // infixexp
      case 251: // infixexp_top
      case 256: // fexp
      case 261: // tup_exprs
      case 263: // lexps
      case 264: // squals
      case 266: // guardquals
      case 267: // guardquals1
      case 268: // altslist
      case 269: // alts
      case 270: // alts1
      case 273: // gdpats
      case 279: // apats1
      case 280: // stmtlist
      case 281: // stmts
        yylhs.value.emplace< std::vector<expression_ref> > ();
        break;

      case 174: // ops
      case 204: // sig_vars
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
  case 2:
#line 504 "parser.y"
             {drv.result = yystack_[0].value.as < expression_ref > ();}
#line 1586 "parser.cc"
    break;

  case 3:
#line 521 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 1592 "parser.cc"
    break;

  case 4:
#line 522 "parser.y"
                                                                 {yylhs.value.as < expression_ref > () = make_module("Main",{},yystack_[0].value.as < expression_ref > ());}
#line 1598 "parser.cc"
    break;

  case 5:
#line 524 "parser.y"
                                                                 {drv.push_module_context();}
#line 1604 "parser.cc"
    break;

  case 9:
#line 532 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1610 "parser.cc"
    break;

  case 10:
#line 533 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1616 "parser.cc"
    break;

  case 11:
#line 535 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1622 "parser.cc"
    break;

  case 12:
#line 536 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1628 "parser.cc"
    break;

  case 13:
#line 539 "parser.y"
                                           {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1634 "parser.cc"
    break;

  case 14:
#line 541 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1640 "parser.cc"
    break;

  case 15:
#line 542 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 1646 "parser.cc"
    break;

  case 16:
#line 543 "parser.y"
                                           {yylhs.value.as < expression_ref > () = make_body(yystack_[0].value.as < std::vector<expression_ref> > (),{});}
#line 1652 "parser.cc"
    break;

  case 17:
#line 551 "parser.y"
                                      {yylhs.value.as < expression_ref > () = make_exports(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 1658 "parser.cc"
    break;

  case 18:
#line 552 "parser.y"
                                      {}
#line 1664 "parser.cc"
    break;

  case 19:
#line 554 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1670 "parser.cc"
    break;

  case 20:
#line 556 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1676 "parser.cc"
    break;

  case 21:
#line 557 "parser.y"
                                      {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1682 "parser.cc"
    break;

  case 22:
#line 559 "parser.y"
                                      {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 1688 "parser.cc"
    break;

  case 23:
#line 560 "parser.y"
                                      {yylhs.value.as < expression_ref > () = AST_node("module",yystack_[0].value.as < std::string > ());}
#line 1694 "parser.cc"
    break;

  case 24:
#line 561 "parser.y"
                                      {}
#line 1700 "parser.cc"
    break;

  case 27:
#line 566 "parser.y"
                   {}
#line 1706 "parser.cc"
    break;

  case 28:
#line 567 "parser.y"
                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 1712 "parser.cc"
    break;

  case 29:
#line 569 "parser.y"
                                                  {yylhs.value.as < std::vector<expression_ref> > () = yystack_[3].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ());}
#line 1718 "parser.cc"
    break;

  case 30:
#line 570 "parser.y"
                                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 1724 "parser.cc"
    break;

  case 31:
#line 572 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1730 "parser.cc"
    break;

  case 32:
#line 573 "parser.y"
                                     {}
#line 1736 "parser.cc"
    break;

  case 33:
#line 575 "parser.y"
                                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1742 "parser.cc"
    break;

  case 34:
#line 576 "parser.y"
                                     {}
#line 1748 "parser.cc"
    break;

  case 35:
#line 578 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1754 "parser.cc"
    break;

  case 36:
#line 579 "parser.y"
                                     {yylhs.value.as < expression_ref > () = AST_node("qvar",yystack_[0].value.as < std::string > ()); }
#line 1760 "parser.cc"
    break;

  case 41:
#line 589 "parser.y"
                                         { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (), yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1766 "parser.cc"
    break;

  case 42:
#line 591 "parser.y"
                                                     { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 1772 "parser.cc"
    break;

  case 43:
#line 592 "parser.y"
                         { }
#line 1778 "parser.cc"
    break;

  case 44:
#line 594 "parser.y"
                                                                                            {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as < bool > ()) e.push_back(AST_node("qualified"));
    e.push_back(String(yystack_[2].value.as < std::string > ()));
    if (yystack_[1].value.as < std::optional<std::string> > ()) e.push_back(AST_node("as", *yystack_[1].value.as < std::optional<std::string> > ()));
    if (yystack_[0].value.as < expression_ref > ()) e.push_back(yystack_[0].value.as < expression_ref > ());
    yylhs.value.as < expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 1791 "parser.cc"
    break;

  case 45:
#line 603 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1797 "parser.cc"
    break;

  case 46:
#line 604 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1803 "parser.cc"
    break;

  case 47:
#line 606 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1809 "parser.cc"
    break;

  case 48:
#line 607 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1815 "parser.cc"
    break;

  case 49:
#line 609 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1821 "parser.cc"
    break;

  case 50:
#line 610 "parser.y"
                               { }
#line 1827 "parser.cc"
    break;

  case 51:
#line 612 "parser.y"
                               { yylhs.value.as < bool > () = true; }
#line 1833 "parser.cc"
    break;

  case 52:
#line 613 "parser.y"
                               { yylhs.value.as < bool > () = false; }
#line 1839 "parser.cc"
    break;

  case 53:
#line 615 "parser.y"
                               { yylhs.value.as < std::optional<std::string> > () = yystack_[0].value.as < std::string > (); }
#line 1845 "parser.cc"
    break;

  case 54:
#line 616 "parser.y"
                               { }
#line 1851 "parser.cc"
    break;

  case 55:
#line 618 "parser.y"
                               { yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > (); }
#line 1857 "parser.cc"
    break;

  case 56:
#line 619 "parser.y"
                               { }
#line 1863 "parser.cc"
    break;

  case 57:
#line 621 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 1869 "parser.cc"
    break;

  case 58:
#line 622 "parser.y"
                                      { yylhs.value.as < expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as < std::vector<expression_ref> > ()}; }
#line 1875 "parser.cc"
    break;

  case 59:
#line 627 "parser.y"
                   { }
#line 1881 "parser.cc"
    break;

  case 60:
#line 628 "parser.y"
                   { yylhs.value.as < std::optional<int> > () = yystack_[0].value.as < int > (); }
#line 1887 "parser.cc"
    break;

  case 61:
#line 630 "parser.y"
                   { yylhs.value.as < std::string > () = "infix";  }
#line 1893 "parser.cc"
    break;

  case 62:
#line 631 "parser.y"
                   { yylhs.value.as < std::string > () = "infixl"; }
#line 1899 "parser.cc"
    break;

  case 63:
#line 632 "parser.y"
                   { yylhs.value.as < std::string > () = "infixr"; }
#line 1905 "parser.cc"
    break;

  case 64:
#line 634 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 1911 "parser.cc"
    break;

  case 65:
#line 635 "parser.y"
                   { yylhs.value.as < std::vector<std::string> > () = {yystack_[0].value.as < std::string > ()}; }
#line 1917 "parser.cc"
    break;

  case 66:
#line 639 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ()); }
#line 1923 "parser.cc"
    break;

  case 67:
#line 641 "parser.y"
                                            { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[1].value.as < expression_ref > ()); }
#line 1929 "parser.cc"
    break;

  case 68:
#line 642 "parser.y"
                                            { }
#line 1935 "parser.cc"
    break;

  case 69:
#line 644 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1941 "parser.cc"
    break;

  case 70:
#line 645 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1947 "parser.cc"
    break;

  case 71:
#line 646 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1953 "parser.cc"
    break;

  case 72:
#line 649 "parser.y"
                                               {}
#line 1959 "parser.cc"
    break;

  case 73:
#line 656 "parser.y"
                                               {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 1965 "parser.cc"
    break;

  case 74:
#line 657 "parser.y"
                                               {}
#line 1971 "parser.cc"
    break;

  case 75:
#line 658 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 1977 "parser.cc"
    break;

  case 76:
#line 659 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 1983 "parser.cc"
    break;

  case 77:
#line 660 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[3].value.as < std::string > (),yystack_[2].value.as < int > (),yystack_[1].value.as < std::string > (),yystack_[0].value.as < std::string > ());}
#line 1989 "parser.cc"
    break;

  case 78:
#line 661 "parser.y"
                                               {yylhs.value.as < expression_ref > () = make_builtin_expr(yystack_[2].value.as < std::string > (),yystack_[1].value.as < int > (),yystack_[0].value.as < std::string > ());}
#line 1995 "parser.cc"
    break;

  case 79:
#line 663 "parser.y"
                                               {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Class"),{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}};}
#line 2001 "parser.cc"
    break;

  case 80:
#line 665 "parser.y"
                                                                           {}
#line 2007 "parser.cc"
    break;

  case 81:
#line 666 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = make_data_or_newtype(yystack_[4].value.as < std::string > (),yystack_[2].value.as < expression_ref > (),yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2013 "parser.cc"
    break;

  case 82:
#line 667 "parser.y"
                                                                           {}
#line 2019 "parser.cc"
    break;

  case 83:
#line 671 "parser.y"
                                                                           {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Instance"),{yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}};}
#line 2025 "parser.cc"
    break;

  case 93:
#line 726 "parser.y"
                           {yylhs.value.as < std::string > ()="data";}
#line 2031 "parser.cc"
    break;

  case 94:
#line 727 "parser.y"
                           {yylhs.value.as < std::string > ()="newtype";}
#line 2037 "parser.cc"
    break;

  case 97:
#line 739 "parser.y"
                             {yylhs.value.as < expression_ref > () = expression_ref{AST_node("ClassHeader"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}};}
#line 2043 "parser.cc"
    break;

  case 98:
#line 740 "parser.y"
                             {yylhs.value.as < expression_ref > () = expression_ref{AST_node("ClassHeader"),{{},yystack_[0].value.as < expression_ref > ()}};}
#line 2049 "parser.cc"
    break;

  case 114:
#line 784 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2055 "parser.cc"
    break;

  case 115:
#line 785 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 2061 "parser.cc"
    break;

  case 116:
#line 786 "parser.y"
                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2067 "parser.cc"
    break;

  case 117:
#line 787 "parser.y"
                        {}
#line 2073 "parser.cc"
    break;

  case 118:
#line 789 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2079 "parser.cc"
    break;

  case 119:
#line 790 "parser.y"
                                 {yylhs.value.as < expression_ref > () = expression_ref{AST_node("Decls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2085 "parser.cc"
    break;

  case 120:
#line 792 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2091 "parser.cc"
    break;

  case 121:
#line 794 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2097 "parser.cc"
    break;

  case 122:
#line 795 "parser.y"
                                 {}
#line 2103 "parser.cc"
    break;

  case 128:
#line 816 "parser.y"
                 {}
#line 2109 "parser.cc"
    break;

  case 129:
#line 817 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2115 "parser.cc"
    break;

  case 130:
#line 819 "parser.y"
                     {}
#line 2121 "parser.cc"
    break;

  case 131:
#line 820 "parser.y"
                     {yylhs.value.as < expression_ref > () = make_type_id(yystack_[0].value.as < std::string > ());}
#line 2127 "parser.cc"
    break;

  case 132:
#line 822 "parser.y"
                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2133 "parser.cc"
    break;

  case 133:
#line 824 "parser.y"
                     {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2139 "parser.cc"
    break;

  case 134:
#line 826 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > () = yystack_[2].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2145 "parser.cc"
    break;

  case 135:
#line 827 "parser.y"
                           {yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ());}
#line 2151 "parser.cc"
    break;

  case 136:
#line 829 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2157 "parser.cc"
    break;

  case 137:
#line 830 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2163 "parser.cc"
    break;

  case 138:
#line 834 "parser.y"
                                            {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 2169 "parser.cc"
    break;

  case 139:
#line 835 "parser.y"
                                            {}
#line 2175 "parser.cc"
    break;

  case 140:
#line 836 "parser.y"
                                            {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 2181 "parser.cc"
    break;

  case 141:
#line 838 "parser.y"
                {yylhs.value.as < std::string > () = "!";}
#line 2187 "parser.cc"
    break;

  case 142:
#line 839 "parser.y"
                {yylhs.value.as < std::string > () = "~";}
#line 2193 "parser.cc"
    break;

  case 145:
#line 844 "parser.y"
                                   {yylhs.value.as < expression_ref > () = new expression(AST_node("forall"),{make_tv_bndrs(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2199 "parser.cc"
    break;

  case 146:
#line 845 "parser.y"
                                   {yylhs.value.as < expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2205 "parser.cc"
    break;

  case 147:
#line 847 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2211 "parser.cc"
    break;

  case 148:
#line 849 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2217 "parser.cc"
    break;

  case 149:
#line 858 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2223 "parser.cc"
    break;

  case 150:
#line 860 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2229 "parser.cc"
    break;

  case 151:
#line 862 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2235 "parser.cc"
    break;

  case 152:
#line 863 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps({make_type_id("->"),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 2241 "parser.cc"
    break;

  case 153:
#line 865 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2247 "parser.cc"
    break;

  case 154:
#line 868 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2253 "parser.cc"
    break;

  case 155:
#line 870 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2259 "parser.cc"
    break;

  case 156:
#line 871 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2265 "parser.cc"
    break;

  case 157:
#line 873 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2271 "parser.cc"
    break;

  case 158:
#line 874 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2277 "parser.cc"
    break;

  case 159:
#line 876 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2283 "parser.cc"
    break;

  case 160:
#line 877 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_id(yystack_[0].value.as < std::string > ());}
#line 2289 "parser.cc"
    break;

  case 161:
#line 878 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_type_id(yystack_[0].value.as < std::string > ());}
#line 2295 "parser.cc"
    break;

  case 162:
#line 884 "parser.y"
                                   {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2301 "parser.cc"
    break;

  case 163:
#line 886 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_id(yystack_[0].value.as < std::string > ());}
#line 2307 "parser.cc"
    break;

  case 164:
#line 887 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_id(yystack_[0].value.as < std::string > ());}
#line 2313 "parser.cc"
    break;

  case 165:
#line 888 "parser.y"
                                       {yylhs.value.as < expression_ref > () = AST_node("kind_star");}
#line 2319 "parser.cc"
    break;

  case 166:
#line 889 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref{AST_node("strictness"),{yystack_[1].value.as < std::string > (),yystack_[0].value.as < expression_ref > ()}};}
#line 2325 "parser.cc"
    break;

  case 167:
#line 890 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref{AST_node("FieldDecls"),yystack_[1].value.as < std::vector<expression_ref> > ()};}
#line 2331 "parser.cc"
    break;

  case 168:
#line 891 "parser.y"
                                       {yylhs.value.as < expression_ref > () = make_type_id("()");}
#line 2337 "parser.cc"
    break;

  case 169:
#line 892 "parser.y"
                                       {auto ts = yystack_[3].value.as < std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as < expression_ref > ());yylhs.value.as < expression_ref > () = expression_ref{AST_node("TupleType"),ts};}
#line 2343 "parser.cc"
    break;

  case 170:
#line 893 "parser.y"
                                       {}
#line 2349 "parser.cc"
    break;

  case 171:
#line 894 "parser.y"
                                       {}
#line 2355 "parser.cc"
    break;

  case 172:
#line 895 "parser.y"
                                       {}
#line 2361 "parser.cc"
    break;

  case 173:
#line 896 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref{AST_node("ListType"),{yystack_[1].value.as < expression_ref > ()}};}
#line 2367 "parser.cc"
    break;

  case 174:
#line 897 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2373 "parser.cc"
    break;

  case 175:
#line 898 "parser.y"
                                       {yylhs.value.as < expression_ref > () = expression_ref{AST_node("TypeOfKind"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}};}
#line 2379 "parser.cc"
    break;

  case 176:
#line 901 "parser.y"
                                       {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2385 "parser.cc"
    break;

  case 179:
#line 906 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2391 "parser.cc"
    break;

  case 180:
#line 907 "parser.y"
                                       {}
#line 2397 "parser.cc"
    break;

  case 181:
#line 909 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2403 "parser.cc"
    break;

  case 182:
#line 910 "parser.y"
                                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2409 "parser.cc"
    break;

  case 185:
#line 915 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2415 "parser.cc"
    break;

  case 186:
#line 916 "parser.y"
                               {}
#line 2421 "parser.cc"
    break;

  case 187:
#line 918 "parser.y"
                                    {yylhs.value.as < expression_ref > () = AST_node("type_id",yystack_[0].value.as < std::string > ());}
#line 2427 "parser.cc"
    break;

  case 188:
#line 919 "parser.y"
                                    {yylhs.value.as < expression_ref > () = new expression(AST_node("type_of_kind"),{AST_node("type_id",yystack_[3].value.as < std::string > ()),yystack_[1].value.as < expression_ref > ()});}
#line 2433 "parser.cc"
    break;

  case 189:
#line 937 "parser.y"
             {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2439 "parser.cc"
    break;

  case 190:
#line 943 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2445 "parser.cc"
    break;

  case 191:
#line 945 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2451 "parser.cc"
    break;

  case 192:
#line 946 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2457 "parser.cc"
    break;

  case 193:
#line 948 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_context(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2463 "parser.cc"
    break;

  case 194:
#line 949 "parser.y"
                                                {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2469 "parser.cc"
    break;

  case 195:
#line 951 "parser.y"
                                {if (yystack_[1].value.as < std::vector<expression_ref> > ().size()>1) yylhs.value.as < expression_ref > () = make_tv_bndrs(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2475 "parser.cc"
    break;

  case 196:
#line 952 "parser.y"
                                {}
#line 2481 "parser.cc"
    break;

  case 197:
#line 954 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2487 "parser.cc"
    break;

  case 198:
#line 955 "parser.y"
                                                {yylhs.value.as < expression_ref > () = make_tyapps({AST_node("type_id",yystack_[1].value.as < std::string > ()),make_tyapps(yystack_[2].value.as < std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2493 "parser.cc"
    break;

  case 199:
#line 957 "parser.y"
                                {}
#line 2499 "parser.cc"
    break;

  case 200:
#line 958 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 2505 "parser.cc"
    break;

  case 201:
#line 960 "parser.y"
                                        {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2511 "parser.cc"
    break;

  case 202:
#line 961 "parser.y"
                                        {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2517 "parser.cc"
    break;

  case 203:
#line 963 "parser.y"
                                        {yylhs.value.as < expression_ref > () = new expression(AST_node("FieldDecl"),{make_sig_vars(yystack_[2].value.as < std::vector<std::string> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2523 "parser.cc"
    break;

  case 214:
#line 982 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2529 "parser.cc"
    break;

  case 215:
#line 984 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as < expression_ref > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2535 "parser.cc"
    break;

  case 216:
#line 986 "parser.y"
                              {yylhs.value.as < expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()});}
#line 2541 "parser.cc"
    break;

  case 217:
#line 987 "parser.y"
                              {}
#line 2547 "parser.cc"
    break;

  case 218:
#line 990 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2553 "parser.cc"
    break;

  case 219:
#line 994 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2559 "parser.cc"
    break;

  case 220:
#line 995 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2565 "parser.cc"
    break;

  case 221:
#line 997 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2571 "parser.cc"
    break;

  case 222:
#line 998 "parser.y"
                              {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2577 "parser.cc"
    break;

  case 223:
#line 1002 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2583 "parser.cc"
    break;

  case 224:
#line 1004 "parser.y"
                                       {}
#line 2589 "parser.cc"
    break;

  case 225:
#line 1005 "parser.y"
                                          {}
#line 2595 "parser.cc"
    break;

  case 226:
#line 1006 "parser.y"
                         { yylhs.value.as < expression_ref > () = make_infix(yystack_[2].value.as < std::string > (),yystack_[1].value.as < std::optional<int> > (),yystack_[0].value.as < std::vector<std::string> > ()); }
#line 2601 "parser.cc"
    break;

  case 227:
#line 1007 "parser.y"
                             {}
#line 2607 "parser.cc"
    break;

  case 228:
#line 1008 "parser.y"
                                                    {}
#line 2613 "parser.cc"
    break;

  case 229:
#line 1009 "parser.y"
                                            {}
#line 2619 "parser.cc"
    break;

  case 230:
#line 1010 "parser.y"
                              {}
#line 2625 "parser.cc"
    break;

  case 231:
#line 1011 "parser.y"
                                     {}
#line 2631 "parser.cc"
    break;

  case 232:
#line 1012 "parser.y"
                                                               {}
#line 2637 "parser.cc"
    break;

  case 233:
#line 1013 "parser.y"
                                                                      {}
#line 2643 "parser.cc"
    break;

  case 234:
#line 1014 "parser.y"
                                                     {}
#line 2649 "parser.cc"
    break;

  case 239:
#line 1024 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as < std::vector<expression_ref> > ()),yystack_[0].value.as < expression_ref > ()); }
#line 2655 "parser.cc"
    break;

  case 240:
#line 1025 "parser.y"
                           { yylhs.value.as < expression_ref > () = make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2661 "parser.cc"
    break;

  case 241:
#line 1027 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2667 "parser.cc"
    break;

  case 242:
#line 1028 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2673 "parser.cc"
    break;

  case 243:
#line 1030 "parser.y"
                                {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2679 "parser.cc"
    break;

  case 244:
#line 1031 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as < std::string > ())); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2685 "parser.cc"
    break;

  case 245:
#line 1033 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_minus(make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2691 "parser.cc"
    break;

  case 246:
#line 1034 "parser.y"
                                   {}
#line 2697 "parser.cc"
    break;

  case 247:
#line 1035 "parser.y"
                                   {yylhs.value.as < expression_ref > () = make_fexp(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2703 "parser.cc"
    break;

  case 248:
#line 1037 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2709 "parser.cc"
    break;

  case 249:
#line 1038 "parser.y"
                                 {}
#line 2715 "parser.cc"
    break;

  case 254:
#line 1049 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2721 "parser.cc"
    break;

  case 255:
#line 1050 "parser.y"
                                 {}
#line 2727 "parser.cc"
    break;

  case 256:
#line 1051 "parser.y"
                                 {}
#line 2733 "parser.cc"
    break;

  case 257:
#line 1052 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2739 "parser.cc"
    break;

  case 258:
#line 1054 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_as_pattern(yystack_[2].value.as < std::string > (),yystack_[0].value.as < expression_ref > ());}
#line 2745 "parser.cc"
    break;

  case 259:
#line 1055 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lazy_pattern(yystack_[0].value.as < expression_ref > ());}
#line 2751 "parser.cc"
    break;

  case 260:
#line 1056 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_lambda(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 2757 "parser.cc"
    break;

  case 261:
#line 1057 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_let(yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2763 "parser.cc"
    break;

  case 262:
#line 1058 "parser.y"
                                 {}
#line 2769 "parser.cc"
    break;

  case 263:
#line 1059 "parser.y"
                                                       {yylhs.value.as < expression_ref > () = make_if(yystack_[6].value.as < expression_ref > (),yystack_[3].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 2775 "parser.cc"
    break;

  case 264:
#line 1060 "parser.y"
                                 {}
#line 2781 "parser.cc"
    break;

  case 265:
#line 1061 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_case(yystack_[2].value.as < expression_ref > (),make_alts(yystack_[0].value.as < std::vector<expression_ref> > ()));}
#line 2787 "parser.cc"
    break;

  case 266:
#line 1062 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_do(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2793 "parser.cc"
    break;

  case 267:
#line 1063 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_mdo(yystack_[0].value.as < std::vector<expression_ref> > ());}
#line 2799 "parser.cc"
    break;

  case 268:
#line 1064 "parser.y"
                                 {}
#line 2805 "parser.cc"
    break;

  case 269:
#line 1065 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2811 "parser.cc"
    break;

  case 270:
#line 1067 "parser.y"
                              {}
#line 2817 "parser.cc"
    break;

  case 271:
#line 1068 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2823 "parser.cc"
    break;

  case 272:
#line 1070 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].value.as < std::string > ());}
#line 2829 "parser.cc"
    break;

  case 273:
#line 1071 "parser.y"
                              {yylhs.value.as < expression_ref > () = make_id(yystack_[0].value.as < std::string > ());}
#line 2835 "parser.cc"
    break;

  case 274:
#line 1072 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2841 "parser.cc"
    break;

  case 275:
#line 1073 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2847 "parser.cc"
    break;

  case 276:
#line 1074 "parser.y"
                              {yylhs.value.as < expression_ref > () = yy_make_tuple(yystack_[1].value.as < std::vector<expression_ref> > ());}
#line 2853 "parser.cc"
    break;

  case 277:
#line 1075 "parser.y"
                              {}
#line 2859 "parser.cc"
    break;

  case 278:
#line 1076 "parser.y"
                              {}
#line 2865 "parser.cc"
    break;

  case 279:
#line 1077 "parser.y"
                              {yylhs.value.as < expression_ref > () = yystack_[1].value.as < expression_ref > ();}
#line 2871 "parser.cc"
    break;

  case 280:
#line 1078 "parser.y"
                              {yylhs.value.as < expression_ref > () = AST_node("WildcardPattern");}
#line 2877 "parser.cc"
    break;

  case 281:
#line 1083 "parser.y"
                      {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 2883 "parser.cc"
    break;

  case 282:
#line 1084 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as < std::vector<expression_ref> > ()),make_id(yystack_[0].value.as < std::string > ())});}
#line 2889 "parser.cc"
    break;

  case 283:
#line 1085 "parser.y"
                      {yylhs.value.as < expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].value.as < std::string > ()),make_infixexp(yystack_[0].value.as < std::vector<expression_ref> > ())});}
#line 2895 "parser.cc"
    break;

  case 284:
#line 1090 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2901 "parser.cc"
    break;

  case 285:
#line 1091 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2907 "parser.cc"
    break;

  case 286:
#line 1109 "parser.y"
                                 { yylhs.value.as < expression_ref > () = {AST_node("id",":"),yystack_[0].value.as < expression_ref > (),AST_node("id","[]")}; }
#line 2913 "parser.cc"
    break;

  case 287:
#line 1110 "parser.y"
                                 { yylhs.value.as < expression_ref > () = make_list(yystack_[0].value.as < std::vector<expression_ref> > ()); }
#line 2919 "parser.cc"
    break;

  case 288:
#line 1111 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as < expression_ref > ()}); }
#line 2925 "parser.cc"
    break;

  case 289:
#line 1112 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as < expression_ref > (),yystack_[1].value.as < expression_ref > ()}); }
#line 2931 "parser.cc"
    break;

  case 290:
#line 1113 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 2937 "parser.cc"
    break;

  case 291:
#line 1114 "parser.y"
                                 { yylhs.value.as < expression_ref > () = expression_ref(AST_node("enumFromThenTo"),{yystack_[4].value.as < expression_ref > (),yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()}); }
#line 2943 "parser.cc"
    break;

  case 292:
#line 1115 "parser.y"
                                 { auto quals = yystack_[0].value.as < std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 2949 "parser.cc"
    break;

  case 293:
#line 1117 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2955 "parser.cc"
    break;

  case 294:
#line 1118 "parser.y"
                                 { yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[2].value.as < expression_ref > ()); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2961 "parser.cc"
    break;

  case 295:
#line 1130 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2967 "parser.cc"
    break;

  case 296:
#line 1131 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2973 "parser.cc"
    break;

  case 297:
#line 1132 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2979 "parser.cc"
    break;

  case 298:
#line 1133 "parser.y"
                                          {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 2985 "parser.cc"
    break;

  case 299:
#line 1135 "parser.y"
                                                    {}
#line 2991 "parser.cc"
    break;

  case 300:
#line 1136 "parser.y"
                                                    {}
#line 2997 "parser.cc"
    break;

  case 301:
#line 1137 "parser.y"
                                                    {}
#line 3003 "parser.cc"
    break;

  case 302:
#line 1138 "parser.y"
                                                    {}
#line 3009 "parser.cc"
    break;

  case 303:
#line 1141 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3015 "parser.cc"
    break;

  case 304:
#line 1143 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > ();yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3021 "parser.cc"
    break;

  case 305:
#line 1144 "parser.y"
                                   {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3027 "parser.cc"
    break;

  case 306:
#line 1147 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3033 "parser.cc"
    break;

  case 307:
#line 1148 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3039 "parser.cc"
    break;

  case 308:
#line 1149 "parser.y"
                                 {}
#line 3045 "parser.cc"
    break;

  case 309:
#line 1150 "parser.y"
                                 {}
#line 3051 "parser.cc"
    break;

  case 310:
#line 1152 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3057 "parser.cc"
    break;

  case 311:
#line 1153 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[0].value.as < std::vector<expression_ref> > ();}
#line 3063 "parser.cc"
    break;

  case 312:
#line 1155 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3069 "parser.cc"
    break;

  case 313:
#line 1156 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3075 "parser.cc"
    break;

  case 314:
#line 1157 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3081 "parser.cc"
    break;

  case 315:
#line 1159 "parser.y"
                                 {yylhs.value.as < expression_ref > () = yy_make_alt(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3087 "parser.cc"
    break;

  case 316:
#line 1161 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_rhs(yystack_[1].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ());}
#line 3093 "parser.cc"
    break;

  case 317:
#line 1162 "parser.y"
                                 {yylhs.value.as < expression_ref > () = make_gdrhs(yystack_[1].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3099 "parser.cc"
    break;

  case 318:
#line 1164 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3105 "parser.cc"
    break;

  case 319:
#line 1165 "parser.y"
                                 {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3111 "parser.cc"
    break;

  case 320:
#line 1167 "parser.y"
                                 {}
#line 3117 "parser.cc"
    break;

  case 321:
#line 1168 "parser.y"
                                 {}
#line 3123 "parser.cc"
    break;

  case 322:
#line 1170 "parser.y"
                                 {yylhs.value.as < expression_ref > ()=make_gdrh(yystack_[2].value.as < std::vector<expression_ref> > (),yystack_[0].value.as < expression_ref > ());}
#line 3129 "parser.cc"
    break;

  case 323:
#line 1172 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3135 "parser.cc"
    break;

  case 324:
#line 1173 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3141 "parser.cc"
    break;

  case 325:
#line 1175 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3147 "parser.cc"
    break;

  case 326:
#line 1176 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3153 "parser.cc"
    break;

  case 327:
#line 1178 "parser.y"
              {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3159 "parser.cc"
    break;

  case 328:
#line 1179 "parser.y"
              {yylhs.value.as < expression_ref > () = make_strict_pattern(yystack_[0].value.as < expression_ref > ());}
#line 3165 "parser.cc"
    break;

  case 329:
#line 1181 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3171 "parser.cc"
    break;

  case 330:
#line 1182 "parser.y"
                    {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3177 "parser.cc"
    break;

  case 331:
#line 1185 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3183 "parser.cc"
    break;

  case 332:
#line 1186 "parser.y"
                               {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3189 "parser.cc"
    break;

  case 333:
#line 1188 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[2].value.as < std::vector<expression_ref> > (); yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3195 "parser.cc"
    break;

  case 334:
#line 1189 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > () = yystack_[1].value.as < std::vector<expression_ref> > ();}
#line 3201 "parser.cc"
    break;

  case 335:
#line 1190 "parser.y"
                       {yylhs.value.as < std::vector<expression_ref> > ().push_back(yystack_[0].value.as < expression_ref > ());}
#line 3207 "parser.cc"
    break;

  case 336:
#line 1191 "parser.y"
                       {}
#line 3213 "parser.cc"
    break;

  case 337:
#line 1196 "parser.y"
                        {yylhs.value.as < expression_ref > () = yystack_[0].value.as < expression_ref > ();}
#line 3219 "parser.cc"
    break;

  case 338:
#line 1197 "parser.y"
                        {yylhs.value.as < expression_ref > () = new expression(AST_node("Rec"),{yystack_[0].value.as < std::vector<expression_ref> > ()});}
#line 3225 "parser.cc"
    break;

  case 339:
#line 1199 "parser.y"
                        {yylhs.value.as < expression_ref > () = new expression(AST_node("PatQual"),{yystack_[2].value.as < expression_ref > (),yystack_[0].value.as < expression_ref > ()});}
#line 3231 "parser.cc"
    break;

  case 340:
#line 1200 "parser.y"
                        {yylhs.value.as < expression_ref > () = new expression(AST_node("SimpleQual"),{yystack_[0].value.as < expression_ref > ()});}
#line 3237 "parser.cc"
    break;

  case 341:
#line 1201 "parser.y"
                        {yylhs.value.as < expression_ref > () = new expression(AST_node("LetQual"),{yystack_[0].value.as < expression_ref > ()});}
#line 3243 "parser.cc"
    break;

  case 349:
#line 1246 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3249 "parser.cc"
    break;

  case 350:
#line 1247 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3255 "parser.cc"
    break;

  case 351:
#line 1249 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3261 "parser.cc"
    break;

  case 352:
#line 1250 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3267 "parser.cc"
    break;

  case 353:
#line 1252 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3273 "parser.cc"
    break;

  case 354:
#line 1253 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3279 "parser.cc"
    break;

  case 355:
#line 1254 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3285 "parser.cc"
    break;

  case 358:
#line 1259 "parser.y"
                            { yylhs.value.as < std::string > () =  "()"; }
#line 3291 "parser.cc"
    break;

  case 359:
#line 1260 "parser.y"
                                   { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3297 "parser.cc"
    break;

  case 360:
#line 1261 "parser.y"
                            { yylhs.value.as < std::string > () = "(##)"; }
#line 3303 "parser.cc"
    break;

  case 361:
#line 1262 "parser.y"
                                   { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3309 "parser.cc"
    break;

  case 362:
#line 1264 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3315 "parser.cc"
    break;

  case 363:
#line 1265 "parser.y"
                         { yylhs.value.as < std::string > () = "[]"; }
#line 3321 "parser.cc"
    break;

  case 364:
#line 1267 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3327 "parser.cc"
    break;

  case 365:
#line 1268 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3333 "parser.cc"
    break;

  case 366:
#line 1270 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3339 "parser.cc"
    break;

  case 367:
#line 1271 "parser.y"
                      { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3345 "parser.cc"
    break;

  case 368:
#line 1274 "parser.y"
                     { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3351 "parser.cc"
    break;

  case 369:
#line 1275 "parser.y"
                    { yylhs.value.as < std::string > () = "()"; }
#line 3357 "parser.cc"
    break;

  case 370:
#line 1276 "parser.y"
                    { yylhs.value.as < std::string > () = "(##)"; }
#line 3363 "parser.cc"
    break;

  case 371:
#line 1278 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3369 "parser.cc"
    break;

  case 372:
#line 1279 "parser.y"
                          { yylhs.value.as < std::string > () = "("+std::string(yystack_[1].value.as < int > (),',')+")"; }
#line 3375 "parser.cc"
    break;

  case 373:
#line 1280 "parser.y"
                          { yylhs.value.as < std::string > () = "(#"+std::string(yystack_[1].value.as < int > (),',')+"#)"; }
#line 3381 "parser.cc"
    break;

  case 374:
#line 1281 "parser.y"
                          { yylhs.value.as < std::string > () = "->"; }
#line 3387 "parser.cc"
    break;

  case 375:
#line 1282 "parser.y"
                          { yylhs.value.as < std::string > () = "[]"; }
#line 3393 "parser.cc"
    break;

  case 376:
#line 1284 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3399 "parser.cc"
    break;

  case 377:
#line 1285 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3405 "parser.cc"
    break;

  case 378:
#line 1286 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3411 "parser.cc"
    break;

  case 379:
#line 1288 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3417 "parser.cc"
    break;

  case 380:
#line 1289 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3423 "parser.cc"
    break;

  case 381:
#line 1290 "parser.y"
                           { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3429 "parser.cc"
    break;

  case 382:
#line 1291 "parser.y"
                           { yylhs.value.as < std::string > () = ":"; }
#line 3435 "parser.cc"
    break;

  case 383:
#line 1292 "parser.y"
                           { yylhs.value.as < std::string > () = "~"; }
#line 3441 "parser.cc"
    break;

  case 384:
#line 1295 "parser.y"
                         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3447 "parser.cc"
    break;

  case 385:
#line 1296 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3453 "parser.cc"
    break;

  case 386:
#line 1298 "parser.y"
                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3459 "parser.cc"
    break;

  case 387:
#line 1300 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3465 "parser.cc"
    break;

  case 388:
#line 1301 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3471 "parser.cc"
    break;

  case 389:
#line 1305 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3477 "parser.cc"
    break;

  case 390:
#line 1307 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3483 "parser.cc"
    break;

  case 391:
#line 1308 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3489 "parser.cc"
    break;

  case 392:
#line 1309 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3495 "parser.cc"
    break;

  case 393:
#line 1311 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3501 "parser.cc"
    break;

  case 394:
#line 1312 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3507 "parser.cc"
    break;

  case 395:
#line 1313 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3513 "parser.cc"
    break;

  case 396:
#line 1314 "parser.y"
                 { yylhs.value.as < std::string > () = "-"; }
#line 3519 "parser.cc"
    break;

  case 397:
#line 1319 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3525 "parser.cc"
    break;

  case 398:
#line 1320 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3531 "parser.cc"
    break;

  case 399:
#line 1322 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3537 "parser.cc"
    break;

  case 400:
#line 1323 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3543 "parser.cc"
    break;

  case 401:
#line 1325 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3549 "parser.cc"
    break;

  case 402:
#line 1326 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3555 "parser.cc"
    break;

  case 403:
#line 1327 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3561 "parser.cc"
    break;

  case 404:
#line 1329 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3567 "parser.cc"
    break;

  case 405:
#line 1330 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3573 "parser.cc"
    break;

  case 406:
#line 1331 "parser.y"
                { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3579 "parser.cc"
    break;

  case 407:
#line 1333 "parser.y"
                      { yylhs.value.as < std::string > () = "_"; }
#line 3585 "parser.cc"
    break;

  case 408:
#line 1335 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3591 "parser.cc"
    break;

  case 409:
#line 1336 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3597 "parser.cc"
    break;

  case 410:
#line 1338 "parser.y"
                           { yylhs.value.as < std::string > () =yystack_[0].value.as < std::string > (); }
#line 3603 "parser.cc"
    break;

  case 411:
#line 1339 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3609 "parser.cc"
    break;

  case 412:
#line 1343 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3615 "parser.cc"
    break;

  case 413:
#line 1345 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3621 "parser.cc"
    break;

  case 414:
#line 1347 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3627 "parser.cc"
    break;

  case 415:
#line 1348 "parser.y"
                          { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3633 "parser.cc"
    break;

  case 416:
#line 1349 "parser.y"
                          { yylhs.value.as < std::string > () = "unsafe"; }
#line 3639 "parser.cc"
    break;

  case 417:
#line 1350 "parser.y"
                          { yylhs.value.as < std::string > () = "safe"; }
#line 3645 "parser.cc"
    break;

  case 418:
#line 1351 "parser.y"
                          { yylhs.value.as < std::string > () = "interruptible"; }
#line 3651 "parser.cc"
    break;

  case 419:
#line 1354 "parser.y"
           { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3657 "parser.cc"
    break;

  case 420:
#line 1355 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3663 "parser.cc"
    break;

  case 421:
#line 1357 "parser.y"
             { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3669 "parser.cc"
    break;

  case 422:
#line 1358 "parser.y"
                 {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3675 "parser.cc"
    break;

  case 423:
#line 1359 "parser.y"
                   {yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 3681 "parser.cc"
    break;

  case 424:
#line 1361 "parser.y"
              { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3687 "parser.cc"
    break;

  case 425:
#line 1362 "parser.y"
         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3693 "parser.cc"
    break;

  case 426:
#line 1364 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3699 "parser.cc"
    break;

  case 427:
#line 1365 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3705 "parser.cc"
    break;

  case 428:
#line 1366 "parser.y"
                    { yylhs.value.as < std::string > () = "unsafe"; }
#line 3711 "parser.cc"
    break;

  case 429:
#line 1367 "parser.y"
                    { yylhs.value.as < std::string > () = "safe"; }
#line 3717 "parser.cc"
    break;

  case 430:
#line 1368 "parser.y"
                    { yylhs.value.as < std::string > () = "interruptible"; }
#line 3723 "parser.cc"
    break;

  case 431:
#line 1369 "parser.y"
                    { yylhs.value.as < std::string > () = "forall"; }
#line 3729 "parser.cc"
    break;

  case 432:
#line 1370 "parser.y"
                    { yylhs.value.as < std::string > () = "family"; }
#line 3735 "parser.cc"
    break;

  case 433:
#line 1371 "parser.y"
                    { yylhs.value.as < std::string > () = "role"; }
#line 3741 "parser.cc"
    break;

  case 434:
#line 1373 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3747 "parser.cc"
    break;

  case 435:
#line 1374 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3753 "parser.cc"
    break;

  case 436:
#line 1376 "parser.y"
                                  {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3759 "parser.cc"
    break;

  case 437:
#line 1377 "parser.y"
                           {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3765 "parser.cc"
    break;

  case 438:
#line 1379 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3771 "parser.cc"
    break;

  case 439:
#line 1381 "parser.y"
                         { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3777 "parser.cc"
    break;

  case 440:
#line 1382 "parser.y"
                         { yylhs.value.as < std::string > () = "-"; }
#line 3783 "parser.cc"
    break;

  case 441:
#line 1384 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3789 "parser.cc"
    break;

  case 442:
#line 1385 "parser.y"
                             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3795 "parser.cc"
    break;

  case 443:
#line 1387 "parser.y"
                          { yylhs.value.as < std::string > () = "as"; }
#line 3801 "parser.cc"
    break;

  case 444:
#line 1388 "parser.y"
                          { yylhs.value.as < std::string > () = "qualified"; }
#line 3807 "parser.cc"
    break;

  case 445:
#line 1389 "parser.y"
                          { yylhs.value.as < std::string > () = "hiding"; }
#line 3813 "parser.cc"
    break;

  case 446:
#line 1390 "parser.y"
                          { yylhs.value.as < std::string > () = "export"; }
#line 3819 "parser.cc"
    break;

  case 447:
#line 1391 "parser.y"
                          { yylhs.value.as < std::string > () = "label"; }
#line 3825 "parser.cc"
    break;

  case 448:
#line 1392 "parser.y"
                          { yylhs.value.as < std::string > () = "dynamic"; }
#line 3831 "parser.cc"
    break;

  case 449:
#line 1393 "parser.y"
                          { yylhs.value.as < std::string > () = "stdcall"; }
#line 3837 "parser.cc"
    break;

  case 450:
#line 1394 "parser.y"
                          { yylhs.value.as < std::string > () = "ccall"; }
#line 3843 "parser.cc"
    break;

  case 451:
#line 1395 "parser.y"
                          { yylhs.value.as < std::string > () = "capi"; }
#line 3849 "parser.cc"
    break;

  case 452:
#line 1396 "parser.y"
                          { yylhs.value.as < std::string > () = "prim"; }
#line 3855 "parser.cc"
    break;

  case 453:
#line 1397 "parser.y"
                          { yylhs.value.as < std::string > () = "javascript"; }
#line 3861 "parser.cc"
    break;

  case 454:
#line 1398 "parser.y"
                          { yylhs.value.as < std::string > () = "group"; }
#line 3867 "parser.cc"
    break;

  case 455:
#line 1399 "parser.y"
                          { yylhs.value.as < std::string > () = "stock"; }
#line 3873 "parser.cc"
    break;

  case 456:
#line 1400 "parser.y"
                          { yylhs.value.as < std::string > () = "anyclass"; }
#line 3879 "parser.cc"
    break;

  case 457:
#line 1401 "parser.y"
                          { yylhs.value.as < std::string > () = "via"; }
#line 3885 "parser.cc"
    break;

  case 458:
#line 1402 "parser.y"
                          { yylhs.value.as < std::string > () = "unit"; }
#line 3891 "parser.cc"
    break;

  case 459:
#line 1403 "parser.y"
                          { yylhs.value.as < std::string > () = "dependency"; }
#line 3897 "parser.cc"
    break;

  case 460:
#line 1404 "parser.y"
                          { yylhs.value.as < std::string > () = "signature"; }
#line 3903 "parser.cc"
    break;

  case 461:
#line 1406 "parser.y"
                 { yylhs.value.as < std::string > () = "!"; }
#line 3909 "parser.cc"
    break;

  case 462:
#line 1407 "parser.y"
                 { yylhs.value.as < std::string > () = "."; }
#line 3915 "parser.cc"
    break;

  case 463:
#line 1408 "parser.y"
                 { yylhs.value.as < std::string > () = "*"; }
#line 3921 "parser.cc"
    break;

  case 464:
#line 1412 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3927 "parser.cc"
    break;

  case 465:
#line 1413 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3933 "parser.cc"
    break;

  case 466:
#line 1415 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3939 "parser.cc"
    break;

  case 467:
#line 1417 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3945 "parser.cc"
    break;

  case 468:
#line 1418 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3951 "parser.cc"
    break;

  case 469:
#line 1420 "parser.y"
                 { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 3957 "parser.cc"
    break;

  case 470:
#line 1421 "parser.y"
                 { yylhs.value.as < std::string > () = ":"; }
#line 3963 "parser.cc"
    break;

  case 471:
#line 1425 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < char > ();}
#line 3969 "parser.cc"
    break;

  case 472:
#line 1426 "parser.y"
                  {yylhs.value.as < expression_ref > () = yy_make_string(yystack_[0].value.as < std::string > ());}
#line 3975 "parser.cc"
    break;

  case 473:
#line 1427 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < int > ();}
#line 3981 "parser.cc"
    break;

  case 474:
#line 1428 "parser.y"
                  {yylhs.value.as < expression_ref > () = yystack_[0].value.as < double > ();}
#line 3987 "parser.cc"
    break;

  case 476:
#line 1436 "parser.y"
      { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 3993 "parser.cc"
    break;

  case 477:
#line 1440 "parser.y"
             {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 3999 "parser.cc"
    break;

  case 478:
#line 1441 "parser.y"
         {yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > ();}
#line 4005 "parser.cc"
    break;

  case 479:
#line 1443 "parser.y"
                   {yylhs.value.as < int > () = yystack_[1].value.as < int > () + 1;}
#line 4011 "parser.cc"
    break;

  case 480:
#line 1444 "parser.y"
                   {yylhs.value.as < int > () = 1;}
#line 4017 "parser.cc"
    break;


#line 4021 "parser.cc"

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


  const short parser::yypact_ninf_ = -622;

  const short parser::yytable_ninf_ = -440;

  const short
  parser::yypact_[] =
  {
      58,   171,  -622,   102,  -622,  -622,  -622,  -622,  -622,   143,
      84,    56,  -622,    71,    73,    73,     7,  -622,  -622,  -622,
    -622,   222,  -622,  -622,  -622,   116,  -622,   174,   182,  4492,
     254,   255,   186,  -622,   714,  -622,    78,  -622,  -622,  -622,
    -622,   171,  -622,   154,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,   103,
    -622,  -622,  -622,  -622,  -622,  -622,   479,  -622,  -622,  -622,
    -622,   248,   218,  -622,   233,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,    14,   289,   336,  -622,   261,  -622,  2590,
    4057,  -622,   270,   249,  1736,  -622,  -622,  -622,   319,   306,
    -622,  4057,  4747,   249,  3444,  4837,  3444,   292,   294,  4701,
      96,  3078,  3444,  3200,  3444,  1356,  1098,  1227,  -622,  -622,
    -622,  -622,  -622,  -622,    72,   292,   269,   186,  -622,  -622,
    -622,   346,  -622,  -622,  -622,  -622,   204,  -622,  3322,  -622,
     326,  -622,  -622,  -622,  -622,  -622,   315,   349,   324,  -622,
    -622,  -622,  -622,   312,  -622,   296,  -622,  -622,   340,   149,
     157,  -622,   343,   350,  -622,  -622,  -622,  -622,  -622,   368,
    -622,   370,   377,   382,  -622,  -622,  -622,  4492,  4539,  -622,
    -622,  -622,  -622,  -622,  -622,   454,  -622,   -25,  1098,   471,
     494,  -622,  -622,  2590,  -622,  -622,  -622,   358,   364,  -622,
    -622,  -622,  -622,  -622,  4984,  3751,  3547,  3649,  4597,  -622,
    -622,  -622,  -622,  -622,   476,  4391,  -622,   128,   414,  -622,
     221,  4057,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  3853,  1858,  1858,  -622,  2346,   423,   399,
      59,  -622,  -622,   439,   440,   443,   444,  3853,   845,   845,
    -622,   508,   442,   438,   369,  5030,   396,   398,  -622,  -622,
    -622,   445,   -14,   219,  4930,   446,  -622,     2,  -622,  -622,
      11,  4701,  -622,   451,   163,    -2,   415,   450,  1980,  3444,
    -622,  -622,  2956,  -622,  3322,   224,  -622,  -622,  4159,  -622,
    -622,  -622,   494,    80,   430,   422,  -622,  2590,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,  3200,  -622,  -622,   129,   144,
     377,   431,   436,   437,   156,  -622,   223,   229,   232,  3853,
    4701,  4701,  -622,   390,   261,   420,  4057,  3853,  4159,   224,
    -622,  2834,  -622,  -622,  -622,  -622,  -622,  4391,  -622,  4644,
    4984,  3444,  -622,   441,   447,   437,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  -622,   452,   458,  -622,  -622,   453,    71,
    -622,   424,   487,   489,   309,  3853,  2590,  -622,  -622,  -622,
      65,   468,   461,  -622,  -622,  -622,  -622,   473,   490,  -622,
     474,   441,  -622,    16,   469,   447,   167,  -622,   503,   235,
     478,   243,   475,   477,   306,  -622,  -622,  -622,  4057,  3853,
    -622,  -622,   481,   480,   306,   249,  3444,   510,   511,    87,
    -622,  -622,    54,   507,   483,  -622,    97,  -622,   576,  -622,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,   476,    98,  -622,
    -622,   582,    64,  2590,  3853,   493,   492,   484,   495,  2590,
     496,  2468,  2468,  5030,    96,  -622,  5030,  3853,   497,  5030,
    -622,   498,   504,   544,  -622,  -622,   545,   397,   547,  1614,
     976,  -622,  -622,  2590,  -622,  2590,  2346,  -622,    48,  -622,
     513,   514,   515,  2590,  2590,  2102,  1485,  -622,  1485,   239,
    -622,  1485,  -622,  1485,   512,  -622,  -622,  -622,  -622,  -622,
    -622,   548,   551,   552,  4883,   520,  -622,  -622,  -622,    10,
     300,  -622,  -622,   283,  -622,   521,  -622,  -622,  -622,  -622,
     538,  -622,   525,   560,    82,  -622,  -622,  -622,  -622,  4539,
    -622,  -622,  -622,   171,  -622,  -622,  -622,  -622,  -622,  3853,
    4984,  -622,  4984,  5076,  -622,  3853,  -622,  3853,  -622,  3853,
    -622,  3853,  -622,  3853,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,   508,  -622,  -622,  2590,  -622,  1858,  -622,  2590,
    2346,  -622,  2590,  -622,  -622,   845,  -622,  -622,  -622,  -622,
    -622,  -622,   516,   517,  -622,  -622,  3444,  -622,  -622,   616,
     543,  5030,  -622,  -622,  -622,   531,  -622,   553,  -622,  -622,
    -622,   555,   695,   251,  -622,  -622,  -622,  -622,  2224,   556,
     535,  -622,   236,    71,  -622,  -622,   476,   570,  -622,  -622,
    -622,  -622,  -622,  -622,  2712,   541,  -622,  -622,   586,  -622,
    -622,  -622,  -622,  -622,  3853,  3853,   390,  -622,   583,  3853,
     639,  -622,   660,  -622,  -622,  4644,  1485,  3853,   562,   667,
    -622,  -622,  -622,  3853,  5157,  -622,  -622,  -622,  -622,   573,
     574,   503,  -622,  -622,  -622,  -622,  -622,  -622,   399,  -622,
    -622,  -622,  -622,   333,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  2468,  2590,  -622,    51,  -622,  -622,  2590,
     417,   631,  2102,  2590,  -622,    -7,    27,  -622,  -622,  -622,
    -622,   603,  -622,  4391,    37,  -622,   660,  -622,  -622,  -622,
    -622,  -622,   171,    61,  -622,   613,  -622,  -622,   684,   845,
     845,  -622,   476,  -622,  -622,  2590,  2590,  2590,  -622,  -622,
    -622,  -622,  3853,  -622,  5110,   639,   606,  4205,  -622,  -622,
    -622,  -622,  -622,  -622,  3955,   180,   643,  -622,  -622,  -622,
    -622,   591,  4492,  -622,  -622,  3853,  2590,   133,    64,  -622,
     651,  -622,  -622,  -622,  -622,  -622,  4391,  -622,  4391,  -622,
    -622,   589,   592,  -622,  4057,  -622,  4492,   596,   600,  -622,
    -622,  -622,  2590,  4298,  -622,  4391,  4057,  -622,  -622,   602,
    -622,  -622,  -622,  -622,  -622
  };

  const short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   477,   478,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    68,   476,   475,    12,   127,   123,     0,     0,     0,
       0,    46,    41,    15,    14,   126,     0,     6,     7,   443,
     445,     0,   444,     0,   431,   446,   447,   448,   429,   430,
     428,   432,   433,   449,   450,   451,   452,   453,   454,     0,
     455,   456,   457,   458,   460,   459,     0,   426,   389,   425,
     387,     0,    19,    21,    25,    33,    36,   379,   388,    35,
     421,   424,   427,     0,     0,    48,    38,    42,   280,     0,
       0,    93,     0,     0,     0,    61,    62,    63,    88,     0,
      94,     0,     0,     0,     0,     0,     0,   235,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   466,   465,
     471,   472,   473,   474,   235,   235,    59,    66,    69,    70,
      71,   101,   217,   227,    73,   214,    74,   243,   247,   257,
     269,   271,   273,   349,   362,   350,     0,   272,   424,   351,
     464,   274,   124,     0,    23,     0,    34,   376,     0,     0,
       0,    24,     0,     0,   440,   461,   463,   462,   441,     0,
     438,     0,     0,     0,   439,   442,    17,     0,    27,    22,
      40,    40,     3,    45,    47,    52,    37,     0,     0,     0,
     240,   248,   241,     0,   417,   418,   416,     0,     0,   395,
     142,   396,   141,   165,   199,     0,     0,     0,     0,   414,
     394,   393,   391,   390,   122,     0,   138,   139,     0,    98,
     151,   154,   157,   159,   163,   371,   160,   384,   392,   164,
     161,   412,   415,   180,   336,   336,   266,     0,     0,   251,
       0,   264,   319,     0,     0,     0,     0,     0,   117,   117,
     120,     0,     0,   151,     0,     0,     0,     0,   419,   399,
     267,     0,     0,     0,   108,     0,   355,     0,   353,   256,
       0,     0,   236,     0,     0,     0,   356,   130,     0,     0,
     327,   330,     0,   259,   245,     0,   470,   363,     0,   469,
     468,   281,   240,   286,     0,   287,   405,     0,   406,   404,
     410,   437,   436,   366,   467,   440,   358,   480,     0,     0,
     437,     0,   436,   366,     0,   360,     0,     0,     0,     0,
       0,     0,    60,     0,    67,     0,     0,     0,     0,     0,
     402,     0,   403,   401,   408,   435,   434,     0,   254,   343,
       0,     0,   125,     0,     0,     0,   382,   383,   381,   380,
     423,   422,    20,    32,     0,    28,    30,    31,     0,     0,
      51,    50,     0,     0,     0,     0,     0,   249,   143,   144,
       0,     0,   200,   202,   135,   186,   375,     0,     0,   147,
       0,   142,   168,   181,     0,   384,     0,   170,   181,     0,
       0,     0,     0,     0,     0,    79,   166,   140,     0,     0,
     158,   181,     0,   179,     0,     0,     0,   340,     0,     0,
     335,   337,     0,     0,   303,   305,     0,   250,     0,   318,
     321,    85,    84,    86,    87,   176,   132,   122,     0,   218,
     116,   128,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   105,   108,     0,     0,     0,
     364,     0,     0,     0,   246,   230,     0,     0,     0,     0,
       0,   262,   328,     0,   329,     0,     0,   215,   122,   222,
       0,     0,     0,   282,   288,     0,     0,   279,     0,   283,
     275,     0,   276,     0,   422,   352,   359,   479,   277,   278,
     361,     0,     0,     0,     0,   226,   398,    65,   397,     0,
      95,   129,   224,   148,   133,     0,   216,   244,   255,   346,
       0,   342,   345,   348,     0,   258,   378,   377,    26,     0,
       9,    10,    49,     0,   253,   252,   265,   239,   242,     0,
       0,   167,     0,     0,   173,     0,   374,     0,   174,     0,
     372,     0,   171,     0,   172,   373,   385,   413,   121,    97,
     152,    72,   341,   338,   326,     0,   331,   334,   332,     0,
       0,   320,     0,    83,   118,   115,   119,   261,   148,    80,
     420,   400,    78,    76,   268,   354,     0,   323,   102,   103,
       0,   108,   357,   109,   113,     0,   106,     0,   237,   229,
     231,     0,     0,     0,   131,   368,   228,   308,     0,     0,
     310,   314,     0,     0,   309,   260,   122,     0,   220,   221,
     407,   411,   367,   290,     0,   292,   297,   298,   281,   294,
     293,   285,   284,   234,     0,     0,     0,   100,     0,     0,
     196,    82,   204,   409,   270,     0,     0,     0,     0,    54,
     203,   134,   201,     0,     0,   185,   187,   146,   189,     0,
     182,   183,   184,   182,   339,   333,   322,   304,   251,   114,
      77,    75,   324,     0,   104,   107,   110,   365,   238,   369,
     370,   311,   306,   313,     0,   315,   122,   307,   219,     0,
     454,   299,     0,   289,   136,     0,     0,    64,    99,    96,
     186,   190,   192,     0,     0,    81,   205,   207,   344,   347,
     225,    29,     0,    56,   145,     0,   175,   169,     0,   117,
     117,   312,   122,   317,   223,     0,     0,     0,   295,   296,
     291,   232,     0,   233,     0,   196,     0,   197,   155,   162,
     194,    91,    89,    90,     0,     0,   208,   211,   386,   206,
      53,     0,     0,    44,    55,     0,     0,     0,     0,   316,
       0,   301,   300,   137,   195,   191,     0,   156,     0,   212,
     153,   177,     0,   209,     0,   210,     0,     0,     0,   263,
     111,   112,     0,   197,   193,   198,     0,   213,    92,     0,
      57,   188,   302,   178,    58
  };

  const short
  parser::yypgoto_[] =
  {
    -622,  -622,  -622,  -622,  -622,  -622,  -622,    69,  -622,  -622,
    -563,  -622,   537,  -622,  -622,  -622,   193,  -166,  -622,   598,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,  -622,
    -622,  -622,  -622,  -622,  -622,   402,  -622,  -622,  -622,  -389,
    -622,  -622,  -622,  -238,  -622,  -291,  -398,   701,  -622,  -622,
    -622,  -307,  -410,   383,   104,  -622,   518,  -622,   -95,   307,
     -57,  -622,   -83,  -622,  -100,  -320,  -622,   523,  -621,  -194,
     455,   -16,  -622,  -124,   238,    83,  -622,  -581,  -622,  -622,
      38,  -622,     6,  -622,  -622,   244,  -622,  -622,    85,    45,
     749,   220,   459,  -622,   321,  -622,   344,  -622,   -62,   -92,
     753,   -24,  -302,   132,  -622,   -77,   -61,  -622,  -622,   -76,
     675,  -622,  -622,  -622,   111,   328,  -622,   432,  -414,  -622,
     122,  -622,  -212,  -622,  -226,    33,  -622,   519,  -622,   -69,
     563,   242,  -218,  -622,   165,  -622,   743,  -622,   702,   -88,
    -622,   -56,  -264,  -105,  -622,   351,   766,  -622,  -622,  -622,
     -27,  -622,  -138,  -622,   185,   710,  -152,  -622,   -96,  -622,
    -622,  -489,  -622,   605,   -74,   -29,  -202,   -18,  -622,  -622,
       1,   -60,   -54,   -86,  -622,  -196,   -75,   -35,  -254,  -622,
    -193,   -36,  -108
  };

  const short
  parser::yydefgoto_[] =
  {
      -1,     3,     4,     5,    16,   182,     6,    10,    19,    30,
      71,    72,    73,   179,   354,   355,   356,    74,    75,    87,
      11,    20,    21,    32,    85,   185,   523,   361,   703,   743,
     744,   323,   126,   495,    33,    34,   127,   128,   129,   130,
     247,   735,   765,   131,   631,   214,   326,   132,   263,   445,
     580,   664,   133,   428,   250,   251,   395,    27,    36,   329,
     458,   425,   502,   370,   685,   215,   216,   217,   426,   504,
     378,   726,   379,   761,   220,   727,   221,   222,   728,   223,
     427,   762,   402,   384,   390,   533,   645,   649,   632,   691,
     692,   693,   730,   371,   372,   373,   695,   696,   697,   736,
     429,   430,   467,   468,   469,   135,   271,   272,   291,   190,
     431,   191,   192,   418,   193,   138,   139,   140,   141,   308,
     309,   294,   295,   615,   616,   413,   414,   461,   599,   600,
     601,   675,   240,   241,   242,   602,   408,   281,   282,   236,
     409,   410,   411,   510,   511,   512,   142,   143,   276,   265,
     144,   145,   496,   296,   594,   224,   225,    76,   226,   737,
     157,    78,   227,   228,   497,   498,   331,   297,   298,   333,
     299,   229,   230,   231,   146,   147,    80,    81,   334,   300,
     301,   336,   174,    82,   175,   149,   150,   303,   304,   151,
      24,     9,   314
  };

  const short
  parser::yytable_[] =
  {
      79,   253,    77,   449,   232,   154,   173,   219,   440,   318,
     137,   432,   357,   450,   419,   232,   148,   344,   252,   415,
     501,   396,   277,   292,   292,   292,   416,   189,   257,   563,
     268,   330,   239,   218,   260,   268,   284,   584,   366,   293,
     332,   316,   259,   261,   646,   269,   603,   420,   689,   266,
     280,   283,   318,   285,   266,    22,   311,   583,   527,   731,
      22,   302,   312,   302,   528,    22,   286,   172,   385,   450,
     608,   721,    22,   741,    13,   394,   455,   338,   394,     1,
     275,   313,   286,   389,   258,   330,   471,   267,   627,   732,
     733,   319,   472,   362,   332,   306,   292,   537,   386,   391,
     451,   307,    12,   548,   363,   723,   757,   289,   722,   403,
     377,   383,   388,   552,   180,    29,   181,   310,   448,   232,
     232,   232,   232,   289,   345,   538,   505,   456,   173,   232,
     374,   367,   472,   466,   312,   232,   237,   335,   401,   628,
     473,   452,   722,   508,   237,   734,   529,   232,    79,    79,
      77,    77,   757,   313,   757,   705,    68,    23,     2,   474,
      70,   232,    23,   637,   768,   475,   521,    23,   557,   742,
      18,   528,   407,   407,    23,   407,   270,    25,   565,   767,
     530,   392,   237,   152,   671,    17,   258,   330,   556,   310,
     419,   335,   666,   153,   435,   476,   332,   530,   561,   564,
     158,   557,    26,   779,   262,   479,   160,   158,   678,    14,
      15,   159,   565,   160,   173,   118,   189,   200,   462,   558,
     202,   280,   118,   338,   137,   137,   119,   700,   284,   286,
     148,   148,   503,   232,   770,   646,    31,   436,   480,   566,
     232,   232,   453,   219,   481,    35,   446,   565,   415,   358,
     359,   232,    37,   482,   164,   165,   166,   617,   306,   483,
      38,   167,   155,   259,   307,   486,   374,   604,   315,   218,
     289,   487,   307,    68,   290,   172,   540,    70,   713,   232,
     515,    83,   487,   168,   286,   327,  -128,   170,   734,  -128,
       7,   492,   493,   335,     8,   164,   165,   166,   253,    68,
      86,   441,   167,    70,   550,   442,   465,   507,   399,   466,
     513,  -149,   232,   232,   749,   549,    84,   684,   684,   286,
     328,   237,   258,   674,   168,   289,   330,   366,   170,   290,
     164,   165,   166,   177,   488,   332,   553,   167,   481,   568,
     489,   178,   657,   490,   483,   554,   542,   487,   232,   234,
     543,   235,   568,   357,   545,   328,   582,   176,   487,   168,
     289,   232,   670,   170,   290,  -132,   307,   183,  -132,   268,
     184,   567,   450,   585,   330,   186,   199,   574,   233,   577,
     577,   629,   630,   332,   292,   343,   292,   201,   266,   292,
     676,   292,   243,   244,   245,   246,   270,   577,   577,   322,
     619,   605,   620,   606,   407,   621,   248,   622,   249,   459,
     677,   460,   613,   407,   618,   753,   210,   211,   325,   585,
     212,   213,   302,   273,   302,   581,   339,   302,   446,   302,
     340,   586,   335,   709,   640,   710,   773,   341,   775,  -419,
     647,   342,   648,   232,   650,   287,   651,   232,   653,   232,
     419,   232,   346,   232,   344,   232,   641,   232,   374,   347,
     164,   165,   166,   758,   719,   715,   716,   167,   320,   321,
     286,   747,   748,   450,   578,   579,   436,   348,   360,   349,
     335,   164,   165,   166,   386,   391,   350,   639,   167,   168,
      79,   351,    77,   654,   364,   407,   368,   656,   407,   729,
     658,   591,   369,   394,   398,   592,   494,   593,   237,   758,
     168,   289,   258,   417,   258,   662,    68,   421,   422,   450,
      70,   423,   424,   433,   434,   399,   437,   447,   438,   454,
     444,   457,   439,   729,   648,   477,   577,   478,   232,   232,
     484,   137,   568,   232,   292,  -439,   485,   148,   704,   499,
     516,   232,   681,   522,   520,   771,   517,   232,   232,   162,
     699,   518,   729,   446,   729,   524,   259,   525,   163,   531,
     164,   165,   166,   519,   286,   365,   532,   167,   534,   729,
     535,   729,   302,   536,   539,   164,   165,   166,   541,   544,
     551,   546,   167,   547,   559,   543,  -325,   555,   560,   168,
     169,   562,   570,   170,   171,   575,   513,   232,   571,   588,
     328,   577,   712,   572,   168,   289,   118,   714,   170,   290,
     407,   720,   589,   590,   573,   596,   623,  -420,   587,   610,
     611,   612,   624,   625,   253,   626,   232,   633,   232,   634,
     635,   232,   636,   663,   665,   660,   661,   667,   232,   673,
     648,   760,   679,   750,   751,   752,   682,   672,   668,   232,
     376,   688,   286,   327,   253,   683,   740,   738,   690,   694,
     232,   702,   232,   164,   165,   166,   253,   701,   232,   717,
     167,   778,   706,   707,   769,   137,   137,   232,   725,   232,
     232,   148,   148,   760,   745,   746,   756,   764,   328,   766,
     772,   777,   168,   289,   776,   780,   170,   290,   738,   781,
     782,   784,   638,    79,   352,    77,    28,    88,    39,    89,
      90,    91,    92,   514,    93,   324,    40,    94,   500,   686,
      95,    96,    97,    98,    99,   397,   100,    79,    42,    77,
     101,   569,   102,    44,   400,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
     783,    58,   774,   755,   105,   106,    60,    61,    62,    63,
      64,    65,   107,   724,   491,   199,   642,   108,   109,   652,
     763,   739,   380,   134,   343,   659,   201,   136,   506,   609,
     708,   110,   317,   718,   607,   711,   526,   111,   412,   655,
     698,   464,   161,   112,   669,   113,   114,   264,   595,   156,
     307,   687,   256,   393,     0,   210,   211,     0,   115,   212,
     213,     0,   116,     0,   117,     0,     0,     0,     0,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,     0,
       0,     0,   120,   121,   122,   123,     0,     0,    88,    39,
      89,     0,     0,   124,   125,    93,     0,    40,    94,     0,
       0,    95,    96,    97,     0,    99,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,    55,    56,    57,
     104,     0,    58,     0,     0,   105,   106,    60,    61,    62,
      63,    64,    65,   107,     0,     0,     0,     0,   108,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,   111,     0,
       0,     0,     0,     0,   112,     0,   113,   114,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   116,     0,   117,     0,     0,     0,     0,
       0,     0,     0,    67,   118,     0,     0,    69,   119,     0,
       0,     0,     0,   120,   121,   122,   123,    22,     0,    88,
      39,    89,     0,     0,   124,   125,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,   106,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   108,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,   112,     0,   113,   576,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    23,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
     598,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,    89,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   286,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   305,
     165,   166,     0,     0,     0,     0,   167,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,   306,   117,     0,
       0,     0,     0,   307,   288,     0,    67,   118,   168,   289,
      69,   119,   170,   290,     0,     0,   120,   121,   122,   123,
      88,    39,    89,     0,     0,     0,     0,    93,     0,    40,
      94,     0,     0,     0,     0,     0,     0,    99,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   103,    51,    52,    53,    54,    55,
      56,    57,   104,     0,    58,     0,     0,     0,   106,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     108,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   286,     0,     0,
     111,     0,     0,     0,     0,     0,   112,     0,   113,   165,
     166,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,   115,     0,     0,     0,   188,     0,   117,   315,     0,
       0,     0,   307,   288,     0,    67,   118,   168,   289,    69,
     119,   170,   290,     0,     0,   120,   121,   122,   123,    88,
      39,    89,     0,     0,     0,     0,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,   106,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   108,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   286,     0,     0,   111,
       0,     0,     0,     0,     0,   112,     0,   113,   165,   166,
       0,     0,     0,     0,   167,     0,     0,     0,     0,     0,
     115,   287,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,   288,     0,    67,   118,   168,   289,    69,   119,
     170,   290,     0,     0,   120,   121,   122,   123,    88,    39,
      89,     0,     0,     0,     0,    93,     0,    40,    94,     0,
       0,     0,     0,     0,     0,    99,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   103,    51,    52,    53,    54,    55,    56,    57,
     104,     0,    58,     0,     0,     0,   106,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,   108,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   286,     0,     0,   111,     0,
       0,     0,     0,     0,   112,     0,   113,   165,   166,     0,
       0,     0,     0,   167,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   188,     0,   117,     0,     0,     0,     0,
       0,   288,     0,    67,   118,   168,   289,    69,   119,   170,
     290,     0,     0,   120,   121,   122,   123,    88,    39,    89,
       0,     0,     0,     0,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,   106,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   108,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,   113,   576,     0,     0,     0,
       0,     0,     0,     0,     0,   597,     0,     0,   115,     0,
       0,     0,   188,     0,   117,     0,     0,     0,   598,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,    88,
      39,    89,   120,   121,   122,   123,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,   106,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   108,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,   237,     0,     0,     0,   112,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,   238,     0,     0,     0,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,    89,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,   404,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,   405,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,     0,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,    88,    39,    89,   120,   121,   122,   123,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   108,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,     0,     0,   112,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
     459,     0,   460,     0,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,    89,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,   404,     0,     0,     0,    42,   614,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,     0,    58,
       0,     0,     0,   106,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   108,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,   113,   406,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     188,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,    88,    39,    89,
     120,   121,   122,   123,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,   106,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   108,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,   113,   576,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,   188,     0,   117,     0,     0,     0,   598,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,    88,
      39,    89,   120,   121,   122,   123,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,   404,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,   106,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   108,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,   112,     0,   113,   406,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,    89,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,   106,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   108,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,   113,
     576,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,     0,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,    88,    39,    89,   120,   121,   122,   123,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   108,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,     0,     0,   112,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,    89,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,     0,   680,
       0,     0,     0,   106,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   108,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
     188,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,    88,    39,    89,
     120,   121,   122,   123,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,   106,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,   188,     0,   117,     0,     0,     0,     0,     0,
       0,     0,    67,   118,     0,     0,    69,   119,     0,    88,
      39,    89,   120,   121,   122,   123,    93,     0,    40,    94,
       0,     0,     0,     0,     0,     0,    99,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   103,    51,    52,    53,    54,    55,    56,
      57,   104,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,   463,     0,   112,     0,     0,   279,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,   188,     0,   117,     0,     0,     0,
       0,     0,     0,     0,    67,   118,     0,     0,    69,   119,
       0,    88,    39,   278,   120,   121,   122,   123,    93,     0,
      40,    94,     0,     0,     0,     0,     0,     0,    99,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   103,    51,    52,    53,    54,
      55,    56,    57,   104,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,   112,     0,     0,
     279,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   188,     0,   117,     0,
       0,     0,     0,     0,     0,     0,    67,   118,     0,     0,
      69,   119,     0,    88,    39,    89,   120,   121,   122,   123,
      93,     0,    40,    94,     0,     0,     0,     0,     0,     0,
      99,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   103,    51,    52,
      53,    54,    55,    56,    57,   104,     0,    58,     0,     0,
       0,   106,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   111,     0,     0,     0,     0,     0,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,   188,     0,
     117,     0,     0,     0,     0,     0,     0,     0,    67,   118,
       0,     0,    69,   119,     0,    88,    39,    89,   120,   121,
     122,   123,    93,     0,    40,    94,     0,     0,     0,     0,
       0,     0,    99,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   103,
      51,    52,    53,    54,    55,    56,    57,   104,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   337,     0,     0,     0,     0,   115,     0,     0,     0,
     188,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      67,   118,     0,     0,    69,   119,     0,    88,    39,    89,
     120,   121,   122,   123,    93,     0,    40,    94,     0,     0,
       0,     0,     0,     0,    99,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   103,    51,    52,    53,    54,    55,    56,    57,   104,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,    39,   188,     0,   117,     0,     0,     0,     0,    40,
       0,     0,    67,   118,     0,     0,    69,   119,     0,     0,
       0,    42,   120,   121,   122,   123,   375,     0,    45,    46,
      47,   194,   195,   196,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   197,   198,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   199,     0,     0,
       0,     0,     0,     0,   380,     0,   381,     0,   201,   202,
     203,     0,     0,     0,     0,     0,     0,   204,     0,     0,
       0,   205,     0,    39,     0,   206,   382,   207,     0,     0,
       0,    40,   307,   208,     0,   209,    68,   210,   211,     0,
      70,   212,   213,    42,     0,     0,     0,     0,   375,     0,
      45,    46,    47,   194,   195,   196,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   197,   198,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   199,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
     201,   202,   203,     0,     0,     0,     0,     0,     0,   204,
       0,     0,     0,   205,     0,    39,     0,   206,     0,   207,
     387,     0,     0,    40,   307,   208,     0,   209,    68,   210,
     211,     0,    70,   212,   213,    42,     0,     0,     0,     0,
     375,     0,    45,    46,    47,   194,   195,   196,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   197,
     198,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   199,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,   201,   202,   203,     0,     0,     0,     0,     0,
       0,   204,     0,     0,     0,   205,   376,    39,     0,   206,
       0,   207,     0,     0,     0,    40,     0,   208,     0,   209,
      68,   210,   211,     0,    70,   212,   213,    42,     0,     0,
       0,     0,   375,     0,    45,    46,    47,   194,   195,   196,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   197,   198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   199,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,   201,   202,   203,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,   205,     0,    39,
       0,   206,     0,   207,     0,     0,     0,    40,     0,   208,
       0,   209,    68,   210,   211,     0,    70,   212,   213,    42,
       0,     0,     0,     0,     0,     0,    45,    46,    47,   194,
     195,   196,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,     0,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   197,   198,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   199,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,   201,   202,   203,     0,
       0,     0,     0,     0,     0,   204,     0,     0,     0,   205,
       0,    39,     0,   206,   759,   207,     0,     0,     0,    40,
       0,   208,     0,   209,    68,   210,   211,     0,    70,   212,
     213,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   194,   195,   196,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   197,   198,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   199,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,   201,   202,
     203,     0,     0,     0,     0,     0,     0,   204,     0,     0,
       0,   205,   470,    39,     0,   206,     0,   207,     0,     0,
       0,    40,     0,   208,     0,   209,    68,   210,   211,     0,
      70,   212,   213,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,    39,
       0,    60,    61,    62,    63,    64,    65,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,     0,     0,     0,     0,    45,    46,    47,   194,
     195,   196,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,     0,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   197,   198,     0,     0,    67,   118,     0,
       0,    69,   119,     0,     0,   286,     0,     0,     0,     0,
       0,     0,     0,     0,   200,  -150,     0,   202,   203,     0,
       0,     0,    39,     0,     0,   204,     0,     0,     0,   205,
      40,     0,     0,   206,     0,   207,     0,     0,     0,     0,
       0,   448,    42,   209,    68,     0,   289,     0,    70,    45,
      46,    47,   194,   195,   196,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   197,   198,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   286,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
     202,   203,     0,     0,     0,    39,     0,     0,   204,     0,
       0,     0,   205,    40,     0,     0,   206,     0,   207,     0,
       0,     0,     0,     0,   448,    42,   209,    68,     0,   289,
       0,    70,    45,    46,    47,   194,   195,   196,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   197,
     198,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   202,   203,     0,     0,     0,     0,     0,
       0,   204,     0,     0,     0,   205,    39,     0,     0,   206,
       0,   207,     0,     0,    40,     0,     0,     0,     0,   209,
      68,     0,     0,    41,    70,     0,    42,     0,    43,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,    59,    39,    60,    61,    62,    63,    64,    65,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    42,     0,    43,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,     0,    51,    52,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
      66,    39,     0,     0,     0,     0,     0,     0,     0,    40,
      67,    68,     0,     0,    69,    70,     0,     0,   353,     0,
       0,    42,     0,     0,     0,     0,     0,     0,    45,    46,
      47,   194,   195,   196,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,    66,    39,    60,
      61,    62,    63,    64,    65,     0,    40,    67,    68,     0,
       0,    69,    70,     0,     0,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,    39,     0,     0,     0,     0,
       0,     0,     0,    40,     0,   209,    68,     0,     0,     0,
      70,     0,     0,   509,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,    39,   274,    60,    61,    62,    63,    64,    65,    40,
       0,     0,    67,     0,     0,     0,    69,     0,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   164,   165,
     166,    39,     0,     0,     0,   167,     0,     0,     0,    40,
       0,     0,     0,     0,     0,   254,     0,     0,     0,     0,
       0,    42,     0,   255,     0,    67,    44,   168,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,    39,     0,    60,
      61,    62,    63,    64,    65,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,    39,    60,    61,    62,    63,    64,
      65,   158,    40,     0,     0,   262,     0,   160,     0,     0,
       0,     0,     0,     0,    42,    67,   118,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,    39,     0,
       0,     0,     0,     0,     0,     0,    40,     0,     0,     0,
       0,    67,   118,     0,     0,     0,     0,     0,    42,     0,
       0,  -356,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
     443,    58,     0,     0,    39,     0,    60,    61,    62,    63,
      64,    65,    40,     0,     0,   444,     0,     0,    67,     0,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
      39,     0,    60,    61,    62,    63,    64,    65,    40,     0,
       0,     0,   254,     0,     0,     0,     0,     0,     0,     0,
      42,     0,    67,     0,     0,     0,     0,    45,    46,    47,
     194,   195,   196,     0,    39,     0,    53,    54,    55,    56,
      57,     0,    40,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,    42,     0,     0,     0,     0,     0,
       0,    45,    46,    47,   194,   195,   196,     0,    67,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    39,    60,    61,    62,    63,    64,    65,     0,    40,
       0,     0,     0,     0,   643,     0,     0,     0,     0,     0,
       0,    42,     0,     0,   644,     0,     0,     0,    45,    46,
      47,   194,   195,   196,   209,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,   754,    60,
      61,    62,    63,    64,    65,     0,     0,     0,   644,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   209
  };

  const short
  parser::yycheck_[] =
  {
      29,   101,    29,   267,    90,    41,    66,    90,   262,   117,
      34,   249,   178,   267,   240,   101,    34,   155,   101,   237,
     327,   215,   110,   115,   116,   117,   238,    89,   102,   427,
     105,   136,    94,    90,   103,   110,   113,   447,   190,   115,
     136,   117,   102,   104,   533,   106,   460,   240,   629,   105,
     111,   112,   160,   114,   110,     1,   116,   446,   365,    22,
       1,   115,   116,   117,   366,     1,    80,    66,   206,   323,
     468,    78,     1,    12,     5,    27,    78,   138,    27,    21,
     109,   116,    80,   207,   102,   190,   288,   105,    78,    52,
      53,    19,   288,   118,   190,   109,   188,    81,   206,   207,
      89,   115,     0,   394,   129,    78,   727,   121,   115,   233,
     205,   206,   207,   404,   100,   108,   102,   116,   116,   205,
     206,   207,   208,   121,   159,   109,   328,   129,   188,   215,
     204,   193,   328,    85,   188,   221,    85,   136,   233,   129,
     292,   130,   115,   337,    85,   108,    81,   233,   177,   178,
     177,   178,   773,   188,   775,   644,   119,   103,   100,    79,
     123,   247,   103,    81,   745,    85,   359,   103,   114,   108,
     114,   473,   234,   235,   103,   237,   104,   104,   114,   742,
     115,   208,    85,   105,   598,   101,   204,   292,   101,   188,
     416,   190,   581,   115,   254,   115,   292,   115,   101,   101,
     104,   114,   129,   766,   108,   297,   110,   104,   606,    66,
      67,   108,   114,   110,   274,   119,   278,    89,   279,   412,
      92,   282,   119,   284,   248,   249,   123,   637,   305,    80,
     248,   249,   327,   319,   101,   724,    14,   255,   109,   432,
     326,   327,   271,   326,   115,   129,   264,   114,   466,   180,
     181,   337,    78,   109,    91,    92,    93,   475,   109,   115,
      78,    98,   108,   323,   115,   109,   340,   460,   111,   326,
     121,   115,   115,   119,   125,   274,   109,   123,   676,   365,
     341,    27,   115,   120,    80,    81,    82,   124,   108,    85,
     119,   320,   321,   292,   123,    91,    92,    93,   398,   119,
     114,    82,    98,   123,   399,    86,    82,   331,    87,    85,
     339,    90,   398,   399,   712,   398,    61,   624,   625,    80,
     116,    85,   340,    87,   120,   121,   431,   479,   124,   125,
      91,    92,    93,   115,   111,   431,   405,    98,   115,   434,
     111,   108,   560,   111,   115,   406,   111,   115,   434,   100,
     115,   102,   447,   519,   111,   116,   444,   109,   115,   120,
     121,   447,   111,   124,   125,    82,   115,    78,    85,   444,
      34,   433,   626,   448,   479,   114,    80,   439,   108,   441,
     442,    81,    82,   479,   476,    89,   478,    91,   444,   481,
     602,   483,    73,    74,    75,    76,   104,   459,   460,   130,
     476,   463,   478,   465,   466,   481,   100,   483,   102,   100,
     603,   102,   474,   475,   476,   722,   120,   121,    72,   494,
     124,   125,   476,   129,   478,   443,   100,   481,   446,   483,
     115,   449,   431,   100,   529,   102,   756,    88,   758,   115,
     535,   129,   537,   529,   539,   105,   541,   533,   543,   535,
     676,   537,   109,   539,   592,   541,   530,   543,   532,   109,
      91,    92,    93,   727,   682,    48,    49,    98,   124,   125,
      80,   709,   710,   727,   441,   442,   494,   109,    24,   109,
     479,    91,    92,    93,   592,   593,   109,   523,    98,   120,
     519,   109,   519,   555,    23,   557,   138,   559,   560,   693,
     562,   104,   138,    27,    90,   108,   116,   110,    85,   773,
     120,   121,   530,   114,   532,   576,   119,    78,    78,   773,
     123,    78,    78,    15,    82,    87,   130,    81,   130,    78,
     115,    81,    87,   727,   629,   105,   598,   115,   624,   625,
     109,   565,   637,   629,   636,   109,   109,   565,   643,   129,
     109,   637,   614,   129,   101,   748,   109,   643,   644,    80,
     636,   109,   756,   581,   758,    78,   626,    78,    89,   101,
      91,    92,    93,   115,    80,    81,   115,    98,   105,   773,
      90,   775,   636,   109,   115,    91,    92,    93,    85,   111,
     109,   116,    98,   116,    87,   115,    86,    86,   115,   120,
     121,    25,   109,   124,   125,   109,   635,   693,   116,   105,
     116,   673,   674,   129,   120,   121,   119,   679,   124,   125,
     682,   683,    78,    78,   129,    78,    78,   115,   130,   116,
     116,   116,    81,    81,   734,   115,   722,   116,   724,   101,
     115,   727,    82,    27,   101,   129,   129,   116,   734,   114,
     745,   734,    82,   715,   716,   717,   115,   101,   105,   745,
     105,    78,    80,    81,   764,    79,   702,   694,    29,     9,
     756,     4,   758,    91,    92,    93,   776,   115,   764,    48,
      98,   764,   109,   109,   746,   709,   710,   773,    85,   775,
     776,   709,   710,   776,    81,    11,    90,    54,   116,   108,
      49,   109,   120,   121,   115,   109,   124,   125,   735,   109,
     772,   109,   519,   742,   177,   742,    15,     3,     4,     5,
       6,     7,     8,   340,    10,   127,    12,    13,   326,   625,
      16,    17,    18,    19,    20,   217,    22,   766,    24,   766,
      26,   434,    28,    29,   221,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     776,    47,   756,   725,    50,    51,    52,    53,    54,    55,
      56,    57,    58,   690,   319,    80,   532,    63,    64,   541,
     735,   696,    87,    34,    89,   565,    91,    34,   329,   468,
     658,    77,   117,   682,   466,   673,   364,    83,   235,   557,
     635,   282,    59,    89,   109,    91,    92,   105,   457,    43,
     115,   626,   102,   208,    -1,   120,   121,    -1,   104,   124,
     125,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,    -1,
      -1,    -1,   128,   129,   130,   131,    -1,    -1,     3,     4,
       5,    -1,    -1,   139,   140,    10,    -1,    12,    13,    -1,
      -1,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    -1,    -1,    -1,    -1,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,
      -1,    -1,    -1,   128,   129,   130,   131,     1,    -1,     3,
       4,     5,    -1,    -1,   139,   140,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
     114,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    93,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,   109,   110,    -1,
      -1,    -1,    -1,   115,   116,    -1,   118,   119,   120,   121,
     122,   123,   124,   125,    -1,    -1,   128,   129,   130,   131,
       3,     4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,
      13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,
      93,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,   108,    -1,   110,   111,    -1,
      -1,    -1,   115,   116,    -1,   118,   119,   120,   121,   122,
     123,   124,   125,    -1,    -1,   128,   129,   130,   131,     3,
       4,     5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,    93,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,   116,    -1,   118,   119,   120,   121,   122,   123,
     124,   125,    -1,    -1,   128,   129,   130,   131,     3,     4,
       5,    -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    91,    92,    93,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,
      -1,   116,    -1,   118,   119,   120,   121,   122,   123,   124,
     125,    -1,    -1,   128,   129,   130,   131,     3,     4,     5,
      -1,    -1,    -1,    -1,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,   110,    -1,    -1,    -1,   114,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,
       4,     5,   128,   129,   130,   131,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    85,    -1,    -1,    -1,    89,    -1,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,     3,     4,     5,   128,   129,   130,   131,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,   102,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    25,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,     3,     4,     5,
     128,   129,   130,   131,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,   110,    -1,    -1,    -1,   114,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,
       4,     5,   128,   129,   130,   131,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,     3,     4,     5,   128,   129,   130,   131,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,     3,     4,     5,
     128,   129,   130,   131,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,     3,
       4,     5,   128,   129,   130,   131,    10,    -1,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    87,    -1,    89,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
      -1,     3,     4,     5,   128,   129,   130,   131,    10,    -1,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,    -1,     3,     4,     5,   128,   129,   130,   131,
      10,    -1,    12,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,   108,    -1,
     110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,    -1,     3,     4,     5,   128,   129,
     130,   131,    10,    -1,    12,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,
     108,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     118,   119,    -1,    -1,   122,   123,    -1,     3,     4,     5,
     128,   129,   130,   131,    10,    -1,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
      -1,     4,   108,    -1,   110,    -1,    -1,    -1,    -1,    12,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,    -1,    -1,
      -1,    24,   128,   129,   130,   131,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    89,    -1,    91,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
      -1,   104,    -1,     4,    -1,   108,   109,   110,    -1,    -1,
      -1,    12,   115,   116,    -1,   118,   119,   120,   121,    -1,
     123,   124,   125,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    -1,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      91,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,    -1,   104,    -1,     4,    -1,   108,    -1,   110,
     111,    -1,    -1,    12,   115,   116,    -1,   118,   119,   120,
     121,    -1,   123,   124,   125,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    91,    92,    93,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,   105,     4,    -1,   108,
      -1,   110,    -1,    -1,    -1,    12,    -1,   116,    -1,   118,
     119,   120,   121,    -1,   123,   124,   125,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    91,    92,    93,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,    -1,   104,    -1,     4,
      -1,   108,    -1,   110,    -1,    -1,    -1,    12,    -1,   116,
      -1,   118,   119,   120,   121,    -1,   123,   124,   125,    24,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    91,    92,    93,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,
      -1,     4,    -1,   108,   109,   110,    -1,    -1,    -1,    12,
      -1,   116,    -1,   118,   119,   120,   121,    -1,   123,   124,
     125,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    91,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
      -1,   104,     3,     4,    -1,   108,    -1,   110,    -1,    -1,
      -1,    12,    -1,   116,    -1,   118,   119,   120,   121,    -1,
     123,   124,   125,    24,    -1,    -1,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,     4,
      -1,    52,    53,    54,    55,    56,    57,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    33,    34,
      35,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    69,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    90,    -1,    92,    93,    -1,
      -1,    -1,     4,    -1,    -1,   100,    -1,    -1,    -1,   104,
      12,    -1,    -1,   108,    -1,   110,    -1,    -1,    -1,    -1,
      -1,   116,    24,   118,   119,    -1,   121,    -1,   123,    31,
      32,    33,    34,    35,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    -1,    -1,     4,    -1,    -1,   100,    -1,
      -1,    -1,   104,    12,    -1,    -1,   108,    -1,   110,    -1,
      -1,    -1,    -1,    -1,   116,    24,   118,   119,    -1,   121,
      -1,   123,    31,    32,    33,    34,    35,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,     4,    -1,    -1,   108,
      -1,   110,    -1,    -1,    12,    -1,    -1,    -1,    -1,   118,
     119,    -1,    -1,    21,   123,    -1,    24,    -1,    26,    -1,
      -1,    29,    -1,    31,    32,    33,    34,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    44,    -1,    -1,    47,
      -1,    -1,    50,     4,    52,    53,    54,    55,    56,    57,
      -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    26,    -1,    -1,    29,    -1,
      31,    32,    33,    34,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
     108,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
     118,   119,    -1,    -1,   122,   123,    -1,    -1,    79,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,   108,     4,    52,
      53,    54,    55,    56,    57,    -1,    12,   118,   119,    -1,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
      -1,    47,    -1,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,   118,   119,    -1,    -1,    -1,
     123,    -1,    -1,    79,    -1,    24,    -1,    -1,    -1,    -1,
      29,    -1,    31,    32,    33,    34,    35,    36,    -1,    38,
      39,    40,    41,    42,    43,    44,    -1,    -1,    47,    -1,
      -1,     4,   108,    52,    53,    54,    55,    56,    57,    12,
      -1,    -1,   118,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,    29,    -1,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,
      -1,    -1,    -1,   122,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      93,     4,    -1,    -1,    -1,    98,    -1,    -1,    -1,    12,
      -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,    -1,
      -1,    24,    -1,   116,    -1,   118,    29,   120,    31,    32,
      33,    34,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,     4,    -1,    52,
      53,    54,    55,    56,    57,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    29,    -1,    31,    32,    33,    34,    35,    36,
      -1,    38,    39,    40,    41,    42,    43,    44,    -1,    -1,
      47,    -1,    -1,    -1,     4,    52,    53,    54,    55,    56,
      57,   104,    12,    -1,    -1,   108,    -1,   110,    -1,    -1,
      -1,    -1,    -1,    -1,    24,   118,   119,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    81,    -1,    29,    -1,    31,    32,    33,    34,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    44,    -1,
     100,    47,    -1,    -1,     4,    -1,    52,    53,    54,    55,
      56,    57,    12,    -1,    -1,   115,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    29,
      -1,    31,    32,    33,    34,    35,    36,    -1,    38,    39,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
       4,    -1,    52,    53,    54,    55,    56,    57,    12,    -1,
      -1,    -1,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      24,    -1,   118,    -1,    -1,    -1,    -1,    31,    32,    33,
      34,    35,    36,    -1,     4,    -1,    40,    41,    42,    43,
      44,    -1,    12,    47,    -1,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    24,    -1,    -1,    -1,    -1,    -1,
      -1,    31,    32,    33,    34,    35,    36,    -1,   118,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    47,    -1,    -1,
      -1,     4,    52,    53,    54,    55,    56,    57,    -1,    12,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,   108,    -1,    -1,    -1,    31,    32,
      33,    34,    35,    36,   118,    -1,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    47,    -1,    -1,    -1,    98,    52,
      53,    54,    55,    56,    57,    -1,    -1,    -1,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   118
  };

  const short
  parser::yystos_[] =
  {
       0,    21,   100,   142,   143,   144,   147,   119,   123,   332,
     148,   161,     0,   148,    66,    67,   145,   101,   114,   149,
     162,   163,     1,   103,   331,   104,   129,   198,   198,   108,
     150,    14,   164,   175,   176,   129,   199,    78,    78,     4,
      12,    21,    24,    26,    29,    31,    32,    33,    34,    35,
      36,    38,    39,    40,    41,    42,    43,    44,    47,    50,
      52,    53,    54,    55,    56,    57,   108,   118,   119,   122,
     123,   151,   152,   153,   158,   159,   298,   301,   302,   316,
     317,   318,   324,    27,    61,   165,   114,   160,     3,     5,
       6,     7,     8,    10,    13,    16,    17,    18,    19,    20,
      22,    26,    28,    37,    45,    50,    51,    58,    63,    64,
      77,    83,    89,    91,    92,   104,   108,   110,   119,   123,
     128,   129,   130,   131,   139,   140,   173,   177,   178,   179,
     180,   184,   188,   193,   241,   246,   251,   252,   256,   257,
     258,   259,   287,   288,   291,   292,   315,   316,   318,   326,
     327,   330,   105,   115,   332,   108,   297,   301,   104,   108,
     110,   287,    80,    89,    91,    92,    93,    98,   120,   121,
     124,   125,   321,   322,   323,   325,   109,   115,   108,   154,
     100,   102,   146,    78,    34,   166,   114,    64,   108,   249,
     250,   252,   253,   255,    34,    35,    36,    68,    69,    80,
      89,    91,    92,    93,   100,   104,   108,   110,   116,   118,
     120,   121,   124,   125,   186,   206,   207,   208,   211,   213,
     215,   217,   218,   220,   296,   297,   299,   303,   304,   312,
     313,   314,   324,   108,   100,   102,   280,    85,   100,   249,
     273,   274,   275,    73,    74,    75,    76,   181,   100,   102,
     195,   196,   213,   215,   108,   116,   306,   315,   318,   322,
     280,   257,   108,   189,   289,   290,   292,   318,   327,   257,
     104,   247,   248,   129,   108,   316,   289,   290,     5,    92,
     257,   278,   279,   257,   256,   257,    80,   105,   116,   121,
     125,   249,   250,   260,   262,   263,   294,   308,   309,   311,
     320,   321,   323,   328,   329,    91,   109,   115,   260,   261,
     321,   322,   323,   328,   333,   111,   260,   261,   333,    19,
     247,   247,   130,   172,   160,    72,   187,    81,   116,   200,
     294,   307,   309,   310,   319,   321,   322,    99,   257,   100,
     115,    88,   129,    89,   303,   328,   109,   109,   109,   109,
     109,   109,   153,    79,   155,   156,   157,   158,   148,   148,
      24,   168,   118,   129,    23,    81,   307,   249,   138,   138,
     204,   234,   235,   236,   315,    29,   105,   209,   211,   213,
      87,    89,   109,   209,   224,   303,   333,   111,   209,   224,
     225,   333,   301,   314,    27,   197,   220,   207,    90,    87,
     218,   209,   223,   224,    20,    46,    92,   249,   277,   281,
     282,   283,   281,   266,   267,   283,   273,   114,   254,   275,
     331,    78,    78,    78,    78,   202,   209,   221,   194,   241,
     242,   251,   194,    15,    82,   322,   318,   130,   130,    87,
     329,    82,    86,   100,   115,   190,   318,    81,   116,   293,
     329,    89,   130,   316,    78,    78,   129,    81,   201,   100,
     102,   268,   257,    87,   278,    82,    85,   243,   244,   245,
       3,   317,   326,   307,    79,    85,   115,   105,   115,   250,
     109,   115,   109,   115,   109,   109,   109,   115,   111,   111,
     111,   221,   316,   316,   116,   174,   293,   305,   306,   129,
     186,   202,   203,   209,   210,   317,   243,   252,   220,    79,
     284,   285,   286,   316,   204,   257,   109,   109,   109,   115,
     101,   331,   129,   167,    78,    78,   268,   202,   253,    81,
     115,   101,   115,   226,   105,    90,   109,    81,   109,   115,
     109,    85,   111,   115,   111,   111,   116,   116,   196,   213,
     209,   109,   196,   280,   257,    86,   101,   114,   331,    87,
     115,   101,    25,   197,   101,   114,   331,   249,   209,   210,
     109,   116,   129,   129,   249,   109,    92,   249,   276,   276,
     191,   318,   290,   190,   203,   327,   318,   130,   105,    78,
      78,   104,   108,   110,   295,   296,    78,   101,   114,   269,
     270,   271,   276,   269,   331,   249,   249,   266,   197,   245,
     116,   116,   116,   249,    25,   264,   265,   283,   249,   260,
     260,   260,   260,    78,    81,    81,   115,    78,   129,    81,
      82,   185,   229,   116,   101,   115,    82,    81,   157,   332,
     209,   315,   236,    98,   108,   227,   312,   209,   209,   228,
     209,   209,   225,   209,   249,   282,   249,   283,   249,   242,
     129,   129,   257,    27,   192,   101,   190,   116,   105,   109,
     111,   269,   101,   114,    87,   272,   273,   331,   197,    82,
      47,   249,   115,    79,   202,   205,   205,   305,    78,   228,
      29,   230,   231,   232,     9,   237,   238,   239,   285,   260,
     203,   115,     4,   169,   209,   312,   109,   109,   254,   100,
     102,   271,   249,   197,   249,    48,    49,    48,   265,   283,
     249,    78,   115,    78,   226,    85,   212,   216,   219,   220,
     233,    22,    52,    53,   108,   182,   240,   300,   301,   239,
     332,    12,   108,   170,   171,    81,    11,   194,   194,   197,
     249,   249,   249,   202,    98,   231,    90,   219,   293,   109,
     213,   214,   222,   240,    54,   183,   108,   151,   228,   249,
     101,   331,    49,   216,   233,   216,   115,   109,   213,   151,
     109,   109,   249,   222,   109
  };

  const short
  parser::yyr1_[] =
  {
       0,   141,   142,   143,   143,   144,   145,   145,   145,   146,
     146,   147,   147,   148,   149,   149,   149,   150,   150,   151,
     152,   152,   153,   153,   153,   154,   154,   155,   155,   156,
     156,   157,   157,   158,   158,   159,   159,   160,   160,   161,
     161,   162,   163,   163,   164,   165,   165,   166,   166,   167,
     167,   168,   168,   169,   169,   170,   170,   171,   171,   172,
     172,   173,   173,   173,   174,   174,   175,   176,   176,   177,
     177,   177,   177,   177,   177,   177,   177,   177,   177,   178,
     179,   179,   179,   180,   181,   181,   181,   181,   181,   182,
     182,   182,   183,   184,   184,   185,   185,   186,   186,   187,
     187,   187,   188,   188,   188,   189,   189,   189,   190,   190,
     191,   192,   192,   193,   194,   194,   194,   194,   195,   195,
     196,   197,   197,   198,   198,   199,   199,   199,   200,   200,
     201,   201,   202,   203,   204,   204,   205,   205,   206,   206,
     206,   207,   207,   208,   208,   209,   209,   209,   210,   211,
     212,   213,   213,   214,   215,   216,   216,   217,   217,   218,
     218,   218,   219,   220,   220,   220,   220,   220,   220,   220,
     220,   220,   220,   220,   220,   220,   221,   222,   222,   223,
     223,   224,   224,   225,   225,   226,   226,   227,   227,   228,
     229,   230,   230,   231,   231,   232,   232,   233,   233,   234,
     234,   235,   235,   236,   237,   237,   238,   238,   239,   239,
     239,   240,   240,   240,   241,   241,   241,   241,   242,   243,
     243,   244,   244,   245,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   247,   247,   248,   248,   249,
     249,   250,   250,   251,   251,   252,   252,   252,   253,   253,
     254,   254,   255,   255,   256,   256,   256,   256,   257,   257,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     258,   258,   259,   259,   259,   259,   259,   259,   259,   259,
     259,   260,   260,   260,   261,   261,   262,   262,   262,   262,
     262,   262,   262,   263,   263,   264,   264,   264,   264,   265,
     265,   265,   265,   266,   267,   267,   268,   268,   268,   268,
     269,   269,   270,   270,   270,   271,   272,   272,   273,   273,
     274,   274,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   281,   281,   281,   281,   282,   282,   283,
     283,   283,   284,   284,   285,   285,   285,   286,   286,   287,
     287,   288,   288,   289,   289,   289,   290,   290,   291,   291,
     291,   291,   292,   292,   293,   293,   294,   294,   295,   295,
     295,   296,   296,   296,   296,   296,   297,   297,   297,   298,
     298,   298,   298,   298,   299,   299,   300,   301,   301,   302,
     303,   303,   303,   304,   304,   304,   304,   305,   305,   306,
     306,   307,   307,   307,   308,   308,   308,   309,   310,   310,
     311,   311,   312,   313,   314,   314,   314,   314,   314,   315,
     315,   316,   316,   316,   317,   317,   318,   318,   318,   318,
     318,   318,   318,   318,   319,   319,   320,   320,   321,   322,
     322,   323,   323,   324,   324,   324,   324,   324,   324,   324,
     324,   324,   324,   324,   324,   324,   324,   324,   324,   324,
     324,   325,   325,   325,   326,   326,   327,   328,   328,   329,
     329,   330,   330,   330,   330,   331,   331,   332,   332,   333,
     333
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     2,     0,     3,     0,     1,     4,
       1,     1,     1,     1,     2,     1,     1,     2,     1,     2,
       0,     2,     3,     0,     8,     2,     0,     1,     0,     1,
       0,     1,     0,     2,     0,     1,     0,     3,     4,     0,
       1,     1,     1,     1,     3,     1,     2,     3,     0,     1,
       1,     1,     4,     1,     1,     5,     4,     5,     4,     3,
       4,     5,     4,     4,     2,     2,     2,     2,     0,     1,
       1,     1,     2,     1,     1,     0,     2,     3,     1,     4,
       3,     0,     4,     4,     5,     2,     3,     4,     0,     2,
       2,     4,     4,     4,     3,     2,     1,     0,     3,     3,
       1,     2,     0,     1,     3,     3,     1,     0,     0,     2,
       0,     2,     1,     1,     3,     1,     1,     3,     1,     1,
       2,     1,     1,     2,     2,     4,     3,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     2,     3,     2,     5,
       2,     3,     3,     3,     3,     5,     1,     1,     3,     1,
       0,     1,     3,     3,     3,     2,     0,     1,     5,     1,
       2,     3,     1,     4,     2,     3,     0,     1,     3,     0,
       1,     3,     1,     3,     0,     1,     2,     1,     2,     3,
       3,     1,     2,     3,     1,     3,     3,     1,     1,     3,
       2,     2,     1,     4,     3,     5,     3,     1,     4,     4,
       3,     4,     6,     6,     4,     0,     1,     3,     4,     3,
       1,     1,     3,     1,     3,     2,     3,     1,     1,     2,
       1,     0,     3,     3,     2,     3,     2,     1,     3,     2,
       4,     4,     3,     8,     2,     4,     2,     2,     4,     1,
       4,     1,     1,     1,     1,     3,     3,     3,     3,     3,
       1,     1,     2,     2,     3,     3,     1,     1,     2,     4,
       3,     5,     3,     3,     3,     3,     3,     1,     1,     2,
       4,     4,     6,     1,     3,     1,     3,     3,     2,     2,
       1,     2,     3,     2,     1,     2,     3,     2,     2,     1,
       3,     2,     4,     1,     2,     1,     2,     1,     2,     2,
       1,     3,     3,     3,     2,     1,     0,     1,     2,     3,
       1,     2,     1,     0,     3,     1,     1,     3,     1,     1,
       1,     1,     3,     1,     3,     1,     1,     3,     2,     3,
       2,     3,     1,     2,     1,     3,     1,     3,     1,     2,
       2,     1,     3,     3,     3,     2,     1,     3,     3,     1,
       3,     3,     3,     3,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       3,     1,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1
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
  "\"proc\"", "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"pattern\"",
  "\"static\"", "\"stock\"", "\"anyclass\"", "\"via\"", "\"unit\"",
  "\"signature\"", "\"dependency\"", "\"{-# INLINE\"",
  "\"{-# SPECIALIZE\"", "\"{-# SPECIALIZE_INLINE\"", "\"{-# SOURCE\"",
  "\"{-# RULES\"", "\"{-# CORE\"", "\"{-# SCC\"", "\"{-# GENERATED\"",
  "\"{-# DEPRECATED\"", "\"{-# WARNING\"", "\"{-# UNPACK\"",
  "\"{-# NOUNPACK\"", "\"{-# ANN\"", "\"{-# MINIMAL\"", "\"{-# CTYPE\"",
  "\"{-# OVERLAPPING\"", "\"{-# OVERLAPPABLE\"", "\"{-# OVERLAPS\"",
  "\"{-# INCOHERENT\"", "\"{-# COMPLETE\"", "\"#-}\"", "\"..\"", "\":\"",
  "\"::\"", "\"=\"", "\"\\\\\"", "\"lcase\"", "\"|\"", "\"<-\"", "\"->\"",
  "\"@\"", "\"~\"", "\"=>\"", "\"-\"", "\"!\"", "\"*\"", "\"-<\"",
  "\">-\"", "\"-<<\"", "\">>-\"", "\".\"", "\"TYPEAPP\"", "\"{\"", "\"}\"",
  "\"vocurly\"", "\"vccurly\"", "\"[\"", "\"]\"", "\"[:\"", "\":]\"",
  "\"(\"", "\")\"", "\"(#\"", "\"#)\"", "\"(|\"", "\"|)\"", "\";\"",
  "\",\"", "\"`\"", "\"'\"", "\"VARID\"", "\"CONID\"", "\"VARSYM\"",
  "\"CONSYM\"", "\"QVARID\"", "\"QCONID\"", "\"QVARSYM\"", "\"QCONSYM\"",
  "\"IPDUPVARID\"", "\"LABELVARID\"", "\"CHAR\"", "\"STRING\"",
  "\"INTEGER\"", "\"RATIONAL\"", "\"PRIMCHAR\"", "\"PRIMSTRING\"",
  "\"PRIMINTEGER\"", "\"PRIMWORD\"", "\"PRIMFLOAT\"", "\"PRIMDOUBLE\"",
  "\"#-\"", "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"", "$accept",
  "unit", "module", "missing_module_keyword", "maybemodwarning", "body",
  "body2", "top", "top1", "maybeexports", "exportlist", "exportlist1",
  "export", "export_subspec", "qcnames", "qcnames1",
  "qcname_ext_w_wildcard", "qcname_ext", "qcname", "semis1", "semis",
  "importdecls", "importdecls_semi", "importdecl", "maybe_src",
  "maybe_safe", "maybe_pkg", "optqualified", "maybeas", "maybeimpspec",
  "impspec", "prec", "infix", "ops", "topdecls", "topdecls_semi",
  "topdecl", "cl_decl", "ty_decl", "inst_decl", "overlap_pragma",
  "deriv_strategy_no_via", "deriv_strategy_via", "data_or_newtype",
  "opt_kind_sig", "tycl_hdr", "capi_ctype", "pattern_synonym_decl",
  "pattern_synonym_lhs", "vars0", "cvars1", "where_decls",
  "pattern_synonym_sig", "decls", "decllist", "binds", "wherebinds",
  "strings", "stringlist", "opt_sig", "opt_tyconsig", "sigtype",
  "sigtypedoc", "sig_vars", "sigtypes1", "strict_mark", "strictness",
  "unpackedness", "ctype", "ctypedoc", "context", "context_no_ops", "type",
  "typedoc", "btype", "btype_no_ops", "tyapps", "tyapp", "atype_docs",
  "atype", "inst_type", "deriv_types", "comma_types0", "comma_types1",
  "bar_types2", "tv_bndrs", "tv_bndr", "kind", "constrs", "constrs1",
  "constr", "forall", "constr_stuff", "fielddecls", "fielddecls1",
  "fielddecl", "maybe_derivings", "derivings", "deriving",
  "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs", "gdrh",
  "sigdecl", "activation", "explicit_activation", "exp", "infixexp",
  "infixexp_top", "exp10_top", "exp10", "optSemi", "scc_annot", "fexp",
  "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps", "squals",
  "transformqual", "guardquals", "guardquals1", "altslist", "alts",
  "alts1", "alt", "alt_rhs", "gdpats", "ifgdpats", "gdpat", "pat",
  "bindpat", "apat", "apats1", "stmtlist", "stmts", "stmt", "qual",
  "fbinds", "fbinds1", "fbind", "qcon", "gen_qcon", "con", "con_list",
  "sysdcon_no_list", "sysdcon", "conop", "qconop", "gtycon", "ntgtycon",
  "oqtycon", "oqtycon_no_varcon", "qtyconop", "qtycondoc", "qtycon",
  "tycon", "qtyconsym", "tyconsym", "op", "varop", "qop", "qopm",
  "hole_op", "qvarop", "qvaropm", "tyvar", "tyvarop", "tyvarid", "var",
  "qvar", "qvarid", "varid", "qvarsym", "qvarsym_no_minus", "qvarsym1",
  "varsym", "varsym_no_minus", "special_id", "special_sym", "qconid",
  "conid", "qconsym", "consym", "literal", "close", "modid", "commas", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  parser::yyrline_[] =
  {
       0,   504,   504,   521,   522,   524,   528,   529,   530,   532,
     533,   535,   536,   539,   541,   542,   543,   551,   552,   554,
     556,   557,   559,   560,   561,   563,   564,   566,   567,   569,
     570,   572,   573,   575,   576,   578,   579,   583,   584,   586,
     587,   589,   591,   592,   594,   603,   604,   606,   607,   609,
     610,   612,   613,   615,   616,   618,   619,   621,   622,   627,
     628,   630,   631,   632,   634,   635,   639,   641,   642,   644,
     645,   646,   649,   656,   657,   658,   659,   660,   661,   663,
     665,   666,   667,   671,   676,   677,   678,   679,   680,   682,
     683,   684,   686,   726,   727,   729,   730,   739,   740,   742,
     743,   744,   761,   762,   763,   765,   766,   767,   769,   770,
     772,   774,   775,   778,   784,   785,   786,   787,   789,   790,
     792,   794,   795,   803,   804,   806,   807,   808,   816,   817,
     819,   820,   822,   824,   826,   827,   829,   830,   834,   835,
     836,   838,   839,   841,   842,   844,   845,   847,   849,   858,
     860,   862,   863,   865,   868,   870,   871,   873,   874,   876,
     877,   878,   884,   886,   887,   888,   889,   890,   891,   892,
     893,   894,   895,   896,   897,   898,   901,   903,   904,   906,
     907,   909,   910,   912,   913,   915,   916,   918,   919,   937,
     943,   945,   946,   948,   949,   951,   952,   954,   955,   957,
     958,   960,   961,   963,   965,   966,   968,   969,   971,   972,
     973,   975,   976,   977,   982,   984,   986,   987,   990,   994,
     995,   997,   998,  1002,  1004,  1005,  1006,  1007,  1008,  1009,
    1010,  1011,  1012,  1013,  1014,  1016,  1017,  1019,  1020,  1024,
    1025,  1027,  1028,  1030,  1031,  1033,  1034,  1035,  1037,  1038,
    1041,  1042,  1044,  1045,  1049,  1050,  1051,  1052,  1054,  1055,
    1056,  1057,  1058,  1059,  1060,  1061,  1062,  1063,  1064,  1065,
    1067,  1068,  1070,  1071,  1072,  1073,  1074,  1075,  1076,  1077,
    1078,  1083,  1084,  1085,  1090,  1091,  1109,  1110,  1111,  1112,
    1113,  1114,  1115,  1117,  1118,  1130,  1131,  1132,  1133,  1135,
    1136,  1137,  1138,  1141,  1143,  1144,  1147,  1148,  1149,  1150,
    1152,  1153,  1155,  1156,  1157,  1159,  1161,  1162,  1164,  1165,
    1167,  1168,  1170,  1172,  1173,  1175,  1176,  1178,  1179,  1181,
    1182,  1185,  1186,  1188,  1189,  1190,  1191,  1196,  1197,  1199,
    1200,  1201,  1206,  1207,  1209,  1210,  1211,  1213,  1214,  1246,
    1247,  1249,  1250,  1252,  1253,  1254,  1256,  1257,  1259,  1260,
    1261,  1262,  1264,  1265,  1267,  1268,  1270,  1271,  1274,  1275,
    1276,  1278,  1279,  1280,  1281,  1282,  1284,  1285,  1286,  1288,
    1289,  1290,  1291,  1292,  1295,  1296,  1298,  1300,  1301,  1305,
    1307,  1308,  1309,  1311,  1312,  1313,  1314,  1319,  1320,  1322,
    1323,  1325,  1326,  1327,  1329,  1330,  1331,  1333,  1335,  1336,
    1338,  1339,  1343,  1345,  1347,  1348,  1349,  1350,  1351,  1354,
    1355,  1357,  1358,  1359,  1361,  1362,  1364,  1365,  1366,  1367,
    1368,  1369,  1370,  1371,  1373,  1374,  1376,  1377,  1379,  1381,
    1382,  1384,  1385,  1387,  1388,  1389,  1390,  1391,  1392,  1393,
    1394,  1395,  1396,  1397,  1398,  1399,  1400,  1401,  1402,  1403,
    1404,  1406,  1407,  1408,  1412,  1413,  1415,  1417,  1418,  1420,
    1421,  1425,  1426,  1427,  1428,  1433,  1436,  1440,  1441,  1443,
    1444
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
#line 6009 "parser.cc"

#line 1453 "parser.y"


using std::optional;
using std::string;
using std::vector;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

expression_ref make_module(const string& name, const expression_ref& exports, const expression_ref& body)
{
    vector<expression_ref> e = {String(name)};
    if (exports)
	e.push_back(exports);
    e.push_back(body);
    return new expression(AST_node("Module"),e);
}

expression_ref make_body(const std::vector<expression_ref>& imports, const std::vector<expression_ref>& topdecls)
{
    expression_ref i = new expression(AST_node("impdecls"),imports);
    expression_ref t = new expression(AST_node("TopDecls"),topdecls);
    return new expression(AST_node("Body"),{i,t});
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

expression_ref make_sig_vars(const vector<std::string>& sig_vars)
{
    return new expression(AST_node("sig_vars"),make_String_vec(sig_vars));
}

expression_ref make_data_or_newtype(const string& d_or_n, const expression_ref& tycls_hdr, const vector<expression_ref>& constrs)
{
    expression_ref c = new expression(AST_node("constrs"),constrs);
    assert(d_or_n == "data" or d_or_n == "newtype");
    return new expression(AST_node("Decl:"+d_or_n),{tycls_hdr,c});
}

expression_ref make_context(const expression_ref& context, const expression_ref& type)
{
    return new expression(AST_node("context"),{context,type});
}

expression_ref make_tv_bndrs(const vector<expression_ref>& tv_bndrs)
{
    return new expression(AST_node("tv_bndrs"),tv_bndrs);
}

expression_ref type_apply(const expression_ref& e1, const expression_ref& e2)
{
    if (is_AST(e1, "TypeApply"))
	return e1 + e2;
    else
	return AST_node("TypeApply") + e1 + e2;
}


expression_ref make_tyapps(const std::vector<expression_ref>& tyapps)
{
    assert(not tyapps.empty());
    expression_ref E = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	E = type_apply(E,tyapps[i]);
    return E;
}

expression_ref make_id(const string& id)
{
    return AST_node("id",id);
}

expression_ref make_type_id(const string& id)
{
    return AST_node("type_id",id);
}

expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type)
{
    return new expression(AST_node("typed_exp"),{exp,type});
}

expression_ref make_rhs(const expression_ref& exp, const expression_ref& wherebinds)
{
    vector<expression_ref> e = {exp};
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return expression_ref{AST_node("rhs"), std::move(e)};
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

expression_ref make_as_pattern(const string& var, const expression_ref& body)
{
    auto x = AST_node("id",var);
    return new expression(AST_node("AsPattern"), {x,body});
}

expression_ref make_lazy_pattern(const expression_ref& pat)
{
    return new expression(AST_node("LazyPattern"), {pat});
}

expression_ref make_strict_pattern(const expression_ref& pat)
{
    return new expression(AST_node("StrictPattern"), {pat});
}

expression_ref make_lambda(const vector<expression_ref>& pats, const expression_ref& body)
{
    auto e = pats;
    e.push_back(body);
    return new expression(AST_node("Lambda"), e);
}

expression_ref make_let(const expression_ref& binds, const expression_ref& body)
{
    return new expression(AST_node("Let"), {binds, body});
}

expression_ref make_if(const expression_ref& cond, const expression_ref& alt_true, const expression_ref& alt_false)
{
    return new expression(AST_node("If"), {cond, alt_true, alt_false});
}

expression_ref make_case(const expression_ref& obj, const expression_ref& alts)
{
    return new expression(AST_node("Case"), {obj, alts});
}

expression_ref make_do(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Do"), stmts);
}

expression_ref make_mdo(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("MDo"), stmts);
}

expression_ref yy_make_tuple(const vector<expression_ref>& tup_exprs)
{
    expression_ref t = AST_node("id", tuple_head(tup_exprs.size()).name());
    for(auto& e: tup_exprs)
	t = {t,e};
    return t;
}


expression_ref make_list(const vector<expression_ref>& pquals)
{
    expression_ref L = AST_node("id","[]");
    expression_ref cons = AST_node("id",":");
    for(int i=pquals.size()-1;i>=0;i--)
	L = {cons,pquals[i],L};
    return L;
}

expression_ref make_alts(const vector<expression_ref>& alts)
{
    return expression_ref(AST_node("alts"), alts);
}

expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs)
{
    return AST_node("alt") + pat + alt_rhs;
}

expression_ref make_gdrhs(const vector<expression_ref>& guards, const expression_ref& wherebinds)
{
    vector<expression_ref> e = {expression_ref{AST_node("guards"),guards}};
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return expression_ref{AST_node("gdrhs"),std::move(e)};
}

expression_ref make_gdrh(const vector<expression_ref>& guardquals, const expression_ref& exp)
{
    return expression_ref(AST_node("gdrh"), {expression_ref{AST_node("guards"),guardquals},exp});
}

expression_ref make_stmts(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Stmts"), stmts);
}

expression_ref make_infix(const string& infix, optional<int>& prec, vector<string>& op_names)
{
    expression_ref ops = new expression(AST_node("Ops"), make_String_vec(op_names));

    vector<expression_ref> e;
    e.push_back(String(infix));
    if (prec)
	e.push_back(*prec);
    e.push_back(ops);

    return new expression(AST_node("FixityDecl"),e);
}

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return make_list(chars);
}

