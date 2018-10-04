// A Bison parser, made by GNU Bison 3.1.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018 Free Software Foundation, Inc.

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


// First part of user declarations.

#line 37 "parser.cc" // lalr1.cc:407

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

#include "parser.hh"

// User implementation prologue.

#line 51 "parser.cc" // lalr1.cc:415
// Unqualified %code blocks.
#line 77 "parser.y" // lalr1.cc:416

# include "driver.hh"

#line 57 "parser.cc" // lalr1.cc:416


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
    while (/*CONSTCOND*/ false)
# endif


// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

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
      yystack_print_ ();                \
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
#line 152 "parser.cc" // lalr1.cc:491

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
        std::string yyr = "";
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
              // Fall through.
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


  /// Build a parser object.
  parser::parser (driver& drv_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      drv (drv_yyarg)
  {}

  parser::~parser ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/



  // by_state.
  parser::by_state::by_state ()
    : state (empty_state)
  {}

  parser::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  void
  parser::by_state::clear ()
  {
    state = empty_state;
  }

  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  parser::by_state::by_state (state_type s)
    : state (s)
  {}

  parser::symbol_number_type
  parser::by_state::type_get () const
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
  }

  parser::stack_symbol_type::stack_symbol_type ()
  {}

  parser::stack_symbol_type::stack_symbol_type (const stack_symbol_type& that)
    : super_type (that.state, that.location)
  {
    switch (that.type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (that.value);
        break;

      case 172: // prec
        value.copy< boost::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< boost::optional<std::string> > (that.value);
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.copy< char > (that.value);
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
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
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 285: // ralt
      case 287: // ifgdpats
      case 288: // gdpat
      case 289: // pat
      case 290: // bindpat
      case 291: // apat
      case 295: // stmt
      case 296: // qual
      case 343: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 346: // commas
        value.copy< int > (that.value);
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 300: // qcon
      case 301: // gen_qcon
      case 302: // con
      case 304: // sysdcon_no_list
      case 305: // sysdcon
      case 306: // conop
      case 307: // qconop
      case 308: // gtycon
      case 309: // ntgtycon
      case 310: // oqtycon
      case 311: // oqtycon_no_varcon
      case 312: // qtyconop
      case 313: // qtycondoc
      case 314: // qtycon
      case 315: // tycon
      case 316: // qtyconsym
      case 317: // tyconsym
      case 318: // op
      case 319: // varop
      case 320: // qop
      case 321: // qopm
      case 322: // hole_op
      case 323: // qvarop
      case 324: // qvaropm
      case 325: // tyvar
      case 326: // tyvarop
      case 327: // tyvarid
      case 328: // var
      case 329: // qvar
      case 330: // qvarid
      case 331: // varid
      case 332: // qvarsym
      case 333: // qvarsym_no_minus
      case 334: // qvarsym1
      case 335: // varsym
      case 336: // varsym_no_minus
      case 337: // special_id
      case 338: // special_sym
      case 339: // qconid
      case 340: // conid
      case 341: // qconsym
      case 342: // consym
      case 345: // modid
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
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 286: // gdpats
      case 292: // apats1
      case 293: // stmtlist
      case 294: // stmts
        value.copy< std::vector<expression_ref> > (that.value);
        break;

      case 174: // ops
        value.copy< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

  }

  parser::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s, that.location)
  {
    switch (that.type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.move< bool > (that.value);
        break;

      case 172: // prec
        value.move< boost::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< boost::optional<std::string> > (that.value);
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.move< char > (that.value);
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
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
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 285: // ralt
      case 287: // ifgdpats
      case 288: // gdpat
      case 289: // pat
      case 290: // bindpat
      case 291: // apat
      case 295: // stmt
      case 296: // qual
      case 343: // literal
        value.move< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 346: // commas
        value.move< int > (that.value);
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 300: // qcon
      case 301: // gen_qcon
      case 302: // con
      case 304: // sysdcon_no_list
      case 305: // sysdcon
      case 306: // conop
      case 307: // qconop
      case 308: // gtycon
      case 309: // ntgtycon
      case 310: // oqtycon
      case 311: // oqtycon_no_varcon
      case 312: // qtyconop
      case 313: // qtycondoc
      case 314: // qtycon
      case 315: // tycon
      case 316: // qtyconsym
      case 317: // tyconsym
      case 318: // op
      case 319: // varop
      case 320: // qop
      case 321: // qopm
      case 322: // hole_op
      case 323: // qvarop
      case 324: // qvaropm
      case 325: // tyvar
      case 326: // tyvarop
      case 327: // tyvarid
      case 328: // var
      case 329: // qvar
      case 330: // qvarid
      case 331: // varid
      case 332: // qvarsym
      case 333: // qvarsym_no_minus
      case 334: // qvarsym1
      case 335: // varsym
      case 336: // varsym_no_minus
      case 337: // special_id
      case 338: // special_sym
      case 339: // qconid
      case 340: // conid
      case 341: // qconsym
      case 342: // consym
      case 345: // modid
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
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 286: // gdpats
      case 292: // apats1
      case 293: // stmtlist
      case 294: // stmts
        value.move< std::vector<expression_ref> > (that.value);
        break;

      case 174: // ops
        value.move< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

    // that is emptied.
    that.type = empty_symbol;
  }

  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    switch (that.type_get ())
    {
      case 165: // maybe_src
      case 166: // maybe_safe
      case 168: // optqualified
        value.copy< bool > (that.value);
        break;

      case 172: // prec
        value.copy< boost::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< boost::optional<std::string> > (that.value);
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        value.copy< char > (that.value);
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
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
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 285: // ralt
      case 287: // ifgdpats
      case 288: // gdpat
      case 289: // pat
      case 290: // bindpat
      case 291: // apat
      case 295: // stmt
      case 296: // qual
      case 343: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 346: // commas
        value.copy< int > (that.value);
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 300: // qcon
      case 301: // gen_qcon
      case 302: // con
      case 304: // sysdcon_no_list
      case 305: // sysdcon
      case 306: // conop
      case 307: // qconop
      case 308: // gtycon
      case 309: // ntgtycon
      case 310: // oqtycon
      case 311: // oqtycon_no_varcon
      case 312: // qtyconop
      case 313: // qtycondoc
      case 314: // qtycon
      case 315: // tycon
      case 316: // qtyconsym
      case 317: // tyconsym
      case 318: // op
      case 319: // varop
      case 320: // qop
      case 321: // qopm
      case 322: // hole_op
      case 323: // qvarop
      case 324: // qvaropm
      case 325: // tyvar
      case 326: // tyvarop
      case 327: // tyvarid
      case 328: // var
      case 329: // qvar
      case 330: // qvarid
      case 331: // varid
      case 332: // qvarsym
      case 333: // qvarsym_no_minus
      case 334: // qvarsym1
      case 335: // varsym
      case 336: // varsym_no_minus
      case 337: // special_id
      case 338: // special_sym
      case 339: // qconid
      case 340: // conid
      case 341: // qconsym
      case 342: // consym
      case 345: // modid
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
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 286: // gdpats
      case 292: // apats1
      case 293: // stmtlist
      case 294: // stmts
        value.copy< std::vector<expression_ref> > (that.value);
        break;

      case 174: // ops
        value.copy< std::vector<std::string> > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    return *this;
  }


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
  parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " ("
        << yysym.location << ": ";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  void
  parser::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  void
  parser::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  void
  parser::yypop_ (unsigned n)
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
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
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
  parser::parse ()
  {
    // State.
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
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << '\n';

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
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
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

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
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
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
        yylhs.value.build< bool > ();
        break;

      case 172: // prec
        yylhs.value.build< boost::optional<int> > ();
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        yylhs.value.build< boost::optional<std::string> > ();
        break;

      case 127: // "CHAR"
      case 131: // "PRIMCHAR"
        yylhs.value.build< char > ();
        break;

      case 130: // "RATIONAL"
      case 136: // "PRIMDOUBLE"
        yylhs.value.build< double > ();
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
      case 177: // topdecl
      case 179: // ty_decl
      case 186: // tycl_hdr
      case 204: // binds
      case 205: // wherebinds
      case 208: // opt_sig
      case 209: // opt_tyconsig
      case 210: // sigtype
      case 211: // sigtypedoc
      case 217: // ctype
      case 218: // ctypedoc
      case 219: // context
      case 220: // context_no_ops
      case 221: // type
      case 222: // typedoc
      case 223: // btype
      case 226: // tyapp
      case 227: // atype_docs
      case 228: // atype
      case 235: // tv_bndr
      case 240: // kind
      case 243: // constr
      case 244: // forall
      case 245: // constr_stuff
      case 248: // fielddecl
      case 253: // decl_no_th
      case 254: // decl
      case 255: // rhs
      case 257: // gdrh
      case 258: // sigdecl
      case 261: // exp
      case 264: // exp10_top
      case 265: // exp10
      case 269: // aexp
      case 270: // aexp1
      case 271: // aexp2
      case 272: // texp
      case 274: // list
      case 277: // transformqual
      case 283: // alt
      case 284: // alt_rhs
      case 285: // ralt
      case 287: // ifgdpats
      case 288: // gdpat
      case 289: // pat
      case 290: // bindpat
      case 291: // apat
      case 295: // stmt
      case 296: // qual
      case 343: // literal
        yylhs.value.build< expression_ref > ();
        break;

      case 135: // "PRIMFLOAT"
        yylhs.value.build< float > ();
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 346: // commas
        yylhs.value.build< int > ();
        break;

      case 117: // "VARID"
      case 118: // "CONID"
      case 119: // "VARSYM"
      case 120: // "CONSYM"
      case 121: // "QVARID"
      case 122: // "QCONID"
      case 123: // "QVARSYM"
      case 124: // "QCONSYM"
      case 125: // "IPDUPVARID"
      case 126: // "LABELVARID"
      case 128: // "STRING"
      case 132: // "PRIMSTRING"
      case 173: // infix
      case 184: // data_or_newtype
      case 214: // strict_mark
      case 215: // strictness
      case 300: // qcon
      case 301: // gen_qcon
      case 302: // con
      case 304: // sysdcon_no_list
      case 305: // sysdcon
      case 306: // conop
      case 307: // qconop
      case 308: // gtycon
      case 309: // ntgtycon
      case 310: // oqtycon
      case 311: // oqtycon_no_varcon
      case 312: // qtyconop
      case 313: // qtycondoc
      case 314: // qtycon
      case 315: // tycon
      case 316: // qtyconsym
      case 317: // tyconsym
      case 318: // op
      case 319: // varop
      case 320: // qop
      case 321: // qopm
      case 322: // hole_op
      case 323: // qvarop
      case 324: // qvaropm
      case 325: // tyvar
      case 326: // tyvarop
      case 327: // tyvarid
      case 328: // var
      case 329: // qvar
      case 330: // qvarid
      case 331: // varid
      case 332: // qvarsym
      case 333: // qvarsym_no_minus
      case 334: // qvarsym1
      case 335: // varsym
      case 336: // varsym_no_minus
      case 337: // special_id
      case 338: // special_sym
      case 339: // qconid
      case 340: // conid
      case 341: // qconsym
      case 342: // consym
      case 345: // modid
        yylhs.value.build< std::string > ();
        break;

      case 151: // exportlist
      case 152: // exportlist1
      case 155: // qcnames
      case 156: // qcnames1
      case 162: // importdecls
      case 163: // importdecls_semi
      case 175: // topdecls
      case 176: // topdecls_semi
      case 202: // decls
      case 203: // decllist
      case 212: // sig_vars
      case 213: // sigtypes1
      case 224: // btype_no_ops
      case 225: // tyapps
      case 231: // comma_types0
      case 232: // comma_types1
      case 234: // tv_bndrs
      case 241: // constrs
      case 242: // constrs1
      case 246: // fielddecls
      case 247: // fielddecls1
      case 256: // gdrhs
      case 262: // infixexp
      case 263: // infixexp_top
      case 268: // fexp
      case 273: // tup_exprs
      case 275: // lexps
      case 276: // squals
      case 278: // guardquals
      case 279: // guardquals1
      case 280: // altslist
      case 281: // alts
      case 282: // alts1
      case 286: // gdpats
      case 292: // apats1
      case 293: // stmtlist
      case 294: // stmts
        yylhs.value.build< std::vector<expression_ref> > ();
        break;

      case 174: // ops
        yylhs.value.build< std::vector<std::string> > ();
        break;

      default:
        break;
    }


      // Default location.
      {
        slice<stack_symbol_type, stack_type> slice (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, slice, yylen);
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
#line 504 "parser.y" // lalr1.cc:870
    {drv.result = yystack_[0].value.as< expression_ref > ();}
#line 1354 "parser.cc" // lalr1.cc:870
    break;

  case 3:
#line 521 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_module(yystack_[4].value.as< std::string > (),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1360 "parser.cc" // lalr1.cc:870
    break;

  case 4:
#line 522 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_module("",{},yystack_[0].value.as< expression_ref > ());}
#line 1366 "parser.cc" // lalr1.cc:870
    break;

  case 9:
#line 532 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1372 "parser.cc" // lalr1.cc:870
    break;

  case 10:
#line 533 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1378 "parser.cc" // lalr1.cc:870
    break;

  case 11:
#line 535 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1384 "parser.cc" // lalr1.cc:870
    break;

  case 12:
#line 536 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1390 "parser.cc" // lalr1.cc:870
    break;

  case 13:
#line 539 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1396 "parser.cc" // lalr1.cc:870
    break;

  case 14:
#line 541 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1402 "parser.cc" // lalr1.cc:870
    break;

  case 15:
#line 542 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1408 "parser.cc" // lalr1.cc:870
    break;

  case 16:
#line 543 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[0].value.as< std::vector<expression_ref> > (),{});}
#line 1414 "parser.cc" // lalr1.cc:870
    break;

  case 17:
#line 551 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_exports(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1420 "parser.cc" // lalr1.cc:870
    break;

  case 18:
#line 552 "parser.y" // lalr1.cc:870
    {}
#line 1426 "parser.cc" // lalr1.cc:870
    break;

  case 19:
#line 554 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1432 "parser.cc" // lalr1.cc:870
    break;

  case 20:
#line 556 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1438 "parser.cc" // lalr1.cc:870
    break;

  case 21:
#line 557 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1444 "parser.cc" // lalr1.cc:870
    break;

  case 22:
#line 559 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1450 "parser.cc" // lalr1.cc:870
    break;

  case 23:
#line 560 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("module"),{String(yystack_[0].value.as< std::string > ())});}
#line 1456 "parser.cc" // lalr1.cc:870
    break;

  case 24:
#line 561 "parser.y" // lalr1.cc:870
    {}
#line 1462 "parser.cc" // lalr1.cc:870
    break;

  case 27:
#line 566 "parser.y" // lalr1.cc:870
    {}
#line 1468 "parser.cc" // lalr1.cc:870
    break;

  case 28:
#line 567 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1474 "parser.cc" // lalr1.cc:870
    break;

  case 29:
#line 569 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[3].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ());}
#line 1480 "parser.cc" // lalr1.cc:870
    break;

  case 30:
#line 570 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1486 "parser.cc" // lalr1.cc:870
    break;

  case 31:
#line 572 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1492 "parser.cc" // lalr1.cc:870
    break;

  case 32:
#line 573 "parser.y" // lalr1.cc:870
    {}
#line 1498 "parser.cc" // lalr1.cc:870
    break;

  case 33:
#line 575 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1504 "parser.cc" // lalr1.cc:870
    break;

  case 34:
#line 576 "parser.y" // lalr1.cc:870
    {}
#line 1510 "parser.cc" // lalr1.cc:870
    break;

  case 35:
#line 578 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("qvar"),{String(yystack_[0].value.as< std::string > ())});}
#line 1516 "parser.cc" // lalr1.cc:870
    break;

  case 36:
#line 579 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("qvar"),{String(yystack_[0].value.as< std::string > ())});}
#line 1522 "parser.cc" // lalr1.cc:870
    break;

  case 41:
#line 589 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()), yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ()); }
#line 1528 "parser.cc" // lalr1.cc:870
    break;

  case 42:
#line 591 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ()); }
#line 1534 "parser.cc" // lalr1.cc:870
    break;

  case 43:
#line 592 "parser.y" // lalr1.cc:870
    { }
#line 1540 "parser.cc" // lalr1.cc:870
    break;

  case 44:
#line 594 "parser.y" // lalr1.cc:870
    {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as< bool > ()) e.push_back(std::string("qualified"));
    e.push_back(String(yystack_[2].value.as< std::string > ()));
    yylhs.value.as< expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 1551 "parser.cc" // lalr1.cc:870
    break;

  case 45:
#line 601 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1557 "parser.cc" // lalr1.cc:870
    break;

  case 46:
#line 602 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1563 "parser.cc" // lalr1.cc:870
    break;

  case 47:
#line 604 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1569 "parser.cc" // lalr1.cc:870
    break;

  case 48:
#line 605 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1575 "parser.cc" // lalr1.cc:870
    break;

  case 49:
#line 607 "parser.y" // lalr1.cc:870
    { yylhs.value.as< boost::optional<std::string> > () = yystack_[0].value.as< std::string > (); }
#line 1581 "parser.cc" // lalr1.cc:870
    break;

  case 50:
#line 608 "parser.y" // lalr1.cc:870
    { }
#line 1587 "parser.cc" // lalr1.cc:870
    break;

  case 51:
#line 610 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1593 "parser.cc" // lalr1.cc:870
    break;

  case 52:
#line 611 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1599 "parser.cc" // lalr1.cc:870
    break;

  case 53:
#line 613 "parser.y" // lalr1.cc:870
    { yylhs.value.as< boost::optional<std::string> > () = yystack_[0].value.as< std::string > (); }
#line 1605 "parser.cc" // lalr1.cc:870
    break;

  case 54:
#line 614 "parser.y" // lalr1.cc:870
    { }
#line 1611 "parser.cc" // lalr1.cc:870
    break;

  case 59:
#line 625 "parser.y" // lalr1.cc:870
    { }
#line 1617 "parser.cc" // lalr1.cc:870
    break;

  case 60:
#line 626 "parser.y" // lalr1.cc:870
    { yylhs.value.as< boost::optional<int> > () = yystack_[0].value.as< int > (); }
#line 1623 "parser.cc" // lalr1.cc:870
    break;

  case 61:
#line 628 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infix";  }
#line 1629 "parser.cc" // lalr1.cc:870
    break;

  case 62:
#line 629 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infixl"; }
#line 1635 "parser.cc" // lalr1.cc:870
    break;

  case 63:
#line 630 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infixr"; }
#line 1641 "parser.cc" // lalr1.cc:870
    break;

  case 64:
#line 632 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<std::string> > (),yystack_[2].value.as< std::vector<std::string> > ()); yylhs.value.as< std::vector<std::string> > ().push_back(yystack_[0].value.as< std::string > ()); }
#line 1647 "parser.cc" // lalr1.cc:870
    break;

  case 65:
#line 633 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::vector<std::string> > () = {yystack_[0].value.as< std::string > ()}; }
#line 1653 "parser.cc" // lalr1.cc:870
    break;

  case 66:
#line 637 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ()); }
#line 1659 "parser.cc" // lalr1.cc:870
    break;

  case 67:
#line 639 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ()); }
#line 1665 "parser.cc" // lalr1.cc:870
    break;

  case 68:
#line 640 "parser.y" // lalr1.cc:870
    { }
#line 1671 "parser.cc" // lalr1.cc:870
    break;

  case 69:
#line 642 "parser.y" // lalr1.cc:870
    {}
#line 1677 "parser.cc" // lalr1.cc:870
    break;

  case 70:
#line 643 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1683 "parser.cc" // lalr1.cc:870
    break;

  case 71:
#line 644 "parser.y" // lalr1.cc:870
    {}
#line 1689 "parser.cc" // lalr1.cc:870
    break;

  case 72:
#line 647 "parser.y" // lalr1.cc:870
    {}
#line 1695 "parser.cc" // lalr1.cc:870
    break;

  case 73:
#line 654 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1701 "parser.cc" // lalr1.cc:870
    break;

  case 74:
#line 655 "parser.y" // lalr1.cc:870
    {}
#line 1707 "parser.cc" // lalr1.cc:870
    break;

  case 75:
#line 656 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[3].value.as< std::string > (),yystack_[2].value.as< int > (),yystack_[1].value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1713 "parser.cc" // lalr1.cc:870
    break;

  case 76:
#line 657 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[2].value.as< std::string > (),yystack_[1].value.as< int > (),yystack_[0].value.as< std::string > ());}
#line 1719 "parser.cc" // lalr1.cc:870
    break;

  case 77:
#line 658 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[3].value.as< std::string > (),yystack_[2].value.as< int > (),yystack_[1].value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1725 "parser.cc" // lalr1.cc:870
    break;

  case 78:
#line 659 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[2].value.as< std::string > (),yystack_[1].value.as< int > (),yystack_[0].value.as< std::string > ());}
#line 1731 "parser.cc" // lalr1.cc:870
    break;

  case 80:
#line 663 "parser.y" // lalr1.cc:870
    {}
#line 1737 "parser.cc" // lalr1.cc:870
    break;

  case 81:
#line 665 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_data_or_newtype(yystack_[4].value.as< std::string > (),yystack_[2].value.as< expression_ref > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1743 "parser.cc" // lalr1.cc:870
    break;

  case 82:
#line 666 "parser.y" // lalr1.cc:870
    {}
#line 1749 "parser.cc" // lalr1.cc:870
    break;

  case 95:
#line 724 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > ()="data";}
#line 1755 "parser.cc" // lalr1.cc:870
    break;

  case 96:
#line 725 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > ()="newtype";}
#line 1761 "parser.cc" // lalr1.cc:870
    break;

  case 99:
#line 737 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1767 "parser.cc" // lalr1.cc:870
    break;

  case 100:
#line 738 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1773 "parser.cc" // lalr1.cc:870
    break;

  case 135:
#line 807 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1779 "parser.cc" // lalr1.cc:870
    break;

  case 136:
#line 808 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1785 "parser.cc" // lalr1.cc:870
    break;

  case 137:
#line 809 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1791 "parser.cc" // lalr1.cc:870
    break;

  case 138:
#line 810 "parser.y" // lalr1.cc:870
    {}
#line 1797 "parser.cc" // lalr1.cc:870
    break;

  case 139:
#line 812 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1803 "parser.cc" // lalr1.cc:870
    break;

  case 140:
#line 813 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1809 "parser.cc" // lalr1.cc:870
    break;

  case 141:
#line 815 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decls"),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1815 "parser.cc" // lalr1.cc:870
    break;

  case 142:
#line 821 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1821 "parser.cc" // lalr1.cc:870
    break;

  case 143:
#line 822 "parser.y" // lalr1.cc:870
    {}
#line 1827 "parser.cc" // lalr1.cc:870
    break;

  case 149:
#line 843 "parser.y" // lalr1.cc:870
    {}
#line 1833 "parser.cc" // lalr1.cc:870
    break;

  case 150:
#line 844 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1839 "parser.cc" // lalr1.cc:870
    break;

  case 151:
#line 846 "parser.y" // lalr1.cc:870
    {}
#line 1845 "parser.cc" // lalr1.cc:870
    break;

  case 152:
#line 847 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 1851 "parser.cc" // lalr1.cc:870
    break;

  case 153:
#line 849 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1857 "parser.cc" // lalr1.cc:870
    break;

  case 154:
#line 851 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1863 "parser.cc" // lalr1.cc:870
    break;

  case 155:
#line 853 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< std::string > ());}
#line 1869 "parser.cc" // lalr1.cc:870
    break;

  case 156:
#line 854 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< std::string > ());}
#line 1875 "parser.cc" // lalr1.cc:870
    break;

  case 157:
#line 856 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1881 "parser.cc" // lalr1.cc:870
    break;

  case 158:
#line 857 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1887 "parser.cc" // lalr1.cc:870
    break;

  case 159:
#line 861 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1893 "parser.cc" // lalr1.cc:870
    break;

  case 160:
#line 862 "parser.y" // lalr1.cc:870
    {}
#line 1899 "parser.cc" // lalr1.cc:870
    break;

  case 161:
#line 863 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1905 "parser.cc" // lalr1.cc:870
    break;

  case 162:
#line 865 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = "!";}
#line 1911 "parser.cc" // lalr1.cc:870
    break;

  case 163:
#line 866 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = "~";}
#line 1917 "parser.cc" // lalr1.cc:870
    break;

  case 166:
#line 871 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("forall"),{make_tv_bndrs(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 1923 "parser.cc" // lalr1.cc:870
    break;

  case 167:
#line 872 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1929 "parser.cc" // lalr1.cc:870
    break;

  case 168:
#line 874 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1935 "parser.cc" // lalr1.cc:870
    break;

  case 169:
#line 876 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1941 "parser.cc" // lalr1.cc:870
    break;

  case 170:
#line 885 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1947 "parser.cc" // lalr1.cc:870
    break;

  case 171:
#line 887 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1953 "parser.cc" // lalr1.cc:870
    break;

  case 172:
#line 889 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1959 "parser.cc" // lalr1.cc:870
    break;

  case 173:
#line 890 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps({make_type_id("->"),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1965 "parser.cc" // lalr1.cc:870
    break;

  case 174:
#line 892 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1971 "parser.cc" // lalr1.cc:870
    break;

  case 175:
#line 895 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1977 "parser.cc" // lalr1.cc:870
    break;

  case 176:
#line 897 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1983 "parser.cc" // lalr1.cc:870
    break;

  case 177:
#line 898 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1989 "parser.cc" // lalr1.cc:870
    break;

  case 178:
#line 900 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1995 "parser.cc" // lalr1.cc:870
    break;

  case 179:
#line 901 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2001 "parser.cc" // lalr1.cc:870
    break;

  case 180:
#line 903 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2007 "parser.cc" // lalr1.cc:870
    break;

  case 181:
#line 904 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2013 "parser.cc" // lalr1.cc:870
    break;

  case 182:
#line 905 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2019 "parser.cc" // lalr1.cc:870
    break;

  case 183:
#line 911 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2025 "parser.cc" // lalr1.cc:870
    break;

  case 184:
#line 913 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2031 "parser.cc" // lalr1.cc:870
    break;

  case 185:
#line 914 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2037 "parser.cc" // lalr1.cc:870
    break;

  case 186:
#line 915 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("kind_star");}
#line 2043 "parser.cc" // lalr1.cc:870
    break;

  case 187:
#line 916 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("strictness"),{yystack_[1].value.as< std::string > (),yystack_[0].value.as< expression_ref > ()}};}
#line 2049 "parser.cc" // lalr1.cc:870
    break;

  case 188:
#line 917 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("FieldDecls"),yystack_[1].value.as< std::vector<expression_ref> > ()};}
#line 2055 "parser.cc" // lalr1.cc:870
    break;

  case 189:
#line 918 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id("()");}
#line 2061 "parser.cc" // lalr1.cc:870
    break;

  case 190:
#line 919 "parser.y" // lalr1.cc:870
    {auto ts = yystack_[3].value.as< std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as< expression_ref > ());yylhs.value.as< expression_ref > () = expression_ref{AST_node("TupleType"),ts};}
#line 2067 "parser.cc" // lalr1.cc:870
    break;

  case 191:
#line 920 "parser.y" // lalr1.cc:870
    {}
#line 2073 "parser.cc" // lalr1.cc:870
    break;

  case 192:
#line 921 "parser.y" // lalr1.cc:870
    {}
#line 2079 "parser.cc" // lalr1.cc:870
    break;

  case 193:
#line 922 "parser.y" // lalr1.cc:870
    {}
#line 2085 "parser.cc" // lalr1.cc:870
    break;

  case 194:
#line 923 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("ListType"),{yystack_[1].value.as< expression_ref > ()}};}
#line 2091 "parser.cc" // lalr1.cc:870
    break;

  case 195:
#line 924 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2097 "parser.cc" // lalr1.cc:870
    break;

  case 196:
#line 925 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("TypeOfKind"),{yystack_[3].value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ()}};}
#line 2103 "parser.cc" // lalr1.cc:870
    break;

  case 200:
#line 933 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2109 "parser.cc" // lalr1.cc:870
    break;

  case 201:
#line 934 "parser.y" // lalr1.cc:870
    {}
#line 2115 "parser.cc" // lalr1.cc:870
    break;

  case 202:
#line 936 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2121 "parser.cc" // lalr1.cc:870
    break;

  case 203:
#line 937 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2127 "parser.cc" // lalr1.cc:870
    break;

  case 206:
#line 942 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2133 "parser.cc" // lalr1.cc:870
    break;

  case 207:
#line 943 "parser.y" // lalr1.cc:870
    {}
#line 2139 "parser.cc" // lalr1.cc:870
    break;

  case 208:
#line 945 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("type_id",yystack_[0].value.as< std::string > ());}
#line 2145 "parser.cc" // lalr1.cc:870
    break;

  case 209:
#line 946 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("type_of_kind"),{AST_node("type_id",yystack_[3].value.as< std::string > ()),yystack_[1].value.as< expression_ref > ()});}
#line 2151 "parser.cc" // lalr1.cc:870
    break;

  case 217:
#line 961 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2157 "parser.cc" // lalr1.cc:870
    break;

  case 218:
#line 967 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2163 "parser.cc" // lalr1.cc:870
    break;

  case 219:
#line 969 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2169 "parser.cc" // lalr1.cc:870
    break;

  case 220:
#line 970 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2175 "parser.cc" // lalr1.cc:870
    break;

  case 221:
#line 972 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_context(yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2181 "parser.cc" // lalr1.cc:870
    break;

  case 222:
#line 973 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2187 "parser.cc" // lalr1.cc:870
    break;

  case 223:
#line 975 "parser.y" // lalr1.cc:870
    {if (yystack_[1].value.as< std::vector<expression_ref> > ().size()>1) yylhs.value.as< expression_ref > () = make_tv_bndrs(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2193 "parser.cc" // lalr1.cc:870
    break;

  case 224:
#line 976 "parser.y" // lalr1.cc:870
    {}
#line 2199 "parser.cc" // lalr1.cc:870
    break;

  case 225:
#line 978 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2205 "parser.cc" // lalr1.cc:870
    break;

  case 226:
#line 979 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps({AST_node("type_id",yystack_[1].value.as< std::string > ()),make_tyapps(yystack_[2].value.as< std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ())});}
#line 2211 "parser.cc" // lalr1.cc:870
    break;

  case 227:
#line 981 "parser.y" // lalr1.cc:870
    {}
#line 2217 "parser.cc" // lalr1.cc:870
    break;

  case 228:
#line 982 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2223 "parser.cc" // lalr1.cc:870
    break;

  case 229:
#line 984 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2229 "parser.cc" // lalr1.cc:870
    break;

  case 230:
#line 985 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2235 "parser.cc" // lalr1.cc:870
    break;

  case 231:
#line 987 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("FieldDecl"),{make_sig_vars(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2241 "parser.cc" // lalr1.cc:870
    break;

  case 242:
#line 1006 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2247 "parser.cc" // lalr1.cc:870
    break;

  case 243:
#line 1007 "parser.y" // lalr1.cc:870
    {}
#line 2253 "parser.cc" // lalr1.cc:870
    break;

  case 244:
#line 1009 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2259 "parser.cc" // lalr1.cc:870
    break;

  case 245:
#line 1010 "parser.y" // lalr1.cc:870
    {}
#line 2265 "parser.cc" // lalr1.cc:870
    break;

  case 246:
#line 1013 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2271 "parser.cc" // lalr1.cc:870
    break;

  case 247:
#line 1016 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_rhs(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2277 "parser.cc" // lalr1.cc:870
    break;

  case 248:
#line 1017 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_rhs(make_gdrhs(yystack_[1].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ());}
#line 2283 "parser.cc" // lalr1.cc:870
    break;

  case 249:
#line 1019 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2289 "parser.cc" // lalr1.cc:870
    break;

  case 250:
#line 1020 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2295 "parser.cc" // lalr1.cc:870
    break;

  case 251:
#line 1022 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("guardquals"),{make_gdpats(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2301 "parser.cc" // lalr1.cc:870
    break;

  case 252:
#line 1024 "parser.y" // lalr1.cc:870
    {}
#line 2307 "parser.cc" // lalr1.cc:870
    break;

  case 253:
#line 1025 "parser.y" // lalr1.cc:870
    {}
#line 2313 "parser.cc" // lalr1.cc:870
    break;

  case 254:
#line 1026 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_infix(yystack_[2].value.as< std::string > (),yystack_[1].value.as< boost::optional<int> > (),yystack_[0].value.as< std::vector<std::string> > ()); }
#line 2319 "parser.cc" // lalr1.cc:870
    break;

  case 255:
#line 1027 "parser.y" // lalr1.cc:870
    {}
#line 2325 "parser.cc" // lalr1.cc:870
    break;

  case 256:
#line 1028 "parser.y" // lalr1.cc:870
    {}
#line 2331 "parser.cc" // lalr1.cc:870
    break;

  case 257:
#line 1029 "parser.y" // lalr1.cc:870
    {}
#line 2337 "parser.cc" // lalr1.cc:870
    break;

  case 258:
#line 1030 "parser.y" // lalr1.cc:870
    {}
#line 2343 "parser.cc" // lalr1.cc:870
    break;

  case 259:
#line 1031 "parser.y" // lalr1.cc:870
    {}
#line 2349 "parser.cc" // lalr1.cc:870
    break;

  case 260:
#line 1032 "parser.y" // lalr1.cc:870
    {}
#line 2355 "parser.cc" // lalr1.cc:870
    break;

  case 261:
#line 1033 "parser.y" // lalr1.cc:870
    {}
#line 2361 "parser.cc" // lalr1.cc:870
    break;

  case 262:
#line 1034 "parser.y" // lalr1.cc:870
    {}
#line 2367 "parser.cc" // lalr1.cc:870
    break;

  case 267:
#line 1044 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()); }
#line 2373 "parser.cc" // lalr1.cc:870
    break;

  case 268:
#line 1045 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_infixexp(yystack_[0].value.as< std::vector<expression_ref> > ()); }
#line 2379 "parser.cc" // lalr1.cc:870
    break;

  case 269:
#line 1047 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2385 "parser.cc" // lalr1.cc:870
    break;

  case 270:
#line 1048 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as< std::string > ())); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2391 "parser.cc" // lalr1.cc:870
    break;

  case 271:
#line 1050 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2397 "parser.cc" // lalr1.cc:870
    break;

  case 272:
#line 1051 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as< std::string > ())); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2403 "parser.cc" // lalr1.cc:870
    break;

  case 273:
#line 1053 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_minus(make_fexp(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2409 "parser.cc" // lalr1.cc:870
    break;

  case 274:
#line 1054 "parser.y" // lalr1.cc:870
    {}
#line 2415 "parser.cc" // lalr1.cc:870
    break;

  case 275:
#line 1055 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_fexp(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2421 "parser.cc" // lalr1.cc:870
    break;

  case 276:
#line 1057 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2427 "parser.cc" // lalr1.cc:870
    break;

  case 277:
#line 1058 "parser.y" // lalr1.cc:870
    {}
#line 2433 "parser.cc" // lalr1.cc:870
    break;

  case 282:
#line 1069 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2439 "parser.cc" // lalr1.cc:870
    break;

  case 283:
#line 1070 "parser.y" // lalr1.cc:870
    {}
#line 2445 "parser.cc" // lalr1.cc:870
    break;

  case 284:
#line 1071 "parser.y" // lalr1.cc:870
    {}
#line 2451 "parser.cc" // lalr1.cc:870
    break;

  case 285:
#line 1072 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2457 "parser.cc" // lalr1.cc:870
    break;

  case 286:
#line 1074 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_as_pattern(yystack_[2].value.as< std::string > (),yystack_[0].value.as< expression_ref > ());}
#line 2463 "parser.cc" // lalr1.cc:870
    break;

  case 287:
#line 1075 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_lazy_pattern(yystack_[0].value.as< expression_ref > ());}
#line 2469 "parser.cc" // lalr1.cc:870
    break;

  case 288:
#line 1076 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_lambda(yystack_[2].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2475 "parser.cc" // lalr1.cc:870
    break;

  case 289:
#line 1077 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_let(yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2481 "parser.cc" // lalr1.cc:870
    break;

  case 290:
#line 1078 "parser.y" // lalr1.cc:870
    {}
#line 2487 "parser.cc" // lalr1.cc:870
    break;

  case 291:
#line 1079 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_if(yystack_[6].value.as< expression_ref > (),yystack_[3].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2493 "parser.cc" // lalr1.cc:870
    break;

  case 292:
#line 1080 "parser.y" // lalr1.cc:870
    {}
#line 2499 "parser.cc" // lalr1.cc:870
    break;

  case 293:
#line 1081 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_case(yystack_[2].value.as< expression_ref > (),make_alts(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2505 "parser.cc" // lalr1.cc:870
    break;

  case 294:
#line 1082 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_do(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2511 "parser.cc" // lalr1.cc:870
    break;

  case 295:
#line 1083 "parser.y" // lalr1.cc:870
    {}
#line 2517 "parser.cc" // lalr1.cc:870
    break;

  case 296:
#line 1084 "parser.y" // lalr1.cc:870
    {}
#line 2523 "parser.cc" // lalr1.cc:870
    break;

  case 297:
#line 1085 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2529 "parser.cc" // lalr1.cc:870
    break;

  case 298:
#line 1087 "parser.y" // lalr1.cc:870
    {}
#line 2535 "parser.cc" // lalr1.cc:870
    break;

  case 299:
#line 1088 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2541 "parser.cc" // lalr1.cc:870
    break;

  case 300:
#line 1090 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_id(yystack_[0].value.as< std::string > ());}
#line 2547 "parser.cc" // lalr1.cc:870
    break;

  case 301:
#line 1091 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_id(yystack_[0].value.as< std::string > ());}
#line 2553 "parser.cc" // lalr1.cc:870
    break;

  case 302:
#line 1092 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2559 "parser.cc" // lalr1.cc:870
    break;

  case 303:
#line 1093 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2565 "parser.cc" // lalr1.cc:870
    break;

  case 304:
#line 1094 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_tuple(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2571 "parser.cc" // lalr1.cc:870
    break;

  case 305:
#line 1095 "parser.y" // lalr1.cc:870
    {}
#line 2577 "parser.cc" // lalr1.cc:870
    break;

  case 306:
#line 1096 "parser.y" // lalr1.cc:870
    {}
#line 2583 "parser.cc" // lalr1.cc:870
    break;

  case 307:
#line 1097 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2589 "parser.cc" // lalr1.cc:870
    break;

  case 308:
#line 1098 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("WildcardPattern");}
#line 2595 "parser.cc" // lalr1.cc:870
    break;

  case 309:
#line 1103 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2601 "parser.cc" // lalr1.cc:870
    break;

  case 310:
#line 1104 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< std::string > ()});}
#line 2607 "parser.cc" // lalr1.cc:870
    break;

  case 311:
#line 1105 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("RightSection"),{yystack_[1].value.as< std::string > (),make_infixexp(yystack_[0].value.as< std::vector<expression_ref> > ())});}
#line 2613 "parser.cc" // lalr1.cc:870
    break;

  case 312:
#line 1110 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2619 "parser.cc" // lalr1.cc:870
    break;

  case 313:
#line 1111 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2625 "parser.cc" // lalr1.cc:870
    break;

  case 314:
#line 1129 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = {AST_node("id",":"),yystack_[0].value.as< expression_ref > (),AST_node("id","[]")}; }
#line 2631 "parser.cc" // lalr1.cc:870
    break;

  case 315:
#line 1130 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_list(yystack_[0].value.as< std::vector<expression_ref> > ()); }
#line 2637 "parser.cc" // lalr1.cc:870
    break;

  case 316:
#line 1131 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as< expression_ref > ()}); }
#line 2643 "parser.cc" // lalr1.cc:870
    break;

  case 317:
#line 1132 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ()}); }
#line 2649 "parser.cc" // lalr1.cc:870
    break;

  case 318:
#line 1133 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2655 "parser.cc" // lalr1.cc:870
    break;

  case 319:
#line 1134 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFromToThen"),{yystack_[4].value.as< expression_ref > (),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2661 "parser.cc" // lalr1.cc:870
    break;

  case 320:
#line 1135 "parser.y" // lalr1.cc:870
    { auto quals = yystack_[0].value.as< std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 2667 "parser.cc" // lalr1.cc:870
    break;

  case 321:
#line 1137 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2673 "parser.cc" // lalr1.cc:870
    break;

  case 322:
#line 1138 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2679 "parser.cc" // lalr1.cc:870
    break;

  case 323:
#line 1150 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2685 "parser.cc" // lalr1.cc:870
    break;

  case 324:
#line 1151 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2691 "parser.cc" // lalr1.cc:870
    break;

  case 325:
#line 1152 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2697 "parser.cc" // lalr1.cc:870
    break;

  case 326:
#line 1153 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2703 "parser.cc" // lalr1.cc:870
    break;

  case 327:
#line 1155 "parser.y" // lalr1.cc:870
    {}
#line 2709 "parser.cc" // lalr1.cc:870
    break;

  case 328:
#line 1156 "parser.y" // lalr1.cc:870
    {}
#line 2715 "parser.cc" // lalr1.cc:870
    break;

  case 329:
#line 1157 "parser.y" // lalr1.cc:870
    {}
#line 2721 "parser.cc" // lalr1.cc:870
    break;

  case 330:
#line 1158 "parser.y" // lalr1.cc:870
    {}
#line 2727 "parser.cc" // lalr1.cc:870
    break;

  case 331:
#line 1161 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2733 "parser.cc" // lalr1.cc:870
    break;

  case 332:
#line 1163 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ());yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2739 "parser.cc" // lalr1.cc:870
    break;

  case 333:
#line 1164 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2745 "parser.cc" // lalr1.cc:870
    break;

  case 334:
#line 1167 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2751 "parser.cc" // lalr1.cc:870
    break;

  case 335:
#line 1168 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2757 "parser.cc" // lalr1.cc:870
    break;

  case 336:
#line 1169 "parser.y" // lalr1.cc:870
    {}
#line 2763 "parser.cc" // lalr1.cc:870
    break;

  case 337:
#line 1170 "parser.y" // lalr1.cc:870
    {}
#line 2769 "parser.cc" // lalr1.cc:870
    break;

  case 338:
#line 1172 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2775 "parser.cc" // lalr1.cc:870
    break;

  case 339:
#line 1173 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2781 "parser.cc" // lalr1.cc:870
    break;

  case 340:
#line 1175 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2787 "parser.cc" // lalr1.cc:870
    break;

  case 341:
#line 1176 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2793 "parser.cc" // lalr1.cc:870
    break;

  case 342:
#line 1177 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2799 "parser.cc" // lalr1.cc:870
    break;

  case 343:
#line 1179 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_alt(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2805 "parser.cc" // lalr1.cc:870
    break;

  case 344:
#line 1181 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_alt_rhs(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2811 "parser.cc" // lalr1.cc:870
    break;

  case 345:
#line 1183 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2817 "parser.cc" // lalr1.cc:870
    break;

  case 346:
#line 1184 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_gdpats(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2823 "parser.cc" // lalr1.cc:870
    break;

  case 347:
#line 1186 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2829 "parser.cc" // lalr1.cc:870
    break;

  case 348:
#line 1187 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2835 "parser.cc" // lalr1.cc:870
    break;

  case 349:
#line 1189 "parser.y" // lalr1.cc:870
    {}
#line 2841 "parser.cc" // lalr1.cc:870
    break;

  case 350:
#line 1190 "parser.y" // lalr1.cc:870
    {}
#line 2847 "parser.cc" // lalr1.cc:870
    break;

  case 351:
#line 1192 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > ()=make_gdpat(make_gdpats(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ());}
#line 2853 "parser.cc" // lalr1.cc:870
    break;

  case 352:
#line 1194 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2859 "parser.cc" // lalr1.cc:870
    break;

  case 353:
#line 1195 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("StrictPat"),{yystack_[0].value.as< expression_ref > ()});}
#line 2865 "parser.cc" // lalr1.cc:870
    break;

  case 354:
#line 1197 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2871 "parser.cc" // lalr1.cc:870
    break;

  case 355:
#line 1198 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("StrictPat"),{yystack_[0].value.as< expression_ref > ()});}
#line 2877 "parser.cc" // lalr1.cc:870
    break;

  case 356:
#line 1200 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2883 "parser.cc" // lalr1.cc:870
    break;

  case 357:
#line 1201 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("StrictPat"),{yystack_[0].value.as< expression_ref > ()});}
#line 2889 "parser.cc" // lalr1.cc:870
    break;

  case 358:
#line 1203 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2895 "parser.cc" // lalr1.cc:870
    break;

  case 359:
#line 1204 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2901 "parser.cc" // lalr1.cc:870
    break;

  case 360:
#line 1207 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2907 "parser.cc" // lalr1.cc:870
    break;

  case 361:
#line 1208 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2913 "parser.cc" // lalr1.cc:870
    break;

  case 362:
#line 1210 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2919 "parser.cc" // lalr1.cc:870
    break;

  case 363:
#line 1211 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2925 "parser.cc" // lalr1.cc:870
    break;

  case 364:
#line 1212 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2931 "parser.cc" // lalr1.cc:870
    break;

  case 365:
#line 1213 "parser.y" // lalr1.cc:870
    {}
#line 2937 "parser.cc" // lalr1.cc:870
    break;

  case 366:
#line 1218 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< expression_ref > ();}
#line 2943 "parser.cc" // lalr1.cc:870
    break;

  case 367:
#line 1219 "parser.y" // lalr1.cc:870
    {}
#line 2949 "parser.cc" // lalr1.cc:870
    break;

  case 368:
#line 1221 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("PatQual"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 2955 "parser.cc" // lalr1.cc:870
    break;

  case 369:
#line 1222 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("SimpleQual"),{yystack_[0].value.as< expression_ref > ()});}
#line 2961 "parser.cc" // lalr1.cc:870
    break;

  case 370:
#line 1223 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("LetQual"),{yystack_[0].value.as< expression_ref > ()});}
#line 2967 "parser.cc" // lalr1.cc:870
    break;

  case 378:
#line 1268 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 2973 "parser.cc" // lalr1.cc:870
    break;

  case 379:
#line 1269 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 2979 "parser.cc" // lalr1.cc:870
    break;

  case 380:
#line 1271 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 2985 "parser.cc" // lalr1.cc:870
    break;

  case 381:
#line 1272 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 2991 "parser.cc" // lalr1.cc:870
    break;

  case 382:
#line 1274 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 2997 "parser.cc" // lalr1.cc:870
    break;

  case 383:
#line 1275 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3003 "parser.cc" // lalr1.cc:870
    break;

  case 384:
#line 1276 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3009 "parser.cc" // lalr1.cc:870
    break;

  case 387:
#line 1281 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () =  "()"; }
#line 3015 "parser.cc" // lalr1.cc:870
    break;

  case 388:
#line 1282 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "("+std::string(yystack_[1].value.as< int > (),',')+")"; }
#line 3021 "parser.cc" // lalr1.cc:870
    break;

  case 389:
#line 1283 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(##)"; }
#line 3027 "parser.cc" // lalr1.cc:870
    break;

  case 390:
#line 1284 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(#"+std::string(yystack_[1].value.as< int > (),',')+"#)"; }
#line 3033 "parser.cc" // lalr1.cc:870
    break;

  case 391:
#line 1286 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3039 "parser.cc" // lalr1.cc:870
    break;

  case 392:
#line 1287 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "[]"; }
#line 3045 "parser.cc" // lalr1.cc:870
    break;

  case 393:
#line 1289 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3051 "parser.cc" // lalr1.cc:870
    break;

  case 394:
#line 1290 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3057 "parser.cc" // lalr1.cc:870
    break;

  case 395:
#line 1292 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3063 "parser.cc" // lalr1.cc:870
    break;

  case 396:
#line 1293 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3069 "parser.cc" // lalr1.cc:870
    break;

  case 397:
#line 1296 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3075 "parser.cc" // lalr1.cc:870
    break;

  case 398:
#line 1297 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "()"; }
#line 3081 "parser.cc" // lalr1.cc:870
    break;

  case 399:
#line 1298 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(##)"; }
#line 3087 "parser.cc" // lalr1.cc:870
    break;

  case 400:
#line 1300 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3093 "parser.cc" // lalr1.cc:870
    break;

  case 401:
#line 1301 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "("+std::string(yystack_[1].value.as< int > (),',')+")"; }
#line 3099 "parser.cc" // lalr1.cc:870
    break;

  case 402:
#line 1302 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(#"+std::string(yystack_[1].value.as< int > (),',')+"#)"; }
#line 3105 "parser.cc" // lalr1.cc:870
    break;

  case 403:
#line 1303 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "->"; }
#line 3111 "parser.cc" // lalr1.cc:870
    break;

  case 404:
#line 1304 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "[]"; }
#line 3117 "parser.cc" // lalr1.cc:870
    break;

  case 405:
#line 1306 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3123 "parser.cc" // lalr1.cc:870
    break;

  case 406:
#line 1307 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3129 "parser.cc" // lalr1.cc:870
    break;

  case 407:
#line 1308 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "~"; }
#line 3135 "parser.cc" // lalr1.cc:870
    break;

  case 408:
#line 1310 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3141 "parser.cc" // lalr1.cc:870
    break;

  case 409:
#line 1311 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3147 "parser.cc" // lalr1.cc:870
    break;

  case 410:
#line 1312 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3153 "parser.cc" // lalr1.cc:870
    break;

  case 411:
#line 1313 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3159 "parser.cc" // lalr1.cc:870
    break;

  case 412:
#line 1314 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "~"; }
#line 3165 "parser.cc" // lalr1.cc:870
    break;

  case 413:
#line 1317 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3171 "parser.cc" // lalr1.cc:870
    break;

  case 414:
#line 1318 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3177 "parser.cc" // lalr1.cc:870
    break;

  case 415:
#line 1320 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3183 "parser.cc" // lalr1.cc:870
    break;

  case 416:
#line 1322 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3189 "parser.cc" // lalr1.cc:870
    break;

  case 417:
#line 1323 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3195 "parser.cc" // lalr1.cc:870
    break;

  case 418:
#line 1327 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3201 "parser.cc" // lalr1.cc:870
    break;

  case 419:
#line 1329 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3207 "parser.cc" // lalr1.cc:870
    break;

  case 420:
#line 1330 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3213 "parser.cc" // lalr1.cc:870
    break;

  case 421:
#line 1331 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3219 "parser.cc" // lalr1.cc:870
    break;

  case 422:
#line 1333 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3225 "parser.cc" // lalr1.cc:870
    break;

  case 423:
#line 1334 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3231 "parser.cc" // lalr1.cc:870
    break;

  case 424:
#line 1335 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3237 "parser.cc" // lalr1.cc:870
    break;

  case 425:
#line 1336 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "-"; }
#line 3243 "parser.cc" // lalr1.cc:870
    break;

  case 426:
#line 1341 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3249 "parser.cc" // lalr1.cc:870
    break;

  case 427:
#line 1342 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3255 "parser.cc" // lalr1.cc:870
    break;

  case 428:
#line 1344 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3261 "parser.cc" // lalr1.cc:870
    break;

  case 429:
#line 1345 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3267 "parser.cc" // lalr1.cc:870
    break;

  case 430:
#line 1347 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3273 "parser.cc" // lalr1.cc:870
    break;

  case 431:
#line 1348 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3279 "parser.cc" // lalr1.cc:870
    break;

  case 432:
#line 1349 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3285 "parser.cc" // lalr1.cc:870
    break;

  case 433:
#line 1351 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3291 "parser.cc" // lalr1.cc:870
    break;

  case 434:
#line 1352 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3297 "parser.cc" // lalr1.cc:870
    break;

  case 435:
#line 1353 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3303 "parser.cc" // lalr1.cc:870
    break;

  case 436:
#line 1355 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "_"; }
#line 3309 "parser.cc" // lalr1.cc:870
    break;

  case 437:
#line 1357 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3315 "parser.cc" // lalr1.cc:870
    break;

  case 438:
#line 1358 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3321 "parser.cc" // lalr1.cc:870
    break;

  case 439:
#line 1360 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () =yystack_[0].value.as< std::string > (); }
#line 3327 "parser.cc" // lalr1.cc:870
    break;

  case 440:
#line 1361 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3333 "parser.cc" // lalr1.cc:870
    break;

  case 441:
#line 1365 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3339 "parser.cc" // lalr1.cc:870
    break;

  case 442:
#line 1367 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3345 "parser.cc" // lalr1.cc:870
    break;

  case 443:
#line 1369 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3351 "parser.cc" // lalr1.cc:870
    break;

  case 444:
#line 1370 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3357 "parser.cc" // lalr1.cc:870
    break;

  case 445:
#line 1371 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unsafe"; }
#line 3363 "parser.cc" // lalr1.cc:870
    break;

  case 446:
#line 1372 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "safe"; }
#line 3369 "parser.cc" // lalr1.cc:870
    break;

  case 447:
#line 1373 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "interruptible"; }
#line 3375 "parser.cc" // lalr1.cc:870
    break;

  case 448:
#line 1376 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3381 "parser.cc" // lalr1.cc:870
    break;

  case 449:
#line 1377 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3387 "parser.cc" // lalr1.cc:870
    break;

  case 450:
#line 1379 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3393 "parser.cc" // lalr1.cc:870
    break;

  case 451:
#line 1380 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3399 "parser.cc" // lalr1.cc:870
    break;

  case 452:
#line 1381 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3405 "parser.cc" // lalr1.cc:870
    break;

  case 453:
#line 1383 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3411 "parser.cc" // lalr1.cc:870
    break;

  case 454:
#line 1384 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3417 "parser.cc" // lalr1.cc:870
    break;

  case 455:
#line 1386 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3423 "parser.cc" // lalr1.cc:870
    break;

  case 456:
#line 1387 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3429 "parser.cc" // lalr1.cc:870
    break;

  case 457:
#line 1388 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unsafe"; }
#line 3435 "parser.cc" // lalr1.cc:870
    break;

  case 458:
#line 1389 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "safe"; }
#line 3441 "parser.cc" // lalr1.cc:870
    break;

  case 459:
#line 1390 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "interruptible"; }
#line 3447 "parser.cc" // lalr1.cc:870
    break;

  case 460:
#line 1391 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "forall"; }
#line 3453 "parser.cc" // lalr1.cc:870
    break;

  case 461:
#line 1392 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "family"; }
#line 3459 "parser.cc" // lalr1.cc:870
    break;

  case 462:
#line 1393 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "role"; }
#line 3465 "parser.cc" // lalr1.cc:870
    break;

  case 463:
#line 1395 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3471 "parser.cc" // lalr1.cc:870
    break;

  case 464:
#line 1396 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3477 "parser.cc" // lalr1.cc:870
    break;

  case 465:
#line 1398 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3483 "parser.cc" // lalr1.cc:870
    break;

  case 466:
#line 1399 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3489 "parser.cc" // lalr1.cc:870
    break;

  case 467:
#line 1401 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3495 "parser.cc" // lalr1.cc:870
    break;

  case 468:
#line 1403 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3501 "parser.cc" // lalr1.cc:870
    break;

  case 469:
#line 1404 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "-"; }
#line 3507 "parser.cc" // lalr1.cc:870
    break;

  case 470:
#line 1406 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3513 "parser.cc" // lalr1.cc:870
    break;

  case 471:
#line 1407 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3519 "parser.cc" // lalr1.cc:870
    break;

  case 472:
#line 1409 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "as"; }
#line 3525 "parser.cc" // lalr1.cc:870
    break;

  case 473:
#line 1410 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "qualified"; }
#line 3531 "parser.cc" // lalr1.cc:870
    break;

  case 474:
#line 1411 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "hiding"; }
#line 3537 "parser.cc" // lalr1.cc:870
    break;

  case 475:
#line 1412 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "export"; }
#line 3543 "parser.cc" // lalr1.cc:870
    break;

  case 476:
#line 1413 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "label"; }
#line 3549 "parser.cc" // lalr1.cc:870
    break;

  case 477:
#line 1414 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "dynamic"; }
#line 3555 "parser.cc" // lalr1.cc:870
    break;

  case 478:
#line 1415 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "stdcall"; }
#line 3561 "parser.cc" // lalr1.cc:870
    break;

  case 479:
#line 1416 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "ccall"; }
#line 3567 "parser.cc" // lalr1.cc:870
    break;

  case 480:
#line 1417 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "capi"; }
#line 3573 "parser.cc" // lalr1.cc:870
    break;

  case 481:
#line 1418 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "prim"; }
#line 3579 "parser.cc" // lalr1.cc:870
    break;

  case 482:
#line 1419 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "javascript"; }
#line 3585 "parser.cc" // lalr1.cc:870
    break;

  case 483:
#line 1420 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "group"; }
#line 3591 "parser.cc" // lalr1.cc:870
    break;

  case 484:
#line 1421 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "stock"; }
#line 3597 "parser.cc" // lalr1.cc:870
    break;

  case 485:
#line 1422 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "anyclass"; }
#line 3603 "parser.cc" // lalr1.cc:870
    break;

  case 486:
#line 1423 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "via"; }
#line 3609 "parser.cc" // lalr1.cc:870
    break;

  case 487:
#line 1424 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unit"; }
#line 3615 "parser.cc" // lalr1.cc:870
    break;

  case 488:
#line 1425 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "dependency"; }
#line 3621 "parser.cc" // lalr1.cc:870
    break;

  case 489:
#line 1426 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "signature"; }
#line 3627 "parser.cc" // lalr1.cc:870
    break;

  case 490:
#line 1428 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "!"; }
#line 3633 "parser.cc" // lalr1.cc:870
    break;

  case 491:
#line 1429 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "."; }
#line 3639 "parser.cc" // lalr1.cc:870
    break;

  case 492:
#line 1430 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "*"; }
#line 3645 "parser.cc" // lalr1.cc:870
    break;

  case 493:
#line 1434 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3651 "parser.cc" // lalr1.cc:870
    break;

  case 494:
#line 1435 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3657 "parser.cc" // lalr1.cc:870
    break;

  case 495:
#line 1437 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3663 "parser.cc" // lalr1.cc:870
    break;

  case 496:
#line 1439 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3669 "parser.cc" // lalr1.cc:870
    break;

  case 497:
#line 1440 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3675 "parser.cc" // lalr1.cc:870
    break;

  case 498:
#line 1442 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3681 "parser.cc" // lalr1.cc:870
    break;

  case 499:
#line 1443 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3687 "parser.cc" // lalr1.cc:870
    break;

  case 500:
#line 1447 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< char > ();}
#line 3693 "parser.cc" // lalr1.cc:870
    break;

  case 501:
#line 1448 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_string(yystack_[0].value.as< std::string > ());}
#line 3699 "parser.cc" // lalr1.cc:870
    break;

  case 502:
#line 1449 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< int > ();}
#line 3705 "parser.cc" // lalr1.cc:870
    break;

  case 503:
#line 1450 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< double > ();}
#line 3711 "parser.cc" // lalr1.cc:870
    break;

  case 505:
#line 1458 "parser.y" // lalr1.cc:870
    { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 3717 "parser.cc" // lalr1.cc:870
    break;

  case 506:
#line 1462 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3723 "parser.cc" // lalr1.cc:870
    break;

  case 507:
#line 1463 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3729 "parser.cc" // lalr1.cc:870
    break;

  case 508:
#line 1465 "parser.y" // lalr1.cc:870
    {yylhs.value.as< int > () = yystack_[1].value.as< int > () + 1;}
#line 3735 "parser.cc" // lalr1.cc:870
    break;

  case 509:
#line 1466 "parser.y" // lalr1.cc:870
    {yylhs.value.as< int > () = 1;}
#line 3741 "parser.cc" // lalr1.cc:870
    break;


#line 3745 "parser.cc" // lalr1.cc:870
            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
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
        error (yyla.location, yysyntax_error_ (yystack_[0].state, yyla));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
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

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
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

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
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

  // Generate an error message.
  std::string
  parser::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

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
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty ())
      {
        int yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

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
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short parser::yypact_ninf_ = -695;

  const short parser::yytable_ninf_ = -469;

  const short
  parser::yypact_[] =
  {
      28,   103,  -695,   104,  -695,  -695,  -695,  -695,  -695,    31,
      64,   101,  -695,    60,    42,    42,   137,  -695,  -695,  -695,
    -695,   250,  -695,  -695,  -695,   142,  -695,   202,   209,  4947,
     322,   301,   242,  -695,   831,  -695,    61,  -695,  -695,  -695,
    -695,   103,  -695,   -23,  -695,  -695,  -695,  -695,  -695,  -695,
    -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,   232,
    -695,  -695,  -695,  -695,  -695,  -695,   347,  -695,  -695,  -695,
    -695,   260,   265,  -695,   267,  -695,  -695,  -695,  -695,  -695,
    -695,  -695,  -695,    90,   334,   396,  -695,   320,  -695,  2897,
    -695,   333,   196,  2001,  -695,  -695,  -695,   272,   281,  -695,
    4513,  5211,   196,  3793,  5301,  3793,   338,   314,  5154,   344,
    3409,  3793,  3537,  3793,  1617,  1361,  1489,  -695,  -695,  -695,
    -695,  -695,  -695,  4513,    58,   338,   319,   242,  -695,  -695,
    -695,    54,  -695,  -695,  -695,  -695,   619,  -695,  3665,  -695,
     350,  -695,  -695,  -695,  -695,  -695,   345,   371,   349,  -695,
    -695,  -695,  -695,   337,  -695,   194,  -695,  -695,   357,   195,
      89,  -695,   360,   364,  -695,  -695,  -695,  -695,  -695,   369,
    -695,   370,   372,   374,  -695,  -695,  -695,  4947,  4993,  -695,
    -695,  -695,  -695,  -695,  -695,   460,  -695,    38,  1361,   463,
     521,  -695,  -695,  2897,  4207,  2129,  2129,  -695,  2641,   402,
     376,   100,  -695,  -695,   410,   416,   418,   419,  4207,  1101,
    1101,  -695,   486,  -695,  -695,  -695,   363,   365,  -695,  -695,
    -695,  -695,  -695,  5455,  4105,  3901,  4003,  5051,  -695,  -695,
    -695,  -695,  -695,  4847,  -695,   181,   421,   426,  4513,  -695,
    -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,
     159,  5501,   375,   387,  -695,  -695,  -695,   431,   151,   162,
    5401,   439,  -695,    27,  -695,  -695,     6,  5154,  -695,   443,
     241,    -9,   415,   450,  2257,  3793,  -695,  -695,  3281,  -695,
    3665,   212,  -695,  -695,  4615,  -695,  -695,  -695,   521,    25,
     427,   420,  -695,  2897,  -695,  -695,  -695,  -695,  -695,  -695,
    -695,  3537,  -695,  -695,   -16,    66,   372,   425,   428,   432,
     114,  -695,   184,   198,   213,   455,   452,  -695,   276,  4207,
    5154,  5154,  -695,   567,   320,   471,   417,  4513,  4207,  4615,
     212,  -695,  3153,  -695,  -695,  -695,  -695,  -695,  4847,  -695,
    5097,  5455,  3793,  -695,   436,   440,   432,  -695,  -695,  -695,
    -695,  -695,  -695,  -695,  -695,   444,   429,  -695,  -695,   453,
      60,  -695,   433,   479,   487,   290,  4207,  2897,  -695,  -695,
    -695,   474,  -695,   458,   454,   281,   196,  3793,   484,   489,
     106,  -695,  -695,    50,   485,   462,  -695,    59,  -695,   556,
    -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,   555,   111,
    -695,  -695,   676,    74,  2897,  -695,  -695,    -2,   488,   470,
    -695,  -695,  -695,   481,   478,   436,  -695,    88,   473,   440,
     138,  -695,   507,   214,   483,   215,   491,   492,  -695,  -695,
    4207,  4207,  -695,   494,   493,   468,   475,  2897,   496,  2769,
    2769,  5501,   344,  -695,  5501,  4207,   497,  5501,  -695,   490,
     513,   543,  -695,  -695,   544,   366,   545,  1873,  1233,  -695,
    -695,  2897,  -695,  2897,  2641,  -695,    30,  -695,   508,   509,
     510,  2897,  2897,  2385,  1745,  -695,  1745,   435,  -695,  1745,
    -695,  1745,   512,  -695,  -695,  -695,  -695,  -695,  -695,  -695,
     601,  4513,   551,   549,   552,  5347,   523,  -695,  -695,  -695,
    4309,     4,   226,  -695,  -695,   282,  -695,   520,  -695,  -695,
    -695,  -695,   538,  -695,   525,   562,    40,  -695,  -695,  -695,
    -695,  4993,  -695,  -695,  -695,   103,  -695,  -695,  -695,  -695,
    -695,  5547,  4207,  -695,  4207,   486,  -695,  -695,  2897,  -695,
    2129,  -695,  2897,  2641,  -695,  2897,   295,  -695,  -695,  1101,
    -695,  -695,  4207,  5455,  -695,  5455,  -695,  -695,  4207,  -695,
    4207,  -695,  4207,  -695,  -695,  -695,  -695,  -695,  -695,  -695,
    -695,  -695,  -695,   524,   528,  -695,  -695,  3793,  -695,  -695,
     623,   560,  5501,  -695,  -695,  -695,   536,  -695,   557,  -695,
    -695,  -695,   564,   700,   216,  -695,  -695,  -695,  -695,  2513,
     570,   565,  -695,   318,    60,  -695,  -695,   647,   598,   281,
    -695,  -695,  -695,  -695,  -695,  -695,  3025,   566,  -695,  -695,
     603,  -695,  -695,  -695,  -695,   569,  -695,  5651,   306,  -695,
    -695,  -695,  4207,  4207,   567,  4207,  -695,   604,  -695,   607,
     660,  -695,   684,  -695,  -695,  5097,  1745,  4207,   579,   690,
    4207,  5708,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,
     376,  1101,  1101,  -695,  -695,  -695,  -695,  -695,  -695,   587,
     588,   507,  -695,  -695,  -695,  -695,   307,  -695,  -695,  -695,
    -695,  -695,  -695,  -695,  -695,  -695,  2769,  2897,  -695,   647,
     402,  -695,  -695,  2897,  -695,   289,   650,  2385,  2897,  -695,
    -695,  -695,   969,   969,  -695,  -695,     3,    10,  -695,  -695,
    -695,  -695,  -695,   617,  -695,  4847,   183,  -695,   684,  -695,
    -695,  -695,  -695,  -695,   103,    51,  -695,   622,   696,  -695,
     154,  -695,    81,  -695,  -695,  1101,  1101,  -695,  -695,  -695,
    -695,  2897,  2897,  2897,  -695,  -695,  -695,  -695,  5708,  2897,
    -695,   164,  -695,    99,  -695,  4207,  -695,  5581,   660,   625,
    4661,  -695,  -695,  -695,  -695,  -695,  -695,  4411,   109,   662,
    -695,  -695,  -695,  -695,   610,  4947,  -695,  -695,  4207,  2897,
    -695,  1101,  -695,   187,    74,   670,  -695,  -695,   812,  -695,
     969,  -695,  -695,  -695,  -695,  4847,  -695,  4847,  -695,  -695,
     605,   612,  -695,  4513,  -695,  4947,   613,   614,  -695,  -695,
    -695,  -695,  2897,  4207,  -695,  4754,  -695,  4847,  4513,  -695,
    -695,   615,  -695,  -695,  -695,  -695,  -695,  -695
  };

  const unsigned short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   506,   507,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    68,   505,   504,    12,   148,   144,     0,     0,     0,
       0,    46,    41,    15,    14,   147,     0,     6,     7,   472,
     474,     0,   473,     0,   460,   475,   476,   477,   458,   459,
     457,   461,   462,   478,   479,   480,   481,   482,   483,     0,
     484,   485,   486,   487,   489,   488,     0,   455,   418,   454,
     416,     0,    19,    21,    25,    33,    36,   408,   417,    35,
     450,   453,   456,     0,     0,    48,    38,    42,   308,     0,
      95,     0,     0,     0,    61,    62,    63,    90,     0,    96,
       0,     0,     0,     0,     0,     0,   263,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   495,   494,   500,
     501,   502,   503,     0,   263,   263,    59,    66,    69,    70,
      71,   103,   245,   255,    73,   242,    74,   271,   275,   285,
     297,   299,   301,   378,   391,   379,     0,   300,   453,   380,
     493,   302,   145,     0,    23,     0,    34,   405,     0,     0,
       0,    24,     0,     0,   469,   490,   492,   491,   470,     0,
     467,     0,     0,     0,   468,   471,    17,     0,    27,    22,
      40,    40,     3,    45,    47,    52,    37,     0,     0,     0,
     268,   276,   269,     0,   201,   365,   365,   294,     0,     0,
     279,     0,   292,   348,     0,     0,     0,     0,     0,   138,
     138,   141,     0,   446,   447,   445,     0,     0,   424,   163,
     425,   162,   186,   227,     0,     0,     0,     0,   443,   423,
     422,   420,   419,     0,   159,   160,     0,   172,   175,   178,
     180,   184,   400,   181,   413,   421,   185,   182,   441,   444,
       0,     0,     0,     0,   448,   428,   295,     0,     0,     0,
     110,     0,   384,     0,   382,   284,     0,     0,   264,     0,
       0,     0,   385,   151,     0,     0,   356,   359,     0,   287,
     273,     0,   499,   392,     0,   498,   497,   309,   268,   314,
       0,   315,   434,     0,   435,   433,   439,   466,   465,   395,
     496,   469,   387,   509,     0,     0,   466,     0,   465,   395,
       0,   389,     0,     0,     0,   210,     0,   100,   172,     0,
       0,     0,    60,     0,    67,   103,     0,     0,     0,     0,
       0,   431,     0,   432,   430,   437,   464,   463,     0,   282,
     372,     0,     0,   146,     0,     0,     0,   411,   412,   410,
     409,   452,   451,    20,    32,     0,    28,    30,    31,     0,
       0,    51,    50,     0,     0,     0,     0,     0,   277,   207,
     202,     0,   168,     0,   200,     0,     0,     0,   369,     0,
       0,   364,   366,     0,     0,   331,   333,     0,   278,     0,
     347,   350,    87,    86,    88,    89,   197,   153,   134,     0,
     246,   137,   149,     0,     0,   164,   165,     0,     0,   228,
     230,   156,   404,     0,     0,   163,   189,   202,     0,   413,
       0,   191,   202,     0,     0,     0,     0,     0,   187,   161,
       0,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   110,     0,     0,     0,   393,     0,
       0,     0,   274,   258,     0,     0,     0,     0,     0,   290,
     357,     0,   358,     0,     0,   243,   143,   250,     0,     0,
       0,   310,   316,     0,     0,   307,     0,   311,   303,     0,
     304,     0,   451,   381,   388,   508,   305,   306,   390,   215,
     125,     0,     0,     0,     0,     0,   254,   427,    65,   426,
      97,     0,    97,   150,   252,   169,   154,     0,   244,   272,
     283,   375,     0,   371,   374,   377,     0,   286,   407,   406,
      26,     0,     9,    10,    49,     0,   281,   280,   293,   267,
     270,     0,     0,    72,     0,   370,   367,   355,     0,   360,
     363,   361,     0,     0,   349,     0,     0,    83,   139,   136,
     140,   289,     0,     0,   188,     0,   194,   403,     0,   195,
       0,   401,     0,   192,   193,   402,   414,   442,   169,    80,
     173,   449,   429,    78,    76,   296,   383,     0,   352,   104,
     105,     0,   110,   386,   111,   115,     0,   108,     0,   265,
     257,   259,     0,     0,     0,   152,   397,   256,   336,     0,
       0,   338,   342,     0,     0,   337,   288,   143,     0,     0,
     248,   249,   436,   440,   396,   318,     0,   320,   325,   326,
     309,   322,   321,   313,   312,   211,   213,     0,     0,    79,
      99,   262,     0,     0,     0,     0,    85,     0,   102,     0,
     224,    82,   232,   438,   298,     0,     0,     0,     0,    54,
       0,     0,   206,   208,   167,   203,   368,   362,   351,   332,
     279,   130,   130,   133,   135,   231,   155,   229,   217,     0,
     203,   204,   205,    77,    75,   353,     0,   106,   109,   112,
     394,   266,   398,   399,   339,   334,   341,     0,   343,   143,
     346,   335,   247,     0,   142,   483,   327,     0,   317,   215,
     215,   216,   121,   121,   124,   157,     0,     0,    64,    98,
      84,   101,   207,   218,   220,     0,     0,    81,   233,   235,
     373,   376,   253,    29,     0,    56,   166,     0,     0,   129,
       0,   126,     0,   196,   190,   138,   138,   340,   345,   344,
     251,     0,     0,     0,   323,   324,   319,   212,   214,     0,
     120,     0,   116,     0,   260,     0,   261,     0,   224,     0,
     225,   176,   183,   222,    93,    91,    92,     0,     0,   236,
     239,   415,   234,    53,     0,     0,    44,    55,     0,     0,
     131,   128,   132,     0,     0,     0,   329,   328,     0,   122,
     119,   123,   158,   223,   219,     0,   177,     0,   240,   174,
     198,     0,   237,     0,   238,     0,     0,     0,   291,   127,
     113,   114,     0,     0,   118,   225,   221,   226,     0,   241,
      94,     0,    57,   209,   330,   117,   199,    58
  };

  const short
  parser::yypgoto_[] =
  {
    -695,  -695,  -695,  -695,  -695,  -695,  -695,    37,  -695,  -695,
    -659,  -695,   554,  -695,  -695,  -695,   208,  -154,  -695,   606,
    -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,
    -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,  -695,
    -695,  -695,  -695,  -695,   230,  -290,   411,  -695,  -695,  -397,
    -695,  -695,  -695,   -50,    32,  -695,  -695,   -44,    82,  -695,
    -695,  -198,  -695,  -348,  -552,   730,  -695,  -695,  -695,  -312,
    -410,   406,   116,  -695,   516,  -695,   -53,   324,   -82,  -695,
     -91,  -695,   -90,  -383,  -695,   514,  -694,  -225,   438,   -60,
    -695,   173,   199,    52,  -695,  -695,  -695,    70,    65,  -601,
     134,  -695,    16,  -695,   -18,  -695,  -695,   225,  -695,  -695,
      63,    17,   749,  -505,   457,  -695,   323,  -695,   296,  -695,
     -88,   -86,   750,   -28,  -318,   132,  -695,   -69,   -20,  -695,
    -695,   -51,   678,  -695,  -695,  -695,   105,   339,  -695,   441,
    -406,  -695,   112,  -695,  -695,  -176,  -695,  -182,    -8,  -695,
     519,  -695,   -77,   608,   261,  -184,  -695,   160,  -695,   748,
    -695,   705,   -70,  -695,   -71,  -250,  -121,  -695,   356,   770,
    -695,  -695,  -695,   -27,  -695,  -137,  -695,   182,   714,  -100,
    -695,  -119,  -695,  -695,  -491,  -695,   590,   -79,   -29,  -207,
     -31,  -695,  -695,   -62,   -55,   -57,    15,  -695,  -195,   -59,
     -48,  -237,  -695,  -170,   -34,   -96
  };

  const short
  parser::yydefgoto_[] =
  {
      -1,     3,     4,     5,    16,   182,     6,    10,    19,    30,
      71,    72,    73,   179,   355,   356,   357,    74,    75,    87,
      11,    20,    21,    32,    85,   185,   525,   362,   725,   776,
     777,   323,   126,   496,    33,    34,   127,   128,   129,   130,
     208,   768,   804,   131,   636,   315,   327,   132,   259,   443,
     581,   677,   133,   750,   751,   704,   629,   729,   730,   663,
     547,   399,   211,   212,   610,    27,    36,   330,   456,   396,
     504,   407,   706,   233,   234,   235,   397,   506,   371,   759,
     372,   800,   318,   760,   238,   239,   761,   240,   398,   801,
     373,   374,   424,   531,   652,   490,   625,   626,   627,   669,
     642,   713,   714,   715,   763,   408,   409,   410,   717,   718,
     719,   769,   400,   401,   465,   466,   467,   135,   267,   268,
     287,   190,   402,   191,   192,   389,   193,   138,   139,   140,
     141,   304,   305,   290,   291,   617,   618,   384,   385,   459,
     600,   601,   602,   688,   689,   201,   202,   203,   603,   379,
     277,   278,   197,   380,   381,   382,   512,   513,   514,   142,
     143,   272,   261,   144,   145,   497,   292,   595,   241,   242,
      76,   243,   770,   157,    78,   244,   245,   498,   499,   367,
     293,   294,   334,   295,   246,   247,   248,   146,   147,    80,
      81,   335,   296,   297,   337,   174,    82,   175,   149,   150,
     299,   300,   151,    24,     9,   310
  };

  const short
  parser::yytable_[] =
  {
      79,   189,    77,   148,   172,   200,   137,   154,   428,   236,
     237,   173,   403,   447,   386,   331,   503,   333,   345,   390,
     314,   438,   253,   387,   358,   256,   448,   535,   288,   288,
     288,   391,   317,   262,   709,   585,   332,   502,   262,   273,
     653,   316,    13,   280,   664,   264,   255,   584,     1,   530,
     264,    22,   604,   306,   529,   692,   609,   298,   308,   298,
     307,    22,   774,   289,   314,   312,   796,   309,   453,   331,
     254,   333,   325,   263,   336,    22,   319,   469,   552,   271,
     754,   638,    22,   257,   155,   265,   448,   756,   419,   470,
     276,   279,   478,   281,   449,    68,    14,    15,   479,    70,
      22,    22,   288,   472,    12,   368,   282,   378,   378,   473,
     378,   346,   553,   510,   464,   249,   806,   755,   339,   454,
     647,   796,   507,   796,   755,   326,   306,     2,   336,   420,
     425,   308,   639,   173,   470,   450,   701,   739,   249,   474,
     309,   370,   446,   198,   411,    25,   821,   285,    79,    79,
      77,    77,    23,   530,   553,   363,   731,   731,   775,   544,
     727,   266,    23,   540,    17,   152,   364,   331,   558,   333,
      26,   413,   417,   422,   480,   153,    23,   807,   148,   148,
     481,   137,   137,    23,   198,   679,   189,   549,   471,   180,
     523,   181,   254,   684,   781,   433,   559,   752,   752,   311,
     426,    23,    23,   303,   764,   390,   539,   477,   172,   249,
     637,   548,   790,   541,    18,   173,   767,   359,   360,   540,
     434,     7,   484,   249,   549,     8,   336,    68,   485,   444,
     282,    70,   280,   550,   765,   766,   317,   722,   451,   249,
     249,   249,   249,   439,    29,   316,   561,   440,   249,   164,
     165,   166,   485,   249,   780,   460,   167,   701,   276,   302,
     339,   694,   411,    31,   789,   303,   653,   781,   255,   219,
      35,   285,   221,   218,   282,   505,   731,   790,   168,    37,
     386,   331,   344,   333,   220,   752,    38,   810,   605,   619,
     767,   493,   494,   463,   486,   195,   464,   196,   479,   536,
     549,    68,   332,   302,   509,    70,   635,   640,   487,   303,
     254,   515,   481,   229,   230,   285,   551,   231,   232,   286,
     705,   705,   517,   488,   563,   565,   683,   485,   534,   485,
     303,   164,   165,   166,   249,   158,   741,   742,   167,   159,
     336,   160,   249,   249,   204,   205,   206,   207,    83,   575,
     117,   578,   578,   249,   118,    86,   331,   537,   333,   659,
     168,    84,   431,  -153,   170,  -170,  -153,   358,   176,   578,
     578,   262,   583,   606,   178,   607,   378,   568,   570,   177,
     209,   249,   210,   264,   615,   378,   620,   586,   288,   457,
     288,   458,   568,   288,   661,   288,   662,   448,   418,   423,
     630,   237,   198,   825,   687,   702,   735,   703,   736,   317,
     582,   183,   815,   444,   817,   336,   587,   298,   316,   298,
     320,   321,   298,   621,   298,   622,   162,   690,   623,   184,
     624,   579,   580,   186,   691,   163,   586,   164,   165,   166,
     194,   266,   269,   792,   167,   249,   249,   158,   322,   340,
     656,   258,   378,   160,   658,   378,   345,   660,   342,   341,
     249,   283,   117,  -448,   434,   343,   168,   169,   347,   592,
     170,   171,   348,   593,   666,   594,   411,   349,   350,   654,
     351,   655,   352,   361,    68,   365,   198,   392,    70,   388,
     762,   649,    79,   393,    77,   394,   395,   420,   425,   665,
     404,   405,   430,   406,   435,   668,   249,   670,   390,   671,
     797,   578,   431,   745,   282,   249,   436,   437,   148,   445,
     452,   137,   254,   448,   254,   164,   165,   166,   696,   442,
     455,   475,   167,   482,   476,   762,  -468,   783,   784,   489,
     483,   491,   326,   521,   518,   501,   249,   249,   519,   249,
     329,   444,   520,   522,   168,   285,   526,   675,   170,   286,
     288,   524,   782,   532,   527,   797,   533,   249,   534,  -354,
     762,   542,   762,   249,   538,   249,   543,   249,   448,   255,
     545,   546,   668,   791,   555,   556,   557,   560,   554,   298,
     762,   562,   762,   564,   568,   721,   573,   726,   578,   738,
     282,   366,   571,   574,   576,   740,   566,   567,   572,   378,
     746,   164,   165,   166,   811,   117,   515,   589,   167,   588,
     590,   591,   597,   612,   613,   614,  -449,   628,   631,   632,
     148,   148,   633,   137,   137,   643,   329,   634,   644,   645,
     168,   285,   249,   646,   170,   286,   282,   249,   249,   676,
     249,   680,   673,   785,   786,   787,   674,   164,   165,   166,
     678,   681,   249,   788,   167,   249,   249,   331,   412,   333,
     685,   148,   148,   609,   137,   137,   799,   237,   686,   693,
     697,   698,   495,   699,   711,   640,   168,   285,   712,   771,
     773,   808,   716,   723,   724,   733,   734,   743,   282,   328,
    -149,   758,   778,  -149,   148,   148,   779,   137,   137,   164,
     165,   166,   820,   237,   795,   803,   167,   805,   812,   818,
     819,   822,   823,   827,   824,   668,   336,   799,   237,   648,
     249,   353,   641,   324,   329,   753,   500,   809,   168,   285,
     814,   771,   170,   286,   732,    28,    79,   516,    77,   707,
     148,   429,   432,   137,   569,   282,   328,   492,   826,   148,
     568,   672,   137,   249,   757,   748,   164,   165,   166,   747,
     249,   710,   249,   167,   794,   249,    79,   816,    77,   218,
     667,   772,   249,   134,   136,   802,   414,   508,   344,   611,
     220,   329,   728,   249,   313,   168,   285,   462,   737,   170,
     286,   657,   744,   608,   383,   720,   528,   161,   682,   260,
     249,   596,   249,   156,   303,   252,   708,   427,   249,   229,
     230,     0,     0,   231,   232,     0,     0,     0,   249,     0,
     249,     0,   249,   249,    88,    39,    89,    90,    91,     0,
      92,     0,    40,    93,     0,     0,    94,    95,    96,    97,
      98,     0,    99,     0,    42,     0,   100,     0,   101,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
     104,   105,    60,    61,    62,    63,    64,    65,   106,     0,
       0,   282,   813,   107,   108,     0,     0,     0,     0,     0,
       0,     0,   164,   165,   166,     0,     0,   109,     0,   167,
       0,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   112,   113,     0,     0,     0,     0,   329,     0,     0,
       0,   168,   285,     0,   114,   170,   286,     0,   115,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,     0,     0,     0,     0,     0,     0,   123,     0,
     124,   125,    88,    39,    89,     0,   749,     0,    92,     0,
      40,    93,     0,     0,    94,    95,    96,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,   104,   105,
      60,    61,    62,    63,    64,    65,   106,     0,     0,     0,
       0,   107,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,   112,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   115,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,     0,     0,   119,   120,   121,   122,
       0,     0,     0,     0,    88,    39,    89,     0,   124,   125,
      92,     0,    40,    93,     0,     0,    94,    95,    96,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
     104,   105,    60,    61,    62,    63,    64,    65,   106,     0,
       0,     0,     0,   107,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   112,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   115,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,     0,     0,    22,     0,    88,    39,    89,     0,
     124,   125,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,   105,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   107,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,   577,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    23,   114,     0,     0,     0,
     188,     0,   116,     0,     0,     0,   599,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   107,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     282,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   301,   165,   166,     0,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   188,   302,
     116,     0,     0,     0,     0,   303,   284,     0,    67,   117,
     168,   285,    69,   118,   170,   286,     0,     0,   119,   120,
     121,   122,    88,    39,    89,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,     0,   105,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   107,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   282,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,   112,
     165,   166,     0,     0,     0,     0,   167,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   188,     0,   116,   311,
       0,     0,     0,   303,   284,     0,    67,   117,   168,   285,
      69,   118,   170,   286,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   107,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   282,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,   112,   165,   166,
       0,     0,     0,     0,   167,     0,     0,     0,     0,     0,
     114,   283,     0,     0,   188,     0,   116,     0,     0,     0,
       0,     0,   284,     0,    67,   117,   168,   285,    69,   118,
     170,   286,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,     0,   105,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   107,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   282,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,   112,   165,   166,     0,     0,
       0,     0,   167,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   188,     0,   116,     0,     0,     0,     0,     0,
     284,     0,    67,   117,   168,   285,    69,   118,   170,   286,
       0,     0,   119,   120,   121,   122,    88,    39,    89,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,   105,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   107,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,   577,     0,     0,     0,     0,     0,
       0,     0,     0,   598,     0,     0,   114,     0,     0,     0,
     188,     0,   116,     0,     0,     0,   599,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   107,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,   198,     0,     0,     0,   111,
       0,   112,     0,     0,     0,     0,     0,     0,     0,     0,
     199,     0,     0,     0,   114,     0,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,    88,    39,    89,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,   376,    58,     0,     0,     0,   105,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   107,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,   112,
     377,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   188,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   107,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,   457,     0,   458,     0,
     114,     0,     0,     0,   188,     0,   116,     0,     0,     0,
       0,     0,     0,     0,    67,   117,     0,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,   375,     0,     0,     0,    42,   616,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,     0,   105,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   107,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,   112,   377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   188,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,    88,    39,    89,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,   105,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   107,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,   577,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     188,     0,   116,     0,     0,     0,   599,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
     375,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   107,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   112,   377,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,    88,    39,    89,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,     0,   105,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   107,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,   112,
     577,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   188,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   107,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,   188,     0,   116,     0,     0,     0,
       0,     0,     0,     0,    67,   117,     0,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,   695,     0,     0,     0,   105,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   107,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   188,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,    88,    39,    89,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,   105,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     188,     0,   116,     0,     0,     0,     0,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   461,     0,   111,
       0,     0,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,    88,    39,   274,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,     0,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   188,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,   188,     0,   116,     0,     0,     0,
       0,     0,     0,     0,    67,   117,     0,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   338,     0,     0,     0,     0,   114,     0,
       0,     0,   188,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,    88,    39,    89,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     188,     0,   116,     0,     0,    39,     0,     0,     0,     0,
      67,   117,    40,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    42,     0,     0,     0,     0,   369,
       0,    45,    46,    47,   213,   214,   215,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,   217,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     218,     0,     0,     0,     0,     0,     0,   414,     0,   415,
       0,   220,   221,   222,     0,     0,     0,     0,     0,     0,
     223,     0,     0,     0,   224,     0,     0,    39,   225,   416,
     226,     0,     0,     0,    40,   303,   227,     0,   228,    68,
     229,   230,     0,    70,   231,   232,    42,     0,     0,     0,
       0,   369,     0,    45,    46,    47,   213,   214,   215,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,   217,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   218,     0,     0,     0,     0,     0,     0,     0,
       0,   219,     0,   220,   221,   222,     0,     0,     0,     0,
       0,     0,   223,     0,     0,     0,   224,     0,     0,    39,
     225,     0,   226,   421,     0,     0,    40,   303,   227,     0,
     228,    68,   229,   230,     0,    70,   231,   232,    42,     0,
       0,     0,     0,   369,     0,    45,    46,    47,   213,   214,
     215,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,   217,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   218,     0,     0,     0,     0,     0,
       0,     0,     0,   219,     0,   220,   221,   222,     0,     0,
       0,     0,     0,     0,   223,     0,     0,     0,   224,   412,
       0,    39,   225,     0,   226,     0,     0,     0,    40,     0,
     227,     0,   228,    68,   229,   230,     0,    70,   231,   232,
      42,     0,     0,     0,     0,   369,     0,    45,    46,    47,
     213,   214,   215,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   216,   217,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   218,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,   220,   221,   222,
       0,     0,     0,     0,     0,     0,   223,     0,     0,     0,
     224,     0,     0,    39,   225,     0,   226,     0,     0,     0,
      40,     0,   227,     0,   228,    68,   229,   230,     0,    70,
     231,   232,    42,     0,     0,     0,     0,     0,     0,    45,
      46,    47,   213,   214,   215,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,   217,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   218,   635,
       0,     0,     0,     0,     0,     0,     0,   219,     0,   220,
     221,   222,     0,     0,     0,     0,     0,     0,   223,     0,
       0,     0,   224,     0,     0,    39,   225,     0,   226,     0,
       0,     0,    40,     0,   227,     0,   228,    68,   229,   230,
       0,    70,   231,   232,    42,     0,     0,     0,     0,     0,
       0,    45,    46,    47,   213,   214,   215,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,   217,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     218,     0,     0,     0,     0,     0,     0,     0,     0,   219,
       0,   220,   221,   222,     0,     0,     0,     0,     0,     0,
     223,     0,     0,     0,   224,     0,     0,    39,   225,   798,
     226,     0,     0,     0,    40,     0,   227,     0,   228,    68,
     229,   230,     0,    70,   231,   232,    42,     0,     0,     0,
       0,     0,     0,    45,    46,    47,   213,   214,   215,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,   217,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   218,     0,     0,     0,     0,     0,     0,     0,
       0,   219,     0,   220,   221,   222,     0,     0,     0,     0,
       0,     0,   223,     0,     0,     0,   224,     0,   468,    39,
     225,     0,   226,     0,     0,     0,    40,     0,   227,     0,
     228,    68,   229,   230,     0,    70,   231,   232,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    39,    60,    61,    62,    63,
      64,    65,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    42,     0,     0,     0,     0,     0,
       0,    45,    46,    47,   213,   214,   215,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,   217,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
     282,     0,     0,     0,     0,     0,     0,     0,     0,   219,
    -171,     0,   221,   222,     0,     0,     0,     0,    39,     0,
     223,     0,     0,     0,   224,    40,     0,     0,   225,     0,
     226,     0,     0,     0,     0,     0,   446,    42,   228,    68,
       0,   285,     0,    70,    45,    46,    47,   213,   214,   215,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,   217,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   282,     0,     0,     0,     0,     0,     0,
       0,     0,   219,     0,     0,   221,   222,     0,     0,     0,
       0,    39,     0,   223,     0,     0,     0,   224,    40,     0,
       0,   225,     0,   226,     0,     0,     0,     0,     0,   446,
      42,   228,    68,     0,   285,     0,    70,    45,    46,    47,
     213,   214,   215,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   216,   217,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,     0,   221,   222,
       0,     0,     0,     0,     0,     0,   223,     0,     0,     0,
     224,    39,     0,     0,   225,     0,   226,     0,    40,     0,
       0,     0,     0,     0,   228,    68,     0,    41,     0,    70,
      42,     0,    43,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,    59,    39,    60,    61,
      62,    63,    64,    65,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    42,     0,    43,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,     0,
      51,    52,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,    66,    39,     0,     0,     0,     0,
       0,     0,    40,     0,    67,    68,     0,     0,    69,    70,
       0,   354,     0,     0,    42,     0,     0,     0,     0,     0,
       0,    45,    46,    47,   213,   214,   215,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
      66,    39,    60,    61,    62,    63,    64,    65,    40,     0,
      67,    68,     0,     0,    69,    70,     0,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,    39,     0,
       0,     0,     0,     0,     0,    40,     0,     0,   228,    68,
       0,     0,     0,    70,     0,   511,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,   270,    60,    61,    62,    63,    64,
      65,     0,     0,     0,    67,    39,     0,     0,    69,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,   270,    60,    61,    62,    63,    64,    65,     0,     0,
       0,    67,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   164,   165,   166,     0,    39,     0,     0,   167,     0,
       0,     0,    40,     0,     0,     0,     0,     0,   250,     0,
       0,     0,     0,     0,    42,     0,   251,     0,    67,    44,
     168,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    39,    60,    61,    62,    63,    64,    65,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,   158,    39,     0,     0,   258,     0,
     160,     0,    40,     0,     0,     0,     0,     0,    67,   117,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,    39,
       0,     0,     0,     0,    67,   117,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,  -385,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
     441,    58,     0,     0,     0,    39,    60,    61,    62,    63,
      64,    65,    40,     0,     0,   442,     0,     0,    67,     0,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,    39,    60,    61,    62,    63,    64,    65,    40,     0,
       0,     0,   250,     0,     0,     0,     0,     0,     0,     0,
      42,     0,    67,     0,     0,     0,     0,    45,    46,    47,
     213,   214,   215,     0,     0,    39,    53,    54,    55,    56,
      57,     0,    40,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,    42,     0,     0,     0,     0,     0,
       0,    45,    46,    47,   213,   214,   215,     0,    67,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,   650,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   651,    39,     0,     0,     0,     0,
       0,     0,    40,     0,   228,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    42,     0,     0,     0,   793,     0,
       0,    45,    46,    47,   213,   214,   215,     0,   651,     0,
      53,    54,    55,    56,    57,     0,     0,    58,   228,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,     0,     0,     0,     0,     0,   700,    45,    46,
      47,   213,   214,   215,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,   228,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   228
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,    34,    66,    93,    34,    41,   233,   100,
     100,    66,   210,   263,   198,   136,   328,   136,   155,   201,
     116,   258,   101,   199,   178,   102,   263,   375,   114,   115,
     116,   201,   123,   104,   635,   445,   136,   327,   109,   109,
     531,   123,     5,   112,   549,   104,   101,   444,    20,   367,
     109,     1,   458,   115,   366,   607,    26,   114,   115,   116,
     115,     1,    11,   114,   160,   116,   760,   115,    77,   190,
     101,   190,    18,   104,   136,     1,    18,   284,    80,   108,
      77,    77,     1,   103,   107,   105,   323,    77,   225,   284,
     110,   111,   108,   113,    88,   118,    65,    66,   114,   122,
       1,     1,   188,    78,     0,   193,    79,   195,   196,    84,
     198,   159,   114,   338,    84,   100,   775,   114,   138,   128,
      80,   815,   329,   817,   114,    71,   188,    99,   190,   225,
     226,   188,   128,   188,   329,   129,   627,   689,   123,   114,
     188,   194,   115,    84,   223,   103,   805,   120,   177,   178,
     177,   178,   102,   471,   114,   117,   661,   662,   107,   100,
     651,   103,   102,   113,   100,   104,   128,   288,    80,   288,
     128,   224,   225,   226,   108,   114,   102,   778,   209,   210,
     114,   209,   210,   102,    84,   582,   274,   113,   288,    99,
     360,   101,   223,   599,   113,   250,   108,   702,   703,   110,
     227,   102,   102,   114,    21,   387,   100,   293,   270,   194,
     500,   100,   113,   383,   113,   270,   107,   180,   181,   113,
     251,   118,   108,   208,   113,   122,   288,   118,   114,   260,
      79,   122,   301,   403,    51,    52,   327,   647,   267,   224,
     225,   226,   227,    81,   107,   327,   108,    85,   233,    90,
      91,    92,   114,   238,   100,   275,    97,   748,   278,   108,
     280,   609,   341,    13,   100,   114,   757,   113,   323,    88,
     128,   120,    91,    79,    79,   328,   781,   113,   119,    77,
     464,   402,    88,   402,    90,   790,    77,   100,   458,   473,
     107,   320,   321,    81,   110,    99,    84,   101,   114,   376,
     113,   118,   402,   108,   332,   122,    80,    81,   110,   114,
     341,   340,   114,   119,   120,   120,   404,   123,   124,   124,
     632,   633,   342,   110,   110,   110,   110,   114,   114,   114,
     114,    90,    91,    92,   319,   103,    47,    48,    97,   107,
     402,   109,   327,   328,    72,    73,    74,    75,    26,   437,
     118,   439,   440,   338,   122,   113,   477,   377,   477,   543,
     119,    60,    86,    81,   123,    89,    84,   521,   108,   457,
     458,   442,   442,   461,   107,   463,   464,   430,   431,   114,
      99,   366,   101,   442,   472,   473,   474,   446,   474,    99,
     476,   101,   445,   479,    99,   481,   101,   634,   225,   226,
     491,   491,    84,   813,    86,    99,    99,   101,   101,   500,
     441,    77,   795,   444,   797,   477,   447,   474,   500,   476,
     124,   125,   479,   474,   481,   476,    79,   603,   479,    33,
     481,   439,   440,   113,   604,    88,   495,    90,    91,    92,
     107,   103,   128,   755,    97,   430,   431,   103,   129,    99,
     538,   107,   540,   109,   542,   543,   593,   545,    87,   114,
     445,   104,   118,   114,   495,   128,   119,   120,   108,   103,
     123,   124,   108,   107,   553,   109,   555,   108,   108,   532,
     108,   534,   108,    23,   118,    22,    84,    77,   122,   113,
     715,   525,   521,    77,   521,    77,    77,   593,   594,   552,
      14,   138,    81,   138,   129,   558,   491,   560,   690,   562,
     760,   599,    86,   697,    79,   500,   129,    86,   549,    80,
      77,   549,   553,   760,   555,    90,    91,    92,   616,   114,
      80,   104,    97,   108,   114,   760,   108,   735,   736,    84,
     108,    89,    71,   114,   108,   128,   531,   532,   108,   534,
     115,   582,   108,   100,   119,   120,    77,   577,   123,   124,
     646,   128,   732,    89,    77,   815,   108,   552,   114,    85,
     795,    86,   797,   558,    85,   560,   114,   562,   815,   634,
      24,    26,   635,   753,   114,   104,   108,   114,   100,   646,
     815,    84,   817,   110,   647,   646,   128,   650,   686,   687,
      79,    80,   108,   128,   108,   693,   115,   115,   115,   697,
     698,    90,    91,    92,   784,   118,   645,   104,    97,   129,
      77,    77,    77,   115,   115,   115,   114,    26,    77,    80,
     661,   662,    80,   661,   662,   115,   115,   114,   100,   114,
     119,   120,   627,    81,   123,   124,    79,   632,   633,    26,
     635,   115,   128,   741,   742,   743,   128,    90,    91,    92,
     100,   104,   647,   749,    97,   650,   651,   788,   104,   788,
     100,   702,   703,    26,   702,   703,   767,   767,   113,    81,
     114,    78,   115,   114,    77,    81,   119,   120,    28,   716,
     724,   779,     8,   114,     4,   108,   108,    47,    79,    80,
      81,    84,    80,    84,   735,   736,    10,   735,   736,    90,
      91,    92,   803,   803,    89,    53,    97,   107,    48,   114,
     108,   108,   108,   108,   812,   778,   788,   818,   818,   521,
     715,   177,   502,   127,   115,   703,   325,   781,   119,   120,
     790,   768,   123,   124,   662,    15,   775,   341,   775,   633,
     781,   235,   238,   781,   430,    79,    80,   319,   818,   790,
     813,   562,   790,   748,   712,   700,    90,    91,    92,   699,
     755,   637,   757,    97,   758,   760,   805,   795,   805,    79,
     555,   718,   767,    34,    34,   768,    86,   330,    88,   466,
      90,   115,   660,   778,   116,   119,   120,   278,   686,   123,
     124,   540,   697,   464,   196,   645,   365,    59,   108,   104,
     795,   455,   797,    43,   114,   101,   634,   227,   803,   119,
     120,    -1,    -1,   123,   124,    -1,    -1,    -1,   813,    -1,
     815,    -1,   817,   818,     3,     4,     5,     6,     7,    -1,
       9,    -1,    11,    12,    -1,    -1,    15,    16,    17,    18,
      19,    -1,    21,    -1,    23,    -1,    25,    -1,    27,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    79,    80,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,    -1,    -1,    76,    -1,    97,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,   120,    -1,   103,   123,   124,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,    -1,    -1,    -1,    -1,   137,    -1,
     139,   140,     3,     4,     5,    -1,     7,    -1,     9,    -1,
      11,    12,    -1,    -1,    15,    16,    17,    -1,    19,    -1,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    -1,    -1,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
      -1,    -1,    -1,    -1,     3,     4,     5,    -1,   139,   140,
       9,    -1,    11,    12,    -1,    -1,    15,    16,    17,    -1,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,    -1,    -1,     1,    -1,     3,     4,     5,    -1,
     139,   140,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,     3,     4,     5,    -1,    -1,    -1,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
     109,    -1,    -1,    -1,    -1,   114,   115,    -1,   117,   118,
     119,   120,   121,   122,   123,   124,    -1,    -1,   127,   128,
     129,   130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    -1,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,   110,
      -1,    -1,    -1,   114,   115,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
     103,   104,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,   115,    -1,   117,   118,   119,   120,   121,   122,
     123,   124,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
     115,    -1,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,     3,     4,     5,    -1,    -1,    -1,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    84,    -1,    -1,    -1,    88,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    24,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,     3,     4,     5,    -1,    -1,    -1,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    -1,    -1,    -1,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,     3,     4,     5,    -1,    -1,    -1,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    86,    -1,    88,
      -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,     4,    -1,    -1,    -1,    -1,
     117,   118,    11,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,   108,
     109,    -1,    -1,    -1,    11,   114,   115,    -1,   117,   118,
     119,   120,    -1,   122,   123,   124,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,    -1,    -1,     4,
     107,    -1,   109,   110,    -1,    -1,    11,   114,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,   104,
      -1,     4,   107,    -1,   109,    -1,    -1,    -1,    11,    -1,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,    -1,     4,   107,    -1,   109,    -1,    -1,    -1,
      11,    -1,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    23,    -1,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      41,    42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,    -1,    -1,     4,   107,    -1,   109,    -1,
      -1,    -1,    11,    -1,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    23,    -1,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,   108,
     109,    -1,    -1,    -1,    11,    -1,   115,    -1,   117,   118,
     119,   120,    -1,   122,   123,   124,    23,    -1,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,    -1,     3,     4,
     107,    -1,   109,    -1,    -1,    -1,    11,    -1,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,     4,    51,    52,    53,    54,
      55,    56,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      89,    -1,    91,    92,    -1,    -1,    -1,    -1,     4,    -1,
      99,    -1,    -1,    -1,   103,    11,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,   115,    23,   117,   118,
      -1,   120,    -1,   122,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,    -1,
      -1,     4,    -1,    99,    -1,    -1,    -1,   103,    11,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,   115,
      23,   117,   118,    -1,   120,    -1,   122,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,     4,    -1,    -1,   107,    -1,   109,    -1,    11,    -1,
      -1,    -1,    -1,    -1,   117,   118,    -1,    20,    -1,   122,
      23,    -1,    25,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    49,     4,    51,    52,
      53,    54,    55,    56,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    25,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    -1,
      37,    38,    39,    40,    41,    42,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,   107,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    11,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    78,    -1,    -1,    23,    -1,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
     107,     4,    51,    52,    53,    54,    55,    56,    11,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,   117,   118,
      -1,    -1,    -1,   122,    -1,    78,    -1,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    41,    42,    43,    -1,    -1,
      46,    -1,    -1,    -1,   107,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,   117,     4,    -1,    -1,   121,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,   107,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,   117,    -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    91,    92,    -1,     4,    -1,    -1,    97,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,   107,    -1,
      -1,    -1,    -1,    -1,    23,    -1,   115,    -1,   117,    28,
     119,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,     4,    51,    52,    53,    54,    55,    56,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,   103,     4,    -1,    -1,   107,    -1,
     109,    -1,    11,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,     4,
      -1,    -1,    -1,    -1,   117,   118,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    80,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      99,    46,    -1,    -1,    -1,     4,    51,    52,    53,    54,
      55,    56,    11,    -1,    -1,   114,    -1,    -1,   117,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,     4,    51,    52,    53,    54,    55,    56,    11,    -1,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,   117,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,     4,    39,    40,    41,    42,
      43,    -1,    11,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    23,    -1,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,   117,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   107,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    11,    -1,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    97,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,   107,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,   117,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    -1,    86,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117
  };

  const unsigned short
  parser::yystos_[] =
  {
       0,    20,    99,   142,   143,   144,   147,   118,   122,   345,
     148,   161,     0,   148,    65,    66,   145,   100,   113,   149,
     162,   163,     1,   102,   344,   103,   128,   206,   206,   107,
     150,    13,   164,   175,   176,   128,   207,    77,    77,     4,
      11,    20,    23,    25,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    41,    42,    43,    46,    49,
      51,    52,    53,    54,    55,    56,   107,   117,   118,   121,
     122,   151,   152,   153,   158,   159,   311,   314,   315,   329,
     330,   331,   337,    26,    60,   165,   113,   160,     3,     5,
       6,     7,     9,    12,    15,    16,    17,    18,    19,    21,
      25,    27,    36,    44,    49,    50,    57,    62,    63,    76,
      82,    88,    90,    91,   103,   107,   109,   118,   122,   127,
     128,   129,   130,   137,   139,   140,   173,   177,   178,   179,
     180,   184,   188,   193,   253,   258,   263,   264,   268,   269,
     270,   271,   300,   301,   304,   305,   328,   329,   331,   339,
     340,   343,   104,   114,   345,   107,   310,   314,   103,   107,
     109,   300,    79,    88,    90,    91,    92,    97,   119,   120,
     123,   124,   334,   335,   336,   338,   108,   114,   107,   154,
      99,   101,   146,    77,    33,   166,   113,    63,   107,   261,
     262,   264,   265,   267,   107,    99,   101,   293,    84,    99,
     261,   286,   287,   288,    72,    73,    74,    75,   181,    99,
     101,   203,   204,    33,    34,    35,    67,    68,    79,    88,
      90,    91,    92,    99,   103,   107,   109,   115,   117,   119,
     120,   123,   124,   214,   215,   216,   221,   223,   225,   226,
     228,   309,   310,   312,   316,   317,   325,   326,   327,   337,
     107,   115,   319,   328,   331,   335,   293,   269,   107,   189,
     302,   303,   305,   331,   340,   269,   103,   259,   260,   128,
     107,   329,   302,   303,     5,    91,   269,   291,   292,   269,
     268,   269,    79,   104,   115,   120,   124,   261,   262,   272,
     274,   275,   307,   321,   322,   324,   333,   334,   336,   341,
     342,    90,   108,   114,   272,   273,   334,   335,   336,   341,
     346,   110,   272,   273,   346,   186,   219,   221,   223,    18,
     259,   259,   129,   172,   160,    18,    71,   187,    80,   115,
     208,   307,   320,   322,   323,   332,   334,   335,    98,   269,
      99,   114,    87,   128,    88,   316,   341,   108,   108,   108,
     108,   108,   108,   153,    78,   155,   156,   157,   158,   148,
     148,    23,   168,   117,   128,    22,    80,   320,   261,    28,
     217,   219,   221,   231,   232,    19,    45,    91,   261,   290,
     294,   295,   296,   294,   278,   279,   296,   286,   113,   266,
     288,   344,    77,    77,    77,    77,   210,   217,   229,   202,
     253,   254,   263,   202,    14,   138,   138,   212,   246,   247,
     248,   328,   104,   217,    86,    88,   108,   217,   232,   316,
     346,   110,   217,   232,   233,   346,   314,   327,   228,   215,
      81,    86,   226,   335,   331,   129,   129,    86,   342,    81,
      85,    99,   114,   190,   331,    80,   115,   306,   342,    88,
     129,   329,    77,    77,   128,    80,   209,    99,   101,   280,
     269,    86,   291,    81,    84,   255,   256,   257,     3,   330,
     339,   320,    78,    84,   114,   104,   114,   262,   108,   114,
     108,   114,   108,   108,   108,   114,   110,   110,   110,    84,
     236,    89,   229,   329,   329,   115,   174,   306,   318,   319,
     187,   128,   186,   210,   211,   217,   218,   330,   255,   264,
     228,    78,   297,   298,   299,   329,   212,   269,   108,   108,
     108,   114,   100,   344,   128,   167,    77,    77,   280,   210,
     265,   234,    89,   108,   114,   204,   293,   269,    85,   100,
     113,   344,    86,   114,   100,    24,    26,   201,   100,   113,
     344,   261,    80,   114,   100,   114,   104,   108,    80,   108,
     114,   108,    84,   110,   110,   110,   115,   115,   217,   218,
     217,   108,   115,   128,   128,   261,   108,    91,   261,   289,
     289,   191,   331,   303,   190,   211,   340,   331,   129,   104,
      77,    77,   103,   107,   109,   308,   309,    77,   100,   113,
     281,   282,   283,   289,   281,   344,   261,   261,   278,    26,
     205,   257,   115,   115,   115,   261,    24,   276,   277,   296,
     261,   272,   272,   272,   272,   237,   238,   239,    26,   197,
     221,    77,    80,    80,   114,    80,   185,   186,    77,   128,
      81,   185,   241,   115,   100,   114,    81,    80,   157,   345,
      97,   107,   235,   325,   217,   217,   261,   295,   261,   296,
     261,    99,   101,   200,   254,   217,   328,   248,   217,   240,
     217,   217,   233,   128,   128,   269,    26,   192,   100,   190,
     115,   104,   108,   110,   281,   100,   113,    86,   284,   285,
     286,   344,   205,    81,   204,    46,   261,   114,    78,   114,
      86,   325,    99,   101,   196,   210,   213,   213,   318,   240,
     241,    77,    28,   242,   243,   244,     8,   249,   250,   251,
     298,   272,   211,   114,     4,   169,   217,   325,   266,   198,
     199,   254,   199,   108,   108,    99,   101,   283,   261,   205,
     261,    47,    48,    47,   277,   296,   261,   238,   239,     7,
     194,   195,   254,   195,    77,   114,    77,   234,    84,   220,
     224,   227,   228,   245,    21,    51,    52,   107,   182,   252,
     313,   314,   251,   345,    11,   107,   170,   171,    80,    10,
     100,   113,   344,   202,   202,   261,   261,   261,   262,   100,
     113,   344,   210,    97,   243,    89,   227,   306,   108,   221,
     222,   230,   252,    53,   183,   107,   151,   240,   261,   198,
     100,   344,    48,    80,   194,   224,   245,   224,   114,   108,
     221,   151,   108,   108,   261,   211,   230,   108
  };

  const unsigned short
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
     179,   179,   179,   180,   180,   180,   181,   181,   181,   181,
     181,   182,   182,   182,   183,   184,   184,   185,   185,   186,
     186,   187,   187,   187,   188,   188,   188,   189,   189,   189,
     190,   190,   191,   192,   192,   193,   194,   194,   195,   195,
     195,   195,   196,   196,   197,   197,   198,   199,   199,   199,
     199,   200,   200,   201,   201,   202,   202,   202,   202,   203,
     203,   204,   205,   205,   206,   206,   207,   207,   207,   208,
     208,   209,   209,   210,   211,   212,   212,   213,   213,   214,
     214,   214,   215,   215,   216,   216,   217,   217,   217,   218,
     219,   220,   221,   221,   222,   223,   224,   224,   225,   225,
     226,   226,   226,   227,   228,   228,   228,   228,   228,   228,
     228,   228,   228,   228,   228,   228,   228,   229,   230,   230,
     231,   231,   232,   232,   233,   233,   234,   234,   235,   235,
     236,   236,   237,   237,   238,   239,   239,   240,   241,   242,
     242,   243,   243,   244,   244,   245,   245,   246,   246,   247,
     247,   248,   249,   249,   250,   250,   251,   251,   251,   252,
     252,   252,   253,   253,   253,   253,   254,   255,   255,   256,
     256,   257,   258,   258,   258,   258,   258,   258,   258,   258,
     258,   258,   258,   259,   259,   260,   260,   261,   261,   262,
     262,   263,   263,   264,   264,   264,   265,   265,   266,   266,
     267,   267,   268,   268,   268,   268,   269,   269,   269,   269,
     269,   269,   269,   269,   269,   269,   269,   269,   270,   270,
     271,   271,   271,   271,   271,   271,   271,   271,   271,   272,
     272,   272,   273,   273,   274,   274,   274,   274,   274,   274,
     274,   275,   275,   276,   276,   276,   276,   277,   277,   277,
     277,   278,   279,   279,   280,   280,   280,   280,   281,   281,
     282,   282,   282,   283,   284,   285,   285,   286,   286,   287,
     287,   288,   289,   289,   290,   290,   291,   291,   292,   292,
     293,   293,   294,   294,   294,   294,   295,   295,   296,   296,
     296,   297,   297,   298,   298,   298,   299,   299,   300,   300,
     301,   301,   302,   302,   302,   303,   303,   304,   304,   304,
     304,   305,   305,   306,   306,   307,   307,   308,   308,   308,
     309,   309,   309,   309,   309,   310,   310,   310,   311,   311,
     311,   311,   311,   312,   312,   313,   314,   314,   315,   316,
     316,   316,   317,   317,   317,   317,   318,   318,   319,   319,
     320,   320,   320,   321,   321,   321,   322,   323,   323,   324,
     324,   325,   326,   327,   327,   327,   327,   327,   328,   328,
     329,   329,   329,   330,   330,   331,   331,   331,   331,   331,
     331,   331,   331,   332,   332,   333,   333,   334,   335,   335,
     336,   336,   337,   337,   337,   337,   337,   337,   337,   337,
     337,   337,   337,   337,   337,   337,   337,   337,   337,   337,
     338,   338,   338,   339,   339,   340,   341,   341,   342,   342,
     343,   343,   343,   343,   344,   344,   345,   345,   346,   346
  };

  const unsigned char
  parser::yyr2_[] =
  {
       0,     2,     1,     6,     1,     0,     3,     3,     0,     3,
       3,     3,     3,     2,     2,     2,     1,     3,     0,     1,
       3,     1,     2,     2,     2,     0,     3,     0,     1,     4,
       1,     1,     1,     1,     2,     1,     1,     2,     1,     2,
       0,     2,     3,     0,     8,     2,     0,     1,     0,     1,
       0,     1,     0,     2,     0,     1,     0,     3,     4,     0,
       1,     1,     1,     1,     3,     1,     2,     3,     0,     1,
       1,     1,     4,     1,     1,     5,     4,     5,     4,     4,
       4,     5,     4,     4,     5,     4,     2,     2,     2,     2,
       0,     1,     1,     1,     2,     1,     1,     0,     2,     3,
       1,     4,     3,     0,     4,     4,     5,     2,     3,     4,
       0,     2,     2,     4,     4,     4,     1,     4,     3,     2,
       1,     0,     3,     3,     2,     0,     1,     3,     2,     1,
       0,     3,     3,     2,     0,     3,     2,     1,     0,     3,
       3,     1,     2,     0,     1,     3,     3,     1,     0,     0,
       2,     0,     2,     1,     1,     3,     1,     1,     3,     1,
       1,     2,     1,     1,     2,     2,     4,     3,     1,     1,
       1,     1,     1,     3,     1,     1,     1,     2,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     3,     2,
       5,     2,     3,     3,     3,     3,     5,     1,     1,     3,
       1,     0,     1,     3,     3,     3,     2,     0,     1,     5,
       0,     2,     3,     1,     3,     0,     2,     1,     2,     3,
       1,     4,     2,     3,     0,     1,     3,     0,     1,     3,
       1,     3,     0,     1,     2,     1,     2,     3,     3,     1,
       2,     3,     1,     3,     3,     1,     1,     3,     2,     2,
       1,     4,     3,     5,     3,     1,     4,     4,     3,     4,
       6,     6,     4,     0,     1,     3,     4,     3,     1,     1,
       3,     1,     3,     2,     3,     1,     1,     2,     1,     0,
       3,     3,     2,     3,     2,     1,     3,     2,     4,     4,
       3,     8,     2,     4,     2,     2,     4,     1,     4,     1,
       1,     1,     1,     3,     3,     3,     3,     3,     1,     1,
       2,     2,     3,     3,     1,     1,     2,     4,     3,     5,
       3,     3,     3,     3,     3,     1,     1,     2,     4,     4,
       6,     1,     3,     1,     3,     3,     2,     2,     1,     2,
       3,     2,     1,     2,     2,     2,     1,     2,     1,     3,
       2,     4,     1,     2,     1,     2,     1,     2,     2,     1,
       3,     3,     3,     2,     1,     0,     1,     2,     3,     1,
       2,     1,     0,     3,     1,     1,     3,     1,     1,     1,
       1,     3,     1,     3,     1,     1,     3,     2,     3,     2,
       3,     1,     2,     1,     3,     1,     3,     1,     2,     2,
       1,     3,     3,     3,     2,     1,     3,     3,     1,     3,
       3,     3,     3,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     3,
       1,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "$undefined", "\"_\"", "\"as\"", "\"case\"",
  "\"data\"", "\"default\"", "\"deriving\"", "\"do\"", "\"else\"",
  "\"hiding\"", "\"if\"", "\"import\"", "\"in\"", "\"infix\"",
  "\"infixl\"", "\"infixr\"", "\"instance\"", "\"let\"", "\"module\"",
  "\"newtype\"", "\"of\"", "\"qualified\"", "\"then\"", "\"type\"",
  "\"where\"", "\"builtin\"", "\"forall\"", "\"foreign\"", "\"export\"",
  "\"label\"", "\"dynamic\"", "\"safe\"", "\"interruptible\"",
  "\"unsafe\"", "\"mdo\"", "\"family\"", "\"role\"", "\"stdcall\"",
  "\"ccall\"", "\"capi\"", "\"prim\"", "\"javascript\"", "\"proc\"",
  "\"rec\"", "\"group\"", "\"by\"", "\"using\"", "\"pattern\"",
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
  "\"class\"", "\"#-\"", "\"{-# SPECIALISE\"", "\"{-# SPECIALISE_INLINE\"",
  "$accept", "unit", "module", "missing_module_keyword", "maybemodwarning",
  "body", "body2", "top", "top1", "maybeexports", "exportlist",
  "exportlist1", "export", "export_subspec", "qcnames", "qcnames1",
  "qcname_ext_w_wildcard", "qcname_ext", "qcname", "semis1", "semis",
  "importdecls", "importdecls_semi", "importdecl", "maybe_src",
  "maybe_safe", "maybe_pkg", "optqualified", "maybeas", "maybeimpspec",
  "impspec", "prec", "infix", "ops", "topdecls", "topdecls_semi",
  "topdecl", "cl_decl", "ty_decl", "inst_decl", "overlap_pragma",
  "deriv_strategy_no_via", "deriv_strategy_via", "data_or_newtype",
  "opt_kind_sig", "tycl_hdr", "capi_ctype", "pattern_synonym_decl",
  "pattern_synonym_lhs", "vars0", "cvars1", "where_decls",
  "pattern_synonym_sig", "decl_cls", "decls_cls", "decllist_cls",
  "where_cls", "decl_inst", "decls_inst", "decllist_inst", "where_inst",
  "decls", "decllist", "binds", "wherebinds", "strings", "stringlist",
  "opt_sig", "opt_tyconsig", "sigtype", "sigtypedoc", "sig_vars",
  "sigtypes1", "strict_mark", "strictness", "unpackedness", "ctype",
  "ctypedoc", "context", "context_no_ops", "type", "typedoc", "btype",
  "btype_no_ops", "tyapps", "tyapp", "atype_docs", "atype", "inst_type",
  "deriv_types", "comma_types0", "comma_types1", "bar_types2", "tv_bndrs",
  "tv_bndr", "fds", "fds1", "fd", "varids0", "kind", "constrs", "constrs1",
  "constr", "forall", "constr_stuff", "fielddecls", "fielddecls1",
  "fielddecl", "maybe_derivings", "derivings", "deriving",
  "deriv_clause_types", "decl_no_th", "decl", "rhs", "gdrhs", "gdrh",
  "sigdecl", "activation", "explicit_activation", "exp", "infixexp",
  "infixexp_top", "exp10_top", "exp10", "optSemi", "scc_annot", "fexp",
  "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps", "squals",
  "transformqual", "guardquals", "guardquals1", "altslist", "alts",
  "alts1", "alt", "alt_rhs", "ralt", "gdpats", "ifgdpats", "gdpat", "pat",
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

#if YYDEBUG
  const unsigned short
  parser::yyrline_[] =
  {
       0,   504,   504,   521,   522,   524,   528,   529,   530,   532,
     533,   535,   536,   539,   541,   542,   543,   551,   552,   554,
     556,   557,   559,   560,   561,   563,   564,   566,   567,   569,
     570,   572,   573,   575,   576,   578,   579,   583,   584,   586,
     587,   589,   591,   592,   594,   601,   602,   604,   605,   607,
     608,   610,   611,   613,   614,   616,   617,   619,   620,   625,
     626,   628,   629,   630,   632,   633,   637,   639,   640,   642,
     643,   644,   647,   654,   655,   656,   657,   658,   659,   661,
     663,   665,   666,   669,   671,   672,   674,   675,   676,   677,
     678,   680,   681,   682,   684,   724,   725,   727,   728,   737,
     738,   740,   741,   742,   759,   760,   761,   763,   764,   765,
     767,   768,   770,   772,   773,   776,   780,   781,   783,   784,
     785,   786,   788,   789,   791,   792,   794,   796,   797,   798,
     799,   801,   802,   804,   805,   807,   808,   809,   810,   812,
     813,   815,   821,   822,   830,   831,   833,   834,   835,   843,
     844,   846,   847,   849,   851,   853,   854,   856,   857,   861,
     862,   863,   865,   866,   868,   869,   871,   872,   874,   876,
     885,   887,   889,   890,   892,   895,   897,   898,   900,   901,
     903,   904,   905,   911,   913,   914,   915,   916,   917,   918,
     919,   920,   921,   922,   923,   924,   925,   928,   930,   931,
     933,   934,   936,   937,   939,   940,   942,   943,   945,   946,
     948,   949,   951,   952,   954,   956,   957,   961,   967,   969,
     970,   972,   973,   975,   976,   978,   979,   981,   982,   984,
     985,   987,   989,   990,   992,   993,   995,   996,   997,   999,
    1000,  1001,  1006,  1007,  1009,  1010,  1013,  1016,  1017,  1019,
    1020,  1022,  1024,  1025,  1026,  1027,  1028,  1029,  1030,  1031,
    1032,  1033,  1034,  1036,  1037,  1039,  1040,  1044,  1045,  1047,
    1048,  1050,  1051,  1053,  1054,  1055,  1057,  1058,  1061,  1062,
    1064,  1065,  1069,  1070,  1071,  1072,  1074,  1075,  1076,  1077,
    1078,  1079,  1080,  1081,  1082,  1083,  1084,  1085,  1087,  1088,
    1090,  1091,  1092,  1093,  1094,  1095,  1096,  1097,  1098,  1103,
    1104,  1105,  1110,  1111,  1129,  1130,  1131,  1132,  1133,  1134,
    1135,  1137,  1138,  1150,  1151,  1152,  1153,  1155,  1156,  1157,
    1158,  1161,  1163,  1164,  1167,  1168,  1169,  1170,  1172,  1173,
    1175,  1176,  1177,  1179,  1181,  1183,  1184,  1186,  1187,  1189,
    1190,  1192,  1194,  1195,  1197,  1198,  1200,  1201,  1203,  1204,
    1207,  1208,  1210,  1211,  1212,  1213,  1218,  1219,  1221,  1222,
    1223,  1228,  1229,  1231,  1232,  1233,  1235,  1236,  1268,  1269,
    1271,  1272,  1274,  1275,  1276,  1278,  1279,  1281,  1282,  1283,
    1284,  1286,  1287,  1289,  1290,  1292,  1293,  1296,  1297,  1298,
    1300,  1301,  1302,  1303,  1304,  1306,  1307,  1308,  1310,  1311,
    1312,  1313,  1314,  1317,  1318,  1320,  1322,  1323,  1327,  1329,
    1330,  1331,  1333,  1334,  1335,  1336,  1341,  1342,  1344,  1345,
    1347,  1348,  1349,  1351,  1352,  1353,  1355,  1357,  1358,  1360,
    1361,  1365,  1367,  1369,  1370,  1371,  1372,  1373,  1376,  1377,
    1379,  1380,  1381,  1383,  1384,  1386,  1387,  1388,  1389,  1390,
    1391,  1392,  1393,  1395,  1396,  1398,  1399,  1401,  1403,  1404,
    1406,  1407,  1409,  1410,  1411,  1412,  1413,  1414,  1415,  1416,
    1417,  1418,  1419,  1420,  1421,  1422,  1423,  1424,  1425,  1426,
    1428,  1429,  1430,  1434,  1435,  1437,  1439,  1440,  1442,  1443,
    1447,  1448,  1449,  1450,  1455,  1458,  1462,  1463,  1465,  1466
  };

  // Print the state stack on the debug stream.
  void
  parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << '\n';
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  parser::yy_reduce_print_ (int yyrule)
  {
    unsigned yylno = yyrline_[yyrule];
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
#line 5775 "parser.cc" // lalr1.cc:1181
#line 1475 "parser.y" // lalr1.cc:1182


using boost::optional;
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

expression_ref make_sig_vars(const vector<expression_ref>& sig_vars)
{
    return new expression(AST_node("sig_vars"),sig_vars);
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
    vector<expression_ref> e;
    e.push_back(exp);
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return new expression(AST_node("rhs"), e);
}

expression_ref make_gdrhs(const vector<expression_ref>& gdrhs)
{
    return new expression(AST_node("gdrhs"), gdrhs);
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
    return new expression(AST_node("alts"), alts);
}

expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs)
{
    expression_ref alt = AST_node("alt") + pat + alt_rhs.sub()[0];
    if (alt_rhs.size() == 2)
	alt = alt + alt_rhs.sub()[1];
    return alt;
}

expression_ref make_alt_rhs(const expression_ref& ralt, const expression_ref& wherebinds)
{
    expression_ref alt = AST_node("altrhs") + ralt;
    if (wherebinds and wherebinds.size())
	alt = alt + wherebinds;
    return alt;
}

expression_ref make_gdpats(const vector<expression_ref>& gdpats)
{
    return new expression(AST_node("Guards"), gdpats);
}

expression_ref make_gdpat(const expression_ref& guardquals, const expression_ref& exp)
{
    return new expression(AST_node("GdPat"), {guardquals, exp});
}

expression_ref make_stmts(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Stmts"), stmts);
}

expression_ref make_infix(const string& infix, optional<int>& prec, vector<string>& op_names)
{
    vector<expression_ref> o;
    for(auto& op_name: op_names)
	o.push_back(String(op_name));
    expression_ref ops = new expression(AST_node("Ops"),o);

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
