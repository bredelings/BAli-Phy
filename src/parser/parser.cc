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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.move< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
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
      case 276: // flattenedpquals
      case 279: // transformqual
      case 285: // alt
      case 286: // alt_rhs
      case 287: // ralt
      case 289: // ifgdpats
      case 290: // gdpat
      case 291: // pat
      case 292: // bindpat
      case 293: // apat
      case 297: // stmt
      case 298: // qual
      case 345: // literal
        yylhs.value.build< expression_ref > ();
        break;

      case 135: // "PRIMFLOAT"
        yylhs.value.build< float > ();
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 348: // commas
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
      case 302: // qcon
      case 303: // gen_qcon
      case 304: // con
      case 306: // sysdcon_no_list
      case 307: // sysdcon
      case 308: // conop
      case 309: // qconop
      case 310: // gtycon
      case 311: // ntgtycon
      case 312: // oqtycon
      case 313: // oqtycon_no_varcon
      case 314: // qtyconop
      case 315: // qtycondoc
      case 316: // qtycon
      case 317: // tycon
      case 318: // qtyconsym
      case 319: // tyconsym
      case 320: // op
      case 321: // varop
      case 322: // qop
      case 323: // qopm
      case 324: // hole_op
      case 325: // qvarop
      case 326: // qvaropm
      case 327: // tyvar
      case 328: // tyvarop
      case 329: // tyvarid
      case 330: // var
      case 331: // qvar
      case 332: // qvarid
      case 333: // varid
      case 334: // qvarsym
      case 335: // qvarsym_no_minus
      case 336: // qvarsym1
      case 337: // varsym
      case 338: // varsym_no_minus
      case 339: // special_id
      case 340: // special_sym
      case 341: // qconid
      case 342: // conid
      case 343: // qconsym
      case 344: // consym
      case 347: // modid
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
      case 277: // pquals
      case 278: // squals
      case 280: // guardquals
      case 281: // guardquals1
      case 282: // altslist
      case 283: // alts
      case 284: // alts1
      case 288: // gdpats
      case 294: // apats1
      case 295: // stmtlist
      case 296: // stmts
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
#line 502 "parser.y" // lalr1.cc:870
    {drv.result = yystack_[0].value.as< expression_ref > ();}
#line 1362 "parser.cc" // lalr1.cc:870
    break;

  case 3:
#line 519 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_module(yystack_[4].value.as< std::string > (),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1368 "parser.cc" // lalr1.cc:870
    break;

  case 4:
#line 520 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_module("",{},yystack_[0].value.as< expression_ref > ());}
#line 1374 "parser.cc" // lalr1.cc:870
    break;

  case 9:
#line 530 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1380 "parser.cc" // lalr1.cc:870
    break;

  case 10:
#line 531 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1386 "parser.cc" // lalr1.cc:870
    break;

  case 11:
#line 533 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1392 "parser.cc" // lalr1.cc:870
    break;

  case 12:
#line 534 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1398 "parser.cc" // lalr1.cc:870
    break;

  case 13:
#line 537 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1404 "parser.cc" // lalr1.cc:870
    break;

  case 14:
#line 539 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1410 "parser.cc" // lalr1.cc:870
    break;

  case 15:
#line 540 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1416 "parser.cc" // lalr1.cc:870
    break;

  case 16:
#line 541 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[0].value.as< std::vector<expression_ref> > (),{});}
#line 1422 "parser.cc" // lalr1.cc:870
    break;

  case 17:
#line 549 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_exports(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1428 "parser.cc" // lalr1.cc:870
    break;

  case 18:
#line 550 "parser.y" // lalr1.cc:870
    {}
#line 1434 "parser.cc" // lalr1.cc:870
    break;

  case 19:
#line 552 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1440 "parser.cc" // lalr1.cc:870
    break;

  case 20:
#line 554 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1446 "parser.cc" // lalr1.cc:870
    break;

  case 21:
#line 555 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1452 "parser.cc" // lalr1.cc:870
    break;

  case 22:
#line 557 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1458 "parser.cc" // lalr1.cc:870
    break;

  case 23:
#line 558 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("module"),{String(yystack_[0].value.as< std::string > ())});}
#line 1464 "parser.cc" // lalr1.cc:870
    break;

  case 24:
#line 559 "parser.y" // lalr1.cc:870
    {}
#line 1470 "parser.cc" // lalr1.cc:870
    break;

  case 27:
#line 564 "parser.y" // lalr1.cc:870
    {}
#line 1476 "parser.cc" // lalr1.cc:870
    break;

  case 28:
#line 565 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1482 "parser.cc" // lalr1.cc:870
    break;

  case 29:
#line 567 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[3].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ());}
#line 1488 "parser.cc" // lalr1.cc:870
    break;

  case 30:
#line 568 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1494 "parser.cc" // lalr1.cc:870
    break;

  case 31:
#line 570 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1500 "parser.cc" // lalr1.cc:870
    break;

  case 32:
#line 571 "parser.y" // lalr1.cc:870
    {}
#line 1506 "parser.cc" // lalr1.cc:870
    break;

  case 33:
#line 573 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1512 "parser.cc" // lalr1.cc:870
    break;

  case 34:
#line 574 "parser.y" // lalr1.cc:870
    {}
#line 1518 "parser.cc" // lalr1.cc:870
    break;

  case 35:
#line 576 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("qvar"),{String(yystack_[0].value.as< std::string > ())});}
#line 1524 "parser.cc" // lalr1.cc:870
    break;

  case 36:
#line 577 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("qvar"),{String(yystack_[0].value.as< std::string > ())});}
#line 1530 "parser.cc" // lalr1.cc:870
    break;

  case 41:
#line 587 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()), yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ()); }
#line 1536 "parser.cc" // lalr1.cc:870
    break;

  case 42:
#line 589 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ()); }
#line 1542 "parser.cc" // lalr1.cc:870
    break;

  case 43:
#line 590 "parser.y" // lalr1.cc:870
    { }
#line 1548 "parser.cc" // lalr1.cc:870
    break;

  case 44:
#line 592 "parser.y" // lalr1.cc:870
    {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as< bool > ()) e.push_back(std::string("qualified"));
    e.push_back(String(yystack_[2].value.as< std::string > ()));
    yylhs.value.as< expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 1559 "parser.cc" // lalr1.cc:870
    break;

  case 45:
#line 599 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1565 "parser.cc" // lalr1.cc:870
    break;

  case 46:
#line 600 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1571 "parser.cc" // lalr1.cc:870
    break;

  case 47:
#line 602 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1577 "parser.cc" // lalr1.cc:870
    break;

  case 48:
#line 603 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1583 "parser.cc" // lalr1.cc:870
    break;

  case 49:
#line 605 "parser.y" // lalr1.cc:870
    { yylhs.value.as< boost::optional<std::string> > () = yystack_[0].value.as< std::string > (); }
#line 1589 "parser.cc" // lalr1.cc:870
    break;

  case 50:
#line 606 "parser.y" // lalr1.cc:870
    { }
#line 1595 "parser.cc" // lalr1.cc:870
    break;

  case 51:
#line 608 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1601 "parser.cc" // lalr1.cc:870
    break;

  case 52:
#line 609 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1607 "parser.cc" // lalr1.cc:870
    break;

  case 53:
#line 611 "parser.y" // lalr1.cc:870
    { yylhs.value.as< boost::optional<std::string> > () = yystack_[0].value.as< std::string > (); }
#line 1613 "parser.cc" // lalr1.cc:870
    break;

  case 54:
#line 612 "parser.y" // lalr1.cc:870
    { }
#line 1619 "parser.cc" // lalr1.cc:870
    break;

  case 59:
#line 623 "parser.y" // lalr1.cc:870
    { }
#line 1625 "parser.cc" // lalr1.cc:870
    break;

  case 60:
#line 624 "parser.y" // lalr1.cc:870
    { yylhs.value.as< boost::optional<int> > () = yystack_[0].value.as< int > (); }
#line 1631 "parser.cc" // lalr1.cc:870
    break;

  case 61:
#line 626 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infix";  }
#line 1637 "parser.cc" // lalr1.cc:870
    break;

  case 62:
#line 627 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infixl"; }
#line 1643 "parser.cc" // lalr1.cc:870
    break;

  case 63:
#line 628 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infixr"; }
#line 1649 "parser.cc" // lalr1.cc:870
    break;

  case 64:
#line 630 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<std::string> > (),yystack_[2].value.as< std::vector<std::string> > ()); yylhs.value.as< std::vector<std::string> > ().push_back(yystack_[0].value.as< std::string > ()); }
#line 1655 "parser.cc" // lalr1.cc:870
    break;

  case 65:
#line 631 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::vector<std::string> > () = {yystack_[0].value.as< std::string > ()}; }
#line 1661 "parser.cc" // lalr1.cc:870
    break;

  case 66:
#line 635 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ()); }
#line 1667 "parser.cc" // lalr1.cc:870
    break;

  case 67:
#line 637 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ()); }
#line 1673 "parser.cc" // lalr1.cc:870
    break;

  case 68:
#line 638 "parser.y" // lalr1.cc:870
    { }
#line 1679 "parser.cc" // lalr1.cc:870
    break;

  case 69:
#line 640 "parser.y" // lalr1.cc:870
    {}
#line 1685 "parser.cc" // lalr1.cc:870
    break;

  case 70:
#line 641 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1691 "parser.cc" // lalr1.cc:870
    break;

  case 71:
#line 642 "parser.y" // lalr1.cc:870
    {}
#line 1697 "parser.cc" // lalr1.cc:870
    break;

  case 72:
#line 645 "parser.y" // lalr1.cc:870
    {}
#line 1703 "parser.cc" // lalr1.cc:870
    break;

  case 73:
#line 652 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1709 "parser.cc" // lalr1.cc:870
    break;

  case 74:
#line 653 "parser.y" // lalr1.cc:870
    {}
#line 1715 "parser.cc" // lalr1.cc:870
    break;

  case 75:
#line 654 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[3].value.as< std::string > (),yystack_[2].value.as< int > (),yystack_[1].value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1721 "parser.cc" // lalr1.cc:870
    break;

  case 76:
#line 655 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[2].value.as< std::string > (),yystack_[1].value.as< int > (),yystack_[0].value.as< std::string > ());}
#line 1727 "parser.cc" // lalr1.cc:870
    break;

  case 77:
#line 656 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[3].value.as< std::string > (),yystack_[2].value.as< int > (),yystack_[1].value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1733 "parser.cc" // lalr1.cc:870
    break;

  case 78:
#line 657 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[2].value.as< std::string > (),yystack_[1].value.as< int > (),yystack_[0].value.as< std::string > ());}
#line 1739 "parser.cc" // lalr1.cc:870
    break;

  case 80:
#line 661 "parser.y" // lalr1.cc:870
    {}
#line 1745 "parser.cc" // lalr1.cc:870
    break;

  case 81:
#line 663 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_data_or_newtype(yystack_[4].value.as< std::string > (),yystack_[2].value.as< expression_ref > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1751 "parser.cc" // lalr1.cc:870
    break;

  case 82:
#line 664 "parser.y" // lalr1.cc:870
    {}
#line 1757 "parser.cc" // lalr1.cc:870
    break;

  case 95:
#line 722 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > ()="data";}
#line 1763 "parser.cc" // lalr1.cc:870
    break;

  case 96:
#line 723 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > ()="newtype";}
#line 1769 "parser.cc" // lalr1.cc:870
    break;

  case 99:
#line 735 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1775 "parser.cc" // lalr1.cc:870
    break;

  case 100:
#line 736 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1781 "parser.cc" // lalr1.cc:870
    break;

  case 135:
#line 805 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1787 "parser.cc" // lalr1.cc:870
    break;

  case 136:
#line 806 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1793 "parser.cc" // lalr1.cc:870
    break;

  case 137:
#line 807 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1799 "parser.cc" // lalr1.cc:870
    break;

  case 138:
#line 808 "parser.y" // lalr1.cc:870
    {}
#line 1805 "parser.cc" // lalr1.cc:870
    break;

  case 139:
#line 810 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1811 "parser.cc" // lalr1.cc:870
    break;

  case 140:
#line 811 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1817 "parser.cc" // lalr1.cc:870
    break;

  case 141:
#line 813 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decls"),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1823 "parser.cc" // lalr1.cc:870
    break;

  case 142:
#line 819 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1829 "parser.cc" // lalr1.cc:870
    break;

  case 143:
#line 820 "parser.y" // lalr1.cc:870
    {}
#line 1835 "parser.cc" // lalr1.cc:870
    break;

  case 149:
#line 841 "parser.y" // lalr1.cc:870
    {}
#line 1841 "parser.cc" // lalr1.cc:870
    break;

  case 150:
#line 842 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1847 "parser.cc" // lalr1.cc:870
    break;

  case 151:
#line 844 "parser.y" // lalr1.cc:870
    {}
#line 1853 "parser.cc" // lalr1.cc:870
    break;

  case 152:
#line 845 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 1859 "parser.cc" // lalr1.cc:870
    break;

  case 153:
#line 847 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1865 "parser.cc" // lalr1.cc:870
    break;

  case 154:
#line 849 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1871 "parser.cc" // lalr1.cc:870
    break;

  case 155:
#line 851 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< std::string > ());}
#line 1877 "parser.cc" // lalr1.cc:870
    break;

  case 156:
#line 852 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< std::string > ());}
#line 1883 "parser.cc" // lalr1.cc:870
    break;

  case 157:
#line 854 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1889 "parser.cc" // lalr1.cc:870
    break;

  case 158:
#line 855 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1895 "parser.cc" // lalr1.cc:870
    break;

  case 159:
#line 859 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1901 "parser.cc" // lalr1.cc:870
    break;

  case 160:
#line 860 "parser.y" // lalr1.cc:870
    {}
#line 1907 "parser.cc" // lalr1.cc:870
    break;

  case 161:
#line 861 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1913 "parser.cc" // lalr1.cc:870
    break;

  case 162:
#line 863 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = "!";}
#line 1919 "parser.cc" // lalr1.cc:870
    break;

  case 163:
#line 864 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = "~";}
#line 1925 "parser.cc" // lalr1.cc:870
    break;

  case 166:
#line 869 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("forall"),{make_tv_bndrs(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 1931 "parser.cc" // lalr1.cc:870
    break;

  case 167:
#line 870 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1937 "parser.cc" // lalr1.cc:870
    break;

  case 168:
#line 872 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1943 "parser.cc" // lalr1.cc:870
    break;

  case 169:
#line 874 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1949 "parser.cc" // lalr1.cc:870
    break;

  case 170:
#line 883 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1955 "parser.cc" // lalr1.cc:870
    break;

  case 171:
#line 885 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1961 "parser.cc" // lalr1.cc:870
    break;

  case 172:
#line 887 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1967 "parser.cc" // lalr1.cc:870
    break;

  case 173:
#line 888 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps({make_type_id("->"),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1973 "parser.cc" // lalr1.cc:870
    break;

  case 174:
#line 890 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1979 "parser.cc" // lalr1.cc:870
    break;

  case 175:
#line 893 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1985 "parser.cc" // lalr1.cc:870
    break;

  case 176:
#line 895 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1991 "parser.cc" // lalr1.cc:870
    break;

  case 177:
#line 896 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1997 "parser.cc" // lalr1.cc:870
    break;

  case 178:
#line 898 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2003 "parser.cc" // lalr1.cc:870
    break;

  case 179:
#line 899 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2009 "parser.cc" // lalr1.cc:870
    break;

  case 180:
#line 901 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2015 "parser.cc" // lalr1.cc:870
    break;

  case 181:
#line 902 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2021 "parser.cc" // lalr1.cc:870
    break;

  case 182:
#line 903 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2027 "parser.cc" // lalr1.cc:870
    break;

  case 183:
#line 909 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2033 "parser.cc" // lalr1.cc:870
    break;

  case 184:
#line 911 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2039 "parser.cc" // lalr1.cc:870
    break;

  case 185:
#line 912 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2045 "parser.cc" // lalr1.cc:870
    break;

  case 186:
#line 913 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("kind_star");}
#line 2051 "parser.cc" // lalr1.cc:870
    break;

  case 187:
#line 914 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("strictness"),{yystack_[1].value.as< std::string > (),yystack_[0].value.as< expression_ref > ()}};}
#line 2057 "parser.cc" // lalr1.cc:870
    break;

  case 188:
#line 915 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("FieldDecls"),yystack_[1].value.as< std::vector<expression_ref> > ()};}
#line 2063 "parser.cc" // lalr1.cc:870
    break;

  case 189:
#line 916 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id("()");}
#line 2069 "parser.cc" // lalr1.cc:870
    break;

  case 190:
#line 917 "parser.y" // lalr1.cc:870
    {auto ts = yystack_[3].value.as< std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as< expression_ref > ());yylhs.value.as< expression_ref > () = expression_ref{AST_node("TupleType"),ts};}
#line 2075 "parser.cc" // lalr1.cc:870
    break;

  case 191:
#line 918 "parser.y" // lalr1.cc:870
    {}
#line 2081 "parser.cc" // lalr1.cc:870
    break;

  case 192:
#line 919 "parser.y" // lalr1.cc:870
    {}
#line 2087 "parser.cc" // lalr1.cc:870
    break;

  case 193:
#line 920 "parser.y" // lalr1.cc:870
    {}
#line 2093 "parser.cc" // lalr1.cc:870
    break;

  case 194:
#line 921 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("ListType"),{yystack_[1].value.as< expression_ref > ()}};}
#line 2099 "parser.cc" // lalr1.cc:870
    break;

  case 195:
#line 922 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2105 "parser.cc" // lalr1.cc:870
    break;

  case 196:
#line 923 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("TypeOfKind"),{yystack_[3].value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ()}};}
#line 2111 "parser.cc" // lalr1.cc:870
    break;

  case 200:
#line 931 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2117 "parser.cc" // lalr1.cc:870
    break;

  case 201:
#line 932 "parser.y" // lalr1.cc:870
    {}
#line 2123 "parser.cc" // lalr1.cc:870
    break;

  case 202:
#line 934 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2129 "parser.cc" // lalr1.cc:870
    break;

  case 203:
#line 935 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2135 "parser.cc" // lalr1.cc:870
    break;

  case 206:
#line 940 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2141 "parser.cc" // lalr1.cc:870
    break;

  case 207:
#line 941 "parser.y" // lalr1.cc:870
    {}
#line 2147 "parser.cc" // lalr1.cc:870
    break;

  case 208:
#line 943 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("type_id",yystack_[0].value.as< std::string > ());}
#line 2153 "parser.cc" // lalr1.cc:870
    break;

  case 209:
#line 944 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("type_of_kind"),{AST_node("type_id",yystack_[3].value.as< std::string > ()),yystack_[1].value.as< expression_ref > ()});}
#line 2159 "parser.cc" // lalr1.cc:870
    break;

  case 217:
#line 959 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2165 "parser.cc" // lalr1.cc:870
    break;

  case 218:
#line 965 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2171 "parser.cc" // lalr1.cc:870
    break;

  case 219:
#line 967 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2177 "parser.cc" // lalr1.cc:870
    break;

  case 220:
#line 968 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2183 "parser.cc" // lalr1.cc:870
    break;

  case 221:
#line 970 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_context(yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2189 "parser.cc" // lalr1.cc:870
    break;

  case 222:
#line 971 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2195 "parser.cc" // lalr1.cc:870
    break;

  case 223:
#line 973 "parser.y" // lalr1.cc:870
    {if (yystack_[1].value.as< std::vector<expression_ref> > ().size()>1) yylhs.value.as< expression_ref > () = make_tv_bndrs(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2201 "parser.cc" // lalr1.cc:870
    break;

  case 224:
#line 974 "parser.y" // lalr1.cc:870
    {}
#line 2207 "parser.cc" // lalr1.cc:870
    break;

  case 225:
#line 976 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2213 "parser.cc" // lalr1.cc:870
    break;

  case 226:
#line 977 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps({AST_node("type_id",yystack_[1].value.as< std::string > ()),make_tyapps(yystack_[2].value.as< std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ())});}
#line 2219 "parser.cc" // lalr1.cc:870
    break;

  case 227:
#line 979 "parser.y" // lalr1.cc:870
    {}
#line 2225 "parser.cc" // lalr1.cc:870
    break;

  case 228:
#line 980 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2231 "parser.cc" // lalr1.cc:870
    break;

  case 229:
#line 982 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2237 "parser.cc" // lalr1.cc:870
    break;

  case 230:
#line 983 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2243 "parser.cc" // lalr1.cc:870
    break;

  case 231:
#line 985 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("FieldDecl"),{make_sig_vars(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2249 "parser.cc" // lalr1.cc:870
    break;

  case 242:
#line 1004 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2255 "parser.cc" // lalr1.cc:870
    break;

  case 243:
#line 1005 "parser.y" // lalr1.cc:870
    {}
#line 2261 "parser.cc" // lalr1.cc:870
    break;

  case 244:
#line 1007 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2267 "parser.cc" // lalr1.cc:870
    break;

  case 245:
#line 1008 "parser.y" // lalr1.cc:870
    {}
#line 2273 "parser.cc" // lalr1.cc:870
    break;

  case 246:
#line 1011 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2279 "parser.cc" // lalr1.cc:870
    break;

  case 247:
#line 1014 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_rhs(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2285 "parser.cc" // lalr1.cc:870
    break;

  case 248:
#line 1015 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_rhs(make_gdrhs(yystack_[1].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ());}
#line 2291 "parser.cc" // lalr1.cc:870
    break;

  case 249:
#line 1017 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2297 "parser.cc" // lalr1.cc:870
    break;

  case 250:
#line 1018 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2303 "parser.cc" // lalr1.cc:870
    break;

  case 251:
#line 1020 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("guardquals"),{make_gdpats(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2309 "parser.cc" // lalr1.cc:870
    break;

  case 252:
#line 1022 "parser.y" // lalr1.cc:870
    {}
#line 2315 "parser.cc" // lalr1.cc:870
    break;

  case 253:
#line 1023 "parser.y" // lalr1.cc:870
    {}
#line 2321 "parser.cc" // lalr1.cc:870
    break;

  case 254:
#line 1024 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_infix(yystack_[2].value.as< std::string > (),yystack_[1].value.as< boost::optional<int> > (),yystack_[0].value.as< std::vector<std::string> > ()); }
#line 2327 "parser.cc" // lalr1.cc:870
    break;

  case 255:
#line 1025 "parser.y" // lalr1.cc:870
    {}
#line 2333 "parser.cc" // lalr1.cc:870
    break;

  case 256:
#line 1026 "parser.y" // lalr1.cc:870
    {}
#line 2339 "parser.cc" // lalr1.cc:870
    break;

  case 257:
#line 1027 "parser.y" // lalr1.cc:870
    {}
#line 2345 "parser.cc" // lalr1.cc:870
    break;

  case 258:
#line 1028 "parser.y" // lalr1.cc:870
    {}
#line 2351 "parser.cc" // lalr1.cc:870
    break;

  case 259:
#line 1029 "parser.y" // lalr1.cc:870
    {}
#line 2357 "parser.cc" // lalr1.cc:870
    break;

  case 260:
#line 1030 "parser.y" // lalr1.cc:870
    {}
#line 2363 "parser.cc" // lalr1.cc:870
    break;

  case 261:
#line 1031 "parser.y" // lalr1.cc:870
    {}
#line 2369 "parser.cc" // lalr1.cc:870
    break;

  case 262:
#line 1032 "parser.y" // lalr1.cc:870
    {}
#line 2375 "parser.cc" // lalr1.cc:870
    break;

  case 267:
#line 1042 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()); }
#line 2381 "parser.cc" // lalr1.cc:870
    break;

  case 268:
#line 1043 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_infixexp(yystack_[0].value.as< std::vector<expression_ref> > ()); }
#line 2387 "parser.cc" // lalr1.cc:870
    break;

  case 269:
#line 1045 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2393 "parser.cc" // lalr1.cc:870
    break;

  case 270:
#line 1046 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as< std::string > ())); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2399 "parser.cc" // lalr1.cc:870
    break;

  case 271:
#line 1048 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2405 "parser.cc" // lalr1.cc:870
    break;

  case 272:
#line 1049 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as< std::string > ())); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2411 "parser.cc" // lalr1.cc:870
    break;

  case 273:
#line 1051 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_minus(make_fexp(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2417 "parser.cc" // lalr1.cc:870
    break;

  case 274:
#line 1052 "parser.y" // lalr1.cc:870
    {}
#line 2423 "parser.cc" // lalr1.cc:870
    break;

  case 275:
#line 1053 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_fexp(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2429 "parser.cc" // lalr1.cc:870
    break;

  case 276:
#line 1055 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2435 "parser.cc" // lalr1.cc:870
    break;

  case 277:
#line 1056 "parser.y" // lalr1.cc:870
    {}
#line 2441 "parser.cc" // lalr1.cc:870
    break;

  case 282:
#line 1067 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2447 "parser.cc" // lalr1.cc:870
    break;

  case 283:
#line 1068 "parser.y" // lalr1.cc:870
    {}
#line 2453 "parser.cc" // lalr1.cc:870
    break;

  case 284:
#line 1069 "parser.y" // lalr1.cc:870
    {}
#line 2459 "parser.cc" // lalr1.cc:870
    break;

  case 285:
#line 1070 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2465 "parser.cc" // lalr1.cc:870
    break;

  case 286:
#line 1072 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_as_pattern(yystack_[2].value.as< std::string > (),yystack_[0].value.as< expression_ref > ());}
#line 2471 "parser.cc" // lalr1.cc:870
    break;

  case 287:
#line 1073 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_lazy_pattern(yystack_[0].value.as< expression_ref > ());}
#line 2477 "parser.cc" // lalr1.cc:870
    break;

  case 288:
#line 1074 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_lambda(yystack_[2].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2483 "parser.cc" // lalr1.cc:870
    break;

  case 289:
#line 1075 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_let(yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2489 "parser.cc" // lalr1.cc:870
    break;

  case 290:
#line 1076 "parser.y" // lalr1.cc:870
    {}
#line 2495 "parser.cc" // lalr1.cc:870
    break;

  case 291:
#line 1077 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_if(yystack_[6].value.as< expression_ref > (),yystack_[3].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2501 "parser.cc" // lalr1.cc:870
    break;

  case 292:
#line 1078 "parser.y" // lalr1.cc:870
    {}
#line 2507 "parser.cc" // lalr1.cc:870
    break;

  case 293:
#line 1079 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_case(yystack_[2].value.as< expression_ref > (),make_alts(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2513 "parser.cc" // lalr1.cc:870
    break;

  case 294:
#line 1080 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_do(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2519 "parser.cc" // lalr1.cc:870
    break;

  case 295:
#line 1081 "parser.y" // lalr1.cc:870
    {}
#line 2525 "parser.cc" // lalr1.cc:870
    break;

  case 296:
#line 1082 "parser.y" // lalr1.cc:870
    {}
#line 2531 "parser.cc" // lalr1.cc:870
    break;

  case 297:
#line 1083 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2537 "parser.cc" // lalr1.cc:870
    break;

  case 298:
#line 1085 "parser.y" // lalr1.cc:870
    {}
#line 2543 "parser.cc" // lalr1.cc:870
    break;

  case 299:
#line 1086 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2549 "parser.cc" // lalr1.cc:870
    break;

  case 300:
#line 1088 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_id(yystack_[0].value.as< std::string > ());}
#line 2555 "parser.cc" // lalr1.cc:870
    break;

  case 301:
#line 1089 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_id(yystack_[0].value.as< std::string > ());}
#line 2561 "parser.cc" // lalr1.cc:870
    break;

  case 302:
#line 1090 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2567 "parser.cc" // lalr1.cc:870
    break;

  case 303:
#line 1091 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2573 "parser.cc" // lalr1.cc:870
    break;

  case 304:
#line 1092 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_tuple(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2579 "parser.cc" // lalr1.cc:870
    break;

  case 305:
#line 1093 "parser.y" // lalr1.cc:870
    {}
#line 2585 "parser.cc" // lalr1.cc:870
    break;

  case 306:
#line 1094 "parser.y" // lalr1.cc:870
    {}
#line 2591 "parser.cc" // lalr1.cc:870
    break;

  case 307:
#line 1095 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2597 "parser.cc" // lalr1.cc:870
    break;

  case 308:
#line 1096 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("WildcardPattern");}
#line 2603 "parser.cc" // lalr1.cc:870
    break;

  case 309:
#line 1101 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2609 "parser.cc" // lalr1.cc:870
    break;

  case 310:
#line 1102 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< std::string > ()});}
#line 2615 "parser.cc" // lalr1.cc:870
    break;

  case 311:
#line 1103 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("RightSection"),{yystack_[1].value.as< std::string > (),make_infixexp(yystack_[0].value.as< std::vector<expression_ref> > ())});}
#line 2621 "parser.cc" // lalr1.cc:870
    break;

  case 312:
#line 1108 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2627 "parser.cc" // lalr1.cc:870
    break;

  case 313:
#line 1109 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2633 "parser.cc" // lalr1.cc:870
    break;

  case 314:
#line 1127 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = {AST_node("id",":"),yystack_[0].value.as< expression_ref > (),AST_node("id","[]")}; }
#line 2639 "parser.cc" // lalr1.cc:870
    break;

  case 315:
#line 1128 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_list(yystack_[0].value.as< std::vector<expression_ref> > ()); }
#line 2645 "parser.cc" // lalr1.cc:870
    break;

  case 316:
#line 1129 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = new expression(AST_node("enumFrom"),{yystack_[1].value.as< expression_ref > ()}); }
#line 2651 "parser.cc" // lalr1.cc:870
    break;

  case 317:
#line 1130 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = new expression(AST_node("enumFromThen"),{yystack_[3].value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ()}); }
#line 2657 "parser.cc" // lalr1.cc:870
    break;

  case 318:
#line 1131 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = new expression(AST_node("enumFromTo"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2663 "parser.cc" // lalr1.cc:870
    break;

  case 319:
#line 1132 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = new expression(AST_node("enumFromToThen"),{yystack_[4].value.as< expression_ref > (),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2669 "parser.cc" // lalr1.cc:870
    break;

  case 320:
#line 1133 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = new expression(AST_node("ListComprehension"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2675 "parser.cc" // lalr1.cc:870
    break;

  case 321:
#line 1135 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2681 "parser.cc" // lalr1.cc:870
    break;

  case 322:
#line 1136 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2687 "parser.cc" // lalr1.cc:870
    break;

  case 323:
#line 1141 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_flattenedpquals(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2693 "parser.cc" // lalr1.cc:870
    break;

  case 324:
#line 1143 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(make_squals(yystack_[2].value.as< std::vector<expression_ref> > ()));yylhs.value.as< std::vector<expression_ref> > ().insert(yylhs.value.as< std::vector<expression_ref> > ().end(),yystack_[0].value.as< std::vector<expression_ref> > ().begin(),yystack_[0].value.as< std::vector<expression_ref> > ().end());}
#line 2699 "parser.cc" // lalr1.cc:870
    break;

  case 325:
#line 1144 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(make_squals(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2705 "parser.cc" // lalr1.cc:870
    break;

  case 326:
#line 1146 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2711 "parser.cc" // lalr1.cc:870
    break;

  case 327:
#line 1147 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2717 "parser.cc" // lalr1.cc:870
    break;

  case 328:
#line 1148 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2723 "parser.cc" // lalr1.cc:870
    break;

  case 329:
#line 1149 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2729 "parser.cc" // lalr1.cc:870
    break;

  case 330:
#line 1151 "parser.y" // lalr1.cc:870
    {}
#line 2735 "parser.cc" // lalr1.cc:870
    break;

  case 331:
#line 1152 "parser.y" // lalr1.cc:870
    {}
#line 2741 "parser.cc" // lalr1.cc:870
    break;

  case 332:
#line 1153 "parser.y" // lalr1.cc:870
    {}
#line 2747 "parser.cc" // lalr1.cc:870
    break;

  case 333:
#line 1154 "parser.y" // lalr1.cc:870
    {}
#line 2753 "parser.cc" // lalr1.cc:870
    break;

  case 334:
#line 1157 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2759 "parser.cc" // lalr1.cc:870
    break;

  case 335:
#line 1159 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ());yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2765 "parser.cc" // lalr1.cc:870
    break;

  case 336:
#line 1160 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2771 "parser.cc" // lalr1.cc:870
    break;

  case 337:
#line 1163 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2777 "parser.cc" // lalr1.cc:870
    break;

  case 338:
#line 1164 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2783 "parser.cc" // lalr1.cc:870
    break;

  case 339:
#line 1165 "parser.y" // lalr1.cc:870
    {}
#line 2789 "parser.cc" // lalr1.cc:870
    break;

  case 340:
#line 1166 "parser.y" // lalr1.cc:870
    {}
#line 2795 "parser.cc" // lalr1.cc:870
    break;

  case 341:
#line 1168 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2801 "parser.cc" // lalr1.cc:870
    break;

  case 342:
#line 1169 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2807 "parser.cc" // lalr1.cc:870
    break;

  case 343:
#line 1171 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2813 "parser.cc" // lalr1.cc:870
    break;

  case 344:
#line 1172 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2819 "parser.cc" // lalr1.cc:870
    break;

  case 345:
#line 1173 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2825 "parser.cc" // lalr1.cc:870
    break;

  case 346:
#line 1175 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_alt(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2831 "parser.cc" // lalr1.cc:870
    break;

  case 347:
#line 1177 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_alt_rhs(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2837 "parser.cc" // lalr1.cc:870
    break;

  case 348:
#line 1179 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2843 "parser.cc" // lalr1.cc:870
    break;

  case 349:
#line 1180 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_gdpats(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2849 "parser.cc" // lalr1.cc:870
    break;

  case 350:
#line 1182 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2855 "parser.cc" // lalr1.cc:870
    break;

  case 351:
#line 1183 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2861 "parser.cc" // lalr1.cc:870
    break;

  case 352:
#line 1185 "parser.y" // lalr1.cc:870
    {}
#line 2867 "parser.cc" // lalr1.cc:870
    break;

  case 353:
#line 1186 "parser.y" // lalr1.cc:870
    {}
#line 2873 "parser.cc" // lalr1.cc:870
    break;

  case 354:
#line 1188 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > ()=make_gdpat(make_gdpats(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ());}
#line 2879 "parser.cc" // lalr1.cc:870
    break;

  case 355:
#line 1190 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2885 "parser.cc" // lalr1.cc:870
    break;

  case 356:
#line 1191 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("StrictPat"),{yystack_[0].value.as< expression_ref > ()});}
#line 2891 "parser.cc" // lalr1.cc:870
    break;

  case 357:
#line 1193 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2897 "parser.cc" // lalr1.cc:870
    break;

  case 358:
#line 1194 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("StrictPat"),{yystack_[0].value.as< expression_ref > ()});}
#line 2903 "parser.cc" // lalr1.cc:870
    break;

  case 359:
#line 1196 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2909 "parser.cc" // lalr1.cc:870
    break;

  case 360:
#line 1197 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("StrictPat"),{yystack_[0].value.as< expression_ref > ()});}
#line 2915 "parser.cc" // lalr1.cc:870
    break;

  case 361:
#line 1199 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2921 "parser.cc" // lalr1.cc:870
    break;

  case 362:
#line 1200 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2927 "parser.cc" // lalr1.cc:870
    break;

  case 363:
#line 1203 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2933 "parser.cc" // lalr1.cc:870
    break;

  case 364:
#line 1204 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2939 "parser.cc" // lalr1.cc:870
    break;

  case 365:
#line 1206 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2945 "parser.cc" // lalr1.cc:870
    break;

  case 366:
#line 1207 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2951 "parser.cc" // lalr1.cc:870
    break;

  case 367:
#line 1208 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2957 "parser.cc" // lalr1.cc:870
    break;

  case 368:
#line 1209 "parser.y" // lalr1.cc:870
    {}
#line 2963 "parser.cc" // lalr1.cc:870
    break;

  case 369:
#line 1214 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< expression_ref > ();}
#line 2969 "parser.cc" // lalr1.cc:870
    break;

  case 370:
#line 1215 "parser.y" // lalr1.cc:870
    {}
#line 2975 "parser.cc" // lalr1.cc:870
    break;

  case 371:
#line 1217 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("PatQual"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 2981 "parser.cc" // lalr1.cc:870
    break;

  case 372:
#line 1218 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("SimpleQual"),{yystack_[0].value.as< expression_ref > ()});}
#line 2987 "parser.cc" // lalr1.cc:870
    break;

  case 373:
#line 1219 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("LetQual"),{yystack_[0].value.as< expression_ref > ()});}
#line 2993 "parser.cc" // lalr1.cc:870
    break;

  case 381:
#line 1264 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 2999 "parser.cc" // lalr1.cc:870
    break;

  case 382:
#line 1265 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3005 "parser.cc" // lalr1.cc:870
    break;

  case 383:
#line 1267 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3011 "parser.cc" // lalr1.cc:870
    break;

  case 384:
#line 1268 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3017 "parser.cc" // lalr1.cc:870
    break;

  case 385:
#line 1270 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3023 "parser.cc" // lalr1.cc:870
    break;

  case 386:
#line 1271 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3029 "parser.cc" // lalr1.cc:870
    break;

  case 387:
#line 1272 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3035 "parser.cc" // lalr1.cc:870
    break;

  case 390:
#line 1277 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () =  "()"; }
#line 3041 "parser.cc" // lalr1.cc:870
    break;

  case 391:
#line 1278 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "("+std::string(yystack_[1].value.as< int > (),',')+")"; }
#line 3047 "parser.cc" // lalr1.cc:870
    break;

  case 392:
#line 1279 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(##)"; }
#line 3053 "parser.cc" // lalr1.cc:870
    break;

  case 393:
#line 1280 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(#"+std::string(yystack_[1].value.as< int > (),',')+"#)"; }
#line 3059 "parser.cc" // lalr1.cc:870
    break;

  case 394:
#line 1282 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3065 "parser.cc" // lalr1.cc:870
    break;

  case 395:
#line 1283 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "[]"; }
#line 3071 "parser.cc" // lalr1.cc:870
    break;

  case 396:
#line 1285 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3077 "parser.cc" // lalr1.cc:870
    break;

  case 397:
#line 1286 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3083 "parser.cc" // lalr1.cc:870
    break;

  case 398:
#line 1288 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3089 "parser.cc" // lalr1.cc:870
    break;

  case 399:
#line 1289 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3095 "parser.cc" // lalr1.cc:870
    break;

  case 400:
#line 1292 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3101 "parser.cc" // lalr1.cc:870
    break;

  case 401:
#line 1293 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "()"; }
#line 3107 "parser.cc" // lalr1.cc:870
    break;

  case 402:
#line 1294 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(##)"; }
#line 3113 "parser.cc" // lalr1.cc:870
    break;

  case 403:
#line 1296 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3119 "parser.cc" // lalr1.cc:870
    break;

  case 404:
#line 1297 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "("+std::string(yystack_[1].value.as< int > (),',')+")"; }
#line 3125 "parser.cc" // lalr1.cc:870
    break;

  case 405:
#line 1298 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(#"+std::string(yystack_[1].value.as< int > (),',')+"#)"; }
#line 3131 "parser.cc" // lalr1.cc:870
    break;

  case 406:
#line 1299 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "->"; }
#line 3137 "parser.cc" // lalr1.cc:870
    break;

  case 407:
#line 1300 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "[]"; }
#line 3143 "parser.cc" // lalr1.cc:870
    break;

  case 408:
#line 1302 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3149 "parser.cc" // lalr1.cc:870
    break;

  case 409:
#line 1303 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3155 "parser.cc" // lalr1.cc:870
    break;

  case 410:
#line 1304 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "~"; }
#line 3161 "parser.cc" // lalr1.cc:870
    break;

  case 411:
#line 1306 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3167 "parser.cc" // lalr1.cc:870
    break;

  case 412:
#line 1307 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3173 "parser.cc" // lalr1.cc:870
    break;

  case 413:
#line 1308 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3179 "parser.cc" // lalr1.cc:870
    break;

  case 414:
#line 1309 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3185 "parser.cc" // lalr1.cc:870
    break;

  case 415:
#line 1310 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "~"; }
#line 3191 "parser.cc" // lalr1.cc:870
    break;

  case 416:
#line 1313 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3197 "parser.cc" // lalr1.cc:870
    break;

  case 417:
#line 1314 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3203 "parser.cc" // lalr1.cc:870
    break;

  case 418:
#line 1316 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3209 "parser.cc" // lalr1.cc:870
    break;

  case 419:
#line 1318 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3215 "parser.cc" // lalr1.cc:870
    break;

  case 420:
#line 1319 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3221 "parser.cc" // lalr1.cc:870
    break;

  case 421:
#line 1323 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3227 "parser.cc" // lalr1.cc:870
    break;

  case 422:
#line 1325 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3233 "parser.cc" // lalr1.cc:870
    break;

  case 423:
#line 1326 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3239 "parser.cc" // lalr1.cc:870
    break;

  case 424:
#line 1327 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3245 "parser.cc" // lalr1.cc:870
    break;

  case 425:
#line 1329 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3251 "parser.cc" // lalr1.cc:870
    break;

  case 426:
#line 1330 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3257 "parser.cc" // lalr1.cc:870
    break;

  case 427:
#line 1331 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3263 "parser.cc" // lalr1.cc:870
    break;

  case 428:
#line 1332 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "-"; }
#line 3269 "parser.cc" // lalr1.cc:870
    break;

  case 429:
#line 1337 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3275 "parser.cc" // lalr1.cc:870
    break;

  case 430:
#line 1338 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3281 "parser.cc" // lalr1.cc:870
    break;

  case 431:
#line 1340 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3287 "parser.cc" // lalr1.cc:870
    break;

  case 432:
#line 1341 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3293 "parser.cc" // lalr1.cc:870
    break;

  case 433:
#line 1343 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3299 "parser.cc" // lalr1.cc:870
    break;

  case 434:
#line 1344 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3305 "parser.cc" // lalr1.cc:870
    break;

  case 435:
#line 1345 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3311 "parser.cc" // lalr1.cc:870
    break;

  case 436:
#line 1347 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3317 "parser.cc" // lalr1.cc:870
    break;

  case 437:
#line 1348 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3323 "parser.cc" // lalr1.cc:870
    break;

  case 438:
#line 1349 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3329 "parser.cc" // lalr1.cc:870
    break;

  case 439:
#line 1351 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "_"; }
#line 3335 "parser.cc" // lalr1.cc:870
    break;

  case 440:
#line 1353 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3341 "parser.cc" // lalr1.cc:870
    break;

  case 441:
#line 1354 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3347 "parser.cc" // lalr1.cc:870
    break;

  case 442:
#line 1356 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () =yystack_[0].value.as< std::string > (); }
#line 3353 "parser.cc" // lalr1.cc:870
    break;

  case 443:
#line 1357 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3359 "parser.cc" // lalr1.cc:870
    break;

  case 444:
#line 1361 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3365 "parser.cc" // lalr1.cc:870
    break;

  case 445:
#line 1363 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3371 "parser.cc" // lalr1.cc:870
    break;

  case 446:
#line 1365 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3377 "parser.cc" // lalr1.cc:870
    break;

  case 447:
#line 1366 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3383 "parser.cc" // lalr1.cc:870
    break;

  case 448:
#line 1367 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unsafe"; }
#line 3389 "parser.cc" // lalr1.cc:870
    break;

  case 449:
#line 1368 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "safe"; }
#line 3395 "parser.cc" // lalr1.cc:870
    break;

  case 450:
#line 1369 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "interruptible"; }
#line 3401 "parser.cc" // lalr1.cc:870
    break;

  case 451:
#line 1372 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3407 "parser.cc" // lalr1.cc:870
    break;

  case 452:
#line 1373 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3413 "parser.cc" // lalr1.cc:870
    break;

  case 453:
#line 1375 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3419 "parser.cc" // lalr1.cc:870
    break;

  case 454:
#line 1376 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3425 "parser.cc" // lalr1.cc:870
    break;

  case 455:
#line 1377 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3431 "parser.cc" // lalr1.cc:870
    break;

  case 456:
#line 1379 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3437 "parser.cc" // lalr1.cc:870
    break;

  case 457:
#line 1380 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3443 "parser.cc" // lalr1.cc:870
    break;

  case 458:
#line 1382 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3449 "parser.cc" // lalr1.cc:870
    break;

  case 459:
#line 1383 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3455 "parser.cc" // lalr1.cc:870
    break;

  case 460:
#line 1384 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unsafe"; }
#line 3461 "parser.cc" // lalr1.cc:870
    break;

  case 461:
#line 1385 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "safe"; }
#line 3467 "parser.cc" // lalr1.cc:870
    break;

  case 462:
#line 1386 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "interruptible"; }
#line 3473 "parser.cc" // lalr1.cc:870
    break;

  case 463:
#line 1387 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "forall"; }
#line 3479 "parser.cc" // lalr1.cc:870
    break;

  case 464:
#line 1388 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "family"; }
#line 3485 "parser.cc" // lalr1.cc:870
    break;

  case 465:
#line 1389 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "role"; }
#line 3491 "parser.cc" // lalr1.cc:870
    break;

  case 466:
#line 1391 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3497 "parser.cc" // lalr1.cc:870
    break;

  case 467:
#line 1392 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3503 "parser.cc" // lalr1.cc:870
    break;

  case 468:
#line 1394 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3509 "parser.cc" // lalr1.cc:870
    break;

  case 469:
#line 1395 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3515 "parser.cc" // lalr1.cc:870
    break;

  case 470:
#line 1397 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3521 "parser.cc" // lalr1.cc:870
    break;

  case 471:
#line 1399 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3527 "parser.cc" // lalr1.cc:870
    break;

  case 472:
#line 1400 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "-"; }
#line 3533 "parser.cc" // lalr1.cc:870
    break;

  case 473:
#line 1402 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3539 "parser.cc" // lalr1.cc:870
    break;

  case 474:
#line 1403 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3545 "parser.cc" // lalr1.cc:870
    break;

  case 475:
#line 1405 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "as"; }
#line 3551 "parser.cc" // lalr1.cc:870
    break;

  case 476:
#line 1406 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "qualified"; }
#line 3557 "parser.cc" // lalr1.cc:870
    break;

  case 477:
#line 1407 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "hiding"; }
#line 3563 "parser.cc" // lalr1.cc:870
    break;

  case 478:
#line 1408 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "export"; }
#line 3569 "parser.cc" // lalr1.cc:870
    break;

  case 479:
#line 1409 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "label"; }
#line 3575 "parser.cc" // lalr1.cc:870
    break;

  case 480:
#line 1410 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "dynamic"; }
#line 3581 "parser.cc" // lalr1.cc:870
    break;

  case 481:
#line 1411 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "stdcall"; }
#line 3587 "parser.cc" // lalr1.cc:870
    break;

  case 482:
#line 1412 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "ccall"; }
#line 3593 "parser.cc" // lalr1.cc:870
    break;

  case 483:
#line 1413 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "capi"; }
#line 3599 "parser.cc" // lalr1.cc:870
    break;

  case 484:
#line 1414 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "prim"; }
#line 3605 "parser.cc" // lalr1.cc:870
    break;

  case 485:
#line 1415 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "javascript"; }
#line 3611 "parser.cc" // lalr1.cc:870
    break;

  case 486:
#line 1416 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "group"; }
#line 3617 "parser.cc" // lalr1.cc:870
    break;

  case 487:
#line 1417 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "stock"; }
#line 3623 "parser.cc" // lalr1.cc:870
    break;

  case 488:
#line 1418 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "anyclass"; }
#line 3629 "parser.cc" // lalr1.cc:870
    break;

  case 489:
#line 1419 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "via"; }
#line 3635 "parser.cc" // lalr1.cc:870
    break;

  case 490:
#line 1420 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unit"; }
#line 3641 "parser.cc" // lalr1.cc:870
    break;

  case 491:
#line 1421 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "dependency"; }
#line 3647 "parser.cc" // lalr1.cc:870
    break;

  case 492:
#line 1422 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "signature"; }
#line 3653 "parser.cc" // lalr1.cc:870
    break;

  case 493:
#line 1424 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "!"; }
#line 3659 "parser.cc" // lalr1.cc:870
    break;

  case 494:
#line 1425 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "."; }
#line 3665 "parser.cc" // lalr1.cc:870
    break;

  case 495:
#line 1426 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "*"; }
#line 3671 "parser.cc" // lalr1.cc:870
    break;

  case 496:
#line 1430 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3677 "parser.cc" // lalr1.cc:870
    break;

  case 497:
#line 1431 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3683 "parser.cc" // lalr1.cc:870
    break;

  case 498:
#line 1433 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3689 "parser.cc" // lalr1.cc:870
    break;

  case 499:
#line 1435 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3695 "parser.cc" // lalr1.cc:870
    break;

  case 500:
#line 1436 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3701 "parser.cc" // lalr1.cc:870
    break;

  case 501:
#line 1438 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3707 "parser.cc" // lalr1.cc:870
    break;

  case 502:
#line 1439 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3713 "parser.cc" // lalr1.cc:870
    break;

  case 503:
#line 1443 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< char > ();}
#line 3719 "parser.cc" // lalr1.cc:870
    break;

  case 504:
#line 1444 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = String(yystack_[0].value.as< std::string > ());}
#line 3725 "parser.cc" // lalr1.cc:870
    break;

  case 505:
#line 1445 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< int > ();}
#line 3731 "parser.cc" // lalr1.cc:870
    break;

  case 506:
#line 1446 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< double > ();}
#line 3737 "parser.cc" // lalr1.cc:870
    break;

  case 508:
#line 1454 "parser.y" // lalr1.cc:870
    { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 3743 "parser.cc" // lalr1.cc:870
    break;

  case 509:
#line 1458 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3749 "parser.cc" // lalr1.cc:870
    break;

  case 510:
#line 1459 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3755 "parser.cc" // lalr1.cc:870
    break;

  case 511:
#line 1461 "parser.y" // lalr1.cc:870
    {yylhs.value.as< int > () = yystack_[1].value.as< int > () + 1;}
#line 3761 "parser.cc" // lalr1.cc:870
    break;

  case 512:
#line 1462 "parser.y" // lalr1.cc:870
    {yylhs.value.as< int > () = 1;}
#line 3767 "parser.cc" // lalr1.cc:870
    break;


#line 3771 "parser.cc" // lalr1.cc:870
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


  const short parser::yypact_ninf_ = -681;

  const short parser::yytable_ninf_ = -472;

  const short
  parser::yypact_[] =
  {
      62,   180,  -681,   118,  -681,  -681,  -681,  -681,  -681,   125,
      69,    -2,  -681,    68,   127,   127,   104,  -681,  -681,  -681,
    -681,   149,  -681,  -681,  -681,   119,  -681,   185,   191,  4951,
     247,   237,   255,  -681,   835,  -681,    -7,  -681,  -681,  -681,
    -681,   180,  -681,     2,  -681,  -681,  -681,  -681,  -681,  -681,
    -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,   110,
    -681,  -681,  -681,  -681,  -681,  -681,   377,  -681,  -681,  -681,
    -681,   289,   290,  -681,   305,  -681,  -681,  -681,  -681,  -681,
    -681,  -681,  -681,   132,   349,   406,  -681,   330,  -681,  2901,
    -681,   341,   258,  2005,  -681,  -681,  -681,   398,   326,  -681,
    4517,  5215,   258,  3797,  5305,  3797,   350,   324,  5158,   168,
    3413,  3797,  3541,  3797,  1621,  1365,  1493,  -681,  -681,  -681,
    -681,  -681,  -681,  4517,    60,   350,   351,   255,  -681,  -681,
    -681,    65,  -681,  -681,  -681,  -681,   446,  -681,  3669,  -681,
     388,  -681,  -681,  -681,  -681,  -681,   345,   408,   379,  -681,
    -681,  -681,  -681,   370,  -681,   195,  -681,  -681,   395,    98,
     225,  -681,   396,   397,  -681,  -681,  -681,  -681,  -681,   399,
    -681,   400,   401,   404,  -681,  -681,  -681,  4951,  4997,  -681,
    -681,  -681,  -681,  -681,  -681,   480,  -681,   -16,  1365,   501,
     209,  -681,  -681,  2901,  4211,  2133,  2133,  -681,  2645,   445,
     418,   113,  -681,  -681,   455,   456,   457,   458,  4211,  1105,
    1105,  -681,   525,  -681,  -681,  -681,   402,   403,  -681,  -681,
    -681,  -681,  -681,  5459,  4109,  3905,  4007,  5055,  -681,  -681,
    -681,  -681,  -681,  4851,  -681,   323,   461,   459,  4517,  -681,
    -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,
     159,  5505,   415,   421,  -681,  -681,  -681,   466,    13,   224,
    5405,   473,  -681,    38,  -681,  -681,    25,  5158,  -681,   478,
     339,     7,   442,   483,  2261,  3797,  -681,  -681,  3285,  -681,
    3669,   337,  -681,  -681,  4619,  -681,  -681,  -681,   209,    96,
     460,   454,  -681,  2901,  -681,  -681,  -681,  -681,  -681,  -681,
    -681,  3541,  -681,  -681,   144,   158,   401,   468,   470,   472,
     230,  -681,   257,   280,   282,   490,   495,  -681,   334,  4211,
    5158,  5158,  -681,   607,   330,   516,   464,  4517,  4211,  4619,
     337,  -681,  3157,  -681,  -681,  -681,  -681,  -681,  4851,  -681,
    5101,  5459,  3797,  -681,   481,   485,   472,  -681,  -681,  -681,
    -681,  -681,  -681,  -681,  -681,   486,   484,  -681,  -681,   497,
      68,  -681,   467,   522,   527,   348,  4211,  2901,  -681,  -681,
    -681,   513,  -681,   498,   496,   326,   258,  3797,   526,   530,
     154,  -681,  -681,    54,   531,   502,  -681,    19,  -681,   597,
    -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,   596,   207,
    -681,  -681,   623,    70,  2901,  -681,  -681,    11,   528,   511,
    -681,  -681,  -681,   523,   518,   481,  -681,    77,   515,   485,
     250,  -681,   546,   285,   524,   292,   532,   533,  -681,  -681,
    4211,  4211,  -681,   529,   536,   510,   512,  2901,   534,  2773,
    2773,  5505,   168,  -681,  5505,  4211,   521,  5505,  -681,   514,
     537,   568,  -681,  -681,   569,   238,   576,  1877,  1237,  -681,
    -681,  2901,  -681,  2901,  2645,  -681,    55,  -681,   539,   540,
     545,  2901,  2901,  2389,  1749,  -681,  1749,   816,  -681,  1749,
    -681,  1749,   547,  -681,  -681,  -681,  -681,  -681,  -681,  -681,
     636,  4517,   586,   585,   591,  5351,   552,  -681,  -681,  -681,
    4313,    12,    95,  -681,  -681,   360,  -681,   557,  -681,  -681,
    -681,  -681,   573,  -681,   562,   598,    30,  -681,  -681,  -681,
    -681,  4997,  -681,  -681,  -681,   180,  -681,  -681,  -681,  -681,
    -681,  5551,  4211,  -681,  4211,   525,  -681,  -681,  2901,  -681,
    2133,  -681,  2901,  2645,  -681,  2901,   376,  -681,  -681,  1105,
    -681,  -681,  4211,  5459,  -681,  5459,  -681,  -681,  4211,  -681,
    4211,  -681,  4211,  -681,  -681,  -681,  -681,  -681,  -681,  -681,
    -681,  -681,  -681,   554,   555,  -681,  -681,  3797,  -681,  -681,
     658,   587,  5505,  -681,  -681,  -681,   570,  -681,   584,  -681,
    -681,  -681,   590,   500,   293,  -681,  -681,  -681,  -681,  2517,
     589,   577,  -681,   392,    68,  -681,  -681,   665,   615,   326,
    -681,  -681,  -681,  -681,  -681,  -681,  3029,  -681,  -681,   120,
    -681,  -681,   622,  -681,  -681,  -681,  -681,   592,  -681,  5655,
     380,  -681,  -681,  -681,  4211,  4211,   607,  4211,  -681,   620,
    -681,   628,   681,  -681,   704,  -681,  -681,  5101,  1749,  4211,
     604,   715,  4211,  5712,  -681,  -681,  -681,  -681,  -681,  -681,
    -681,  -681,   418,  1105,  1105,  -681,  -681,  -681,  -681,  -681,
    -681,   613,   616,   546,  -681,  -681,  -681,  -681,   384,  -681,
    -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  2773,  2901,
    -681,   665,   445,  -681,  -681,  2901,  -681,   275,   676,  2389,
    2389,  2901,  -681,  -681,  -681,   973,   973,  -681,  -681,    27,
      29,  -681,  -681,  -681,  -681,  -681,   641,  -681,  4851,   243,
    -681,   704,  -681,  -681,  -681,  -681,  -681,   180,    61,  -681,
     649,   724,  -681,   208,  -681,    85,  -681,  -681,  1105,  1105,
    -681,  -681,  -681,  -681,  2901,  2901,  2901,  -681,  -681,  -681,
    -681,  -681,  5712,  2901,  -681,   212,  -681,   101,  -681,  4211,
    -681,  5585,   681,   646,  4665,  -681,  -681,  -681,  -681,  -681,
    -681,  4415,   139,   683,  -681,  -681,  -681,  -681,   630,  4951,
    -681,  -681,  4211,  2901,  -681,  1105,  -681,   213,    70,   691,
    -681,  -681,   680,  -681,   973,  -681,  -681,  -681,  -681,  4851,
    -681,  4851,  -681,  -681,   626,   633,  -681,  4517,  -681,  4951,
     640,   643,  -681,  -681,  -681,  -681,  2901,  4211,  -681,  4758,
    -681,  4851,  4517,  -681,  -681,   645,  -681,  -681,  -681,  -681,
    -681,  -681
  };

  const unsigned short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   509,   510,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    68,   508,   507,    12,   148,   144,     0,     0,     0,
       0,    46,    41,    15,    14,   147,     0,     6,     7,   475,
     477,     0,   476,     0,   463,   478,   479,   480,   461,   462,
     460,   464,   465,   481,   482,   483,   484,   485,   486,     0,
     487,   488,   489,   490,   492,   491,     0,   458,   421,   457,
     419,     0,    19,    21,    25,    33,    36,   411,   420,    35,
     453,   456,   459,     0,     0,    48,    38,    42,   308,     0,
      95,     0,     0,     0,    61,    62,    63,    90,     0,    96,
       0,     0,     0,     0,     0,     0,   263,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   498,   497,   503,
     504,   505,   506,     0,   263,   263,    59,    66,    69,    70,
      71,   103,   245,   255,    73,   242,    74,   271,   275,   285,
     297,   299,   301,   381,   394,   382,     0,   300,   456,   383,
     496,   302,   145,     0,    23,     0,    34,   408,     0,     0,
       0,    24,     0,     0,   472,   493,   495,   494,   473,     0,
     470,     0,     0,     0,   471,   474,    17,     0,    27,    22,
      40,    40,     3,    45,    47,    52,    37,     0,     0,     0,
     268,   276,   269,     0,   201,   368,   368,   294,     0,     0,
     279,     0,   292,   351,     0,     0,     0,     0,     0,   138,
     138,   141,     0,   449,   450,   448,     0,     0,   427,   163,
     428,   162,   186,   227,     0,     0,     0,     0,   446,   426,
     425,   423,   422,     0,   159,   160,     0,   172,   175,   178,
     180,   184,   403,   181,   416,   424,   185,   182,   444,   447,
       0,     0,     0,     0,   451,   431,   295,     0,     0,     0,
     110,     0,   387,     0,   385,   284,     0,     0,   264,     0,
       0,     0,   388,   151,     0,     0,   359,   362,     0,   287,
     273,     0,   502,   395,     0,   501,   500,   309,   268,   314,
       0,   315,   437,     0,   438,   436,   442,   469,   468,   398,
     499,   472,   390,   512,     0,     0,   469,     0,   468,   398,
       0,   392,     0,     0,     0,   210,     0,   100,   172,     0,
       0,     0,    60,     0,    67,   103,     0,     0,     0,     0,
       0,   434,     0,   435,   433,   440,   467,   466,     0,   282,
     375,     0,     0,   146,     0,     0,     0,   414,   415,   413,
     412,   455,   454,    20,    32,     0,    28,    30,    31,     0,
       0,    51,    50,     0,     0,     0,     0,     0,   277,   207,
     202,     0,   168,     0,   200,     0,     0,     0,   372,     0,
       0,   367,   369,     0,     0,   334,   336,     0,   278,     0,
     350,   353,    87,    86,    88,    89,   197,   153,   134,     0,
     246,   137,   149,     0,     0,   164,   165,     0,     0,   228,
     230,   156,   407,     0,     0,   163,   189,   202,     0,   416,
       0,   191,   202,     0,     0,     0,     0,     0,   187,   161,
       0,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   110,     0,     0,     0,   396,     0,
       0,     0,   274,   258,     0,     0,     0,     0,     0,   290,
     360,     0,   361,     0,     0,   243,   143,   250,     0,     0,
       0,   310,   316,     0,     0,   307,     0,   311,   303,     0,
     304,     0,   454,   384,   391,   511,   305,   306,   393,   215,
     125,     0,     0,     0,     0,     0,   254,   430,    65,   429,
      97,     0,    97,   150,   252,   169,   154,     0,   244,   272,
     283,   378,     0,   374,   377,   380,     0,   286,   410,   409,
      26,     0,     9,    10,    49,     0,   281,   280,   293,   267,
     270,     0,     0,    72,     0,   373,   370,   358,     0,   363,
     366,   364,     0,     0,   352,     0,     0,    83,   139,   136,
     140,   289,     0,     0,   188,     0,   194,   406,     0,   195,
       0,   404,     0,   192,   193,   405,   417,   445,   169,    80,
     173,   452,   432,    78,    76,   296,   386,     0,   355,   104,
     105,     0,   110,   389,   111,   115,     0,   108,     0,   265,
     257,   259,     0,     0,     0,   152,   400,   256,   339,     0,
       0,   341,   345,     0,     0,   340,   288,   143,     0,     0,
     248,   249,   439,   443,   399,   318,     0,   320,   323,   325,
     328,   329,   309,   322,   321,   313,   312,   211,   213,     0,
       0,    79,    99,   262,     0,     0,     0,     0,    85,     0,
     102,     0,   224,    82,   232,   441,   298,     0,     0,     0,
       0,    54,     0,     0,   206,   208,   167,   203,   371,   365,
     354,   335,   279,   130,   130,   133,   135,   231,   155,   229,
     217,     0,   203,   204,   205,    77,    75,   356,     0,   106,
     109,   112,   397,   266,   401,   402,   342,   337,   344,     0,
     346,   143,   349,   338,   247,     0,   142,   486,   330,     0,
       0,   317,   215,   215,   216,   121,   121,   124,   157,     0,
       0,    64,    98,    84,   101,   207,   218,   220,     0,     0,
      81,   233,   235,   376,   379,   253,    29,     0,    56,   166,
       0,     0,   129,     0,   126,     0,   196,   190,   138,   138,
     343,   348,   347,   251,     0,     0,     0,   324,   326,   327,
     319,   212,   214,     0,   120,     0,   116,     0,   260,     0,
     261,     0,   224,     0,   225,   176,   183,   222,    93,    91,
      92,     0,     0,   236,   239,   418,   234,    53,     0,     0,
      44,    55,     0,     0,   131,   128,   132,     0,     0,     0,
     332,   331,     0,   122,   119,   123,   158,   223,   219,     0,
     177,     0,   240,   174,   198,     0,   237,     0,   238,     0,
       0,     0,   291,   127,   113,   114,     0,     0,   118,   225,
     221,   226,     0,   241,    94,     0,    57,   209,   333,   117,
     199,    58
  };

  const short
  parser::yypgoto_[] =
  {
    -681,  -681,  -681,  -681,  -681,  -681,  -681,    44,  -681,  -681,
    -680,  -681,   567,  -681,  -681,  -681,   228,  -143,  -681,   629,
    -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,
    -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,  -681,
    -681,  -681,  -681,  -681,   253,  -279,   433,  -681,  -681,  -388,
    -681,  -681,  -681,   -33,    56,  -681,  -681,   -21,   105,  -681,
    -681,  -181,  -681,  -333,  -544,   750,  -681,  -681,  -681,  -304,
    -412,   427,   138,  -681,   543,  -681,  -186,   353,   -92,  -681,
     -91,  -681,   -90,  -313,  -681,   549,  -648,  -216,   462,   -47,
    -681,   111,   222,    73,  -681,  -681,  -681,    83,    86,  -587,
     151,  -681,    31,  -681,    -8,  -681,  -681,   239,  -681,  -681,
      71,    24,   764,  -504,   471,  -681,   336,  -681,   309,  -681,
     -88,   -94,   771,   -28,  -316,   145,  -681,   -58,   -15,  -681,
    -681,   -39,   690,  -681,  -681,  -681,   109,  -681,   112,   346,
    -681,   444,  -406,  -681,   123,  -681,  -681,  -165,  -681,  -182,
      50,  -681,   535,  -681,   -59,   619,   277,  -180,  -681,   171,
    -681,   760,  -681,   716,   -65,  -681,   -44,  -248,  -123,  -681,
     366,   780,  -681,  -681,  -681,   -27,  -681,  -132,  -681,   188,
     725,   -99,  -681,  -122,  -681,  -681,  -465,  -681,   600,   -71,
     -29,  -199,   -31,  -681,  -681,   -62,   -54,   -57,    15,  -681,
    -197,   -63,   -51,  -247,  -681,  -176,   -34,   -80
  };

  const short
  parser::yydefgoto_[] =
  {
      -1,     3,     4,     5,    16,   182,     6,    10,    19,    30,
      71,    72,    73,   179,   355,   356,   357,    74,    75,    87,
      11,    20,    21,    32,    85,   185,   525,   362,   728,   780,
     781,   323,   126,   496,    33,    34,   127,   128,   129,   130,
     208,   772,   808,   131,   638,   315,   327,   132,   259,   443,
     581,   679,   133,   754,   755,   707,   631,   732,   733,   665,
     547,   399,   211,   212,   610,    27,    36,   330,   456,   396,
     504,   407,   709,   233,   234,   235,   397,   506,   371,   763,
     372,   804,   318,   764,   238,   239,   765,   240,   398,   805,
     373,   374,   424,   531,   654,   490,   627,   628,   629,   671,
     644,   716,   717,   718,   767,   408,   409,   410,   720,   721,
     722,   773,   400,   401,   465,   466,   467,   135,   267,   268,
     378,   190,   402,   191,   192,   389,   193,   138,   139,   140,
     141,   304,   305,   290,   291,   617,   618,   619,   620,   384,
     385,   459,   600,   601,   602,   690,   691,   201,   202,   203,
     603,   379,   277,   278,   197,   380,   381,   382,   512,   513,
     514,   142,   143,   272,   261,   144,   145,   497,   292,   595,
     241,   242,    76,   243,   774,   157,    78,   244,   245,   498,
     499,   367,   293,   294,   334,   295,   246,   247,   248,   146,
     147,    80,    81,   335,   296,   297,   337,   174,    82,   175,
     149,   150,   299,   300,   151,    24,     9,   310
  };

  const short
  parser::yytable_[] =
  {
      79,   189,    77,   148,   172,   200,   137,   154,   370,   236,
     237,   438,   173,   331,   333,   447,   448,   428,   386,   390,
     288,   288,   288,   345,   503,   391,   287,   287,   287,   403,
     253,   316,   317,   585,   387,   358,   314,   332,   413,   417,
     422,   264,   535,   256,   273,   666,   264,   255,   502,    13,
     712,   530,   604,   306,   280,    22,   584,   298,   308,   298,
     262,   307,   529,   694,   309,   262,   655,   331,   333,    22,
     254,    22,   778,   263,   336,   289,   448,   312,   319,   271,
     314,   609,     1,   325,   453,   469,    22,   470,   257,   640,
     265,   552,   282,   419,   288,   276,   279,   152,   281,   810,
     287,   363,    22,   198,   758,   368,   760,   153,   346,   155,
     649,    18,   364,   449,    22,   249,   800,   282,    12,   544,
      68,   302,   510,   339,    70,   553,   306,   303,   336,   825,
     507,   308,   470,   285,   173,   454,   326,   309,   249,   464,
     641,   759,   505,   759,   553,   420,   425,   742,    79,    79,
      77,    77,   411,   446,   450,   530,    23,   558,   285,   734,
     734,     2,    31,   266,   704,   331,   333,   540,   779,    17,
      23,   800,    23,   800,   472,   637,   642,   282,   148,   148,
     473,   137,   137,   549,   523,   559,   189,    23,   730,   471,
      14,    15,   254,   686,   681,   811,   433,   198,   785,   477,
     426,   756,   756,    23,   699,   390,   302,   541,   172,   249,
     474,    29,   303,   158,   794,    23,   173,   159,   285,   160,
     434,   639,   286,   249,   359,   360,   336,   550,   117,   444,
      25,   180,   118,   181,   700,   316,   317,   725,   451,   249,
     249,   249,   249,   280,   568,   570,   771,    35,   249,   164,
     165,   166,   478,   249,   539,    26,   167,    68,   479,   568,
     460,    70,    37,   276,   768,   339,   480,   540,    38,   255,
     411,   158,   481,    83,   218,   258,   696,   160,   168,   331,
     333,   734,   605,   344,   386,   220,   117,   704,   282,   366,
     756,   493,   494,   621,   769,   770,   655,    84,     7,   164,
     165,   166,     8,   332,   509,   439,   167,   548,   784,   440,
     254,   515,   793,   814,   229,   230,   551,   536,   231,   232,
     549,   785,   744,   745,   329,   794,   549,   517,   168,   285,
     708,   708,   170,   286,   249,   311,   418,   423,   484,   303,
     336,   592,   249,   249,   485,   593,   656,   594,   657,   575,
     771,   578,   578,   249,   331,   333,    68,   195,   561,   196,
      70,    68,   537,   661,   485,    70,   667,   486,    86,   578,
     578,   479,   670,   606,   672,   607,   673,   583,   358,   264,
     288,   249,   288,   586,   615,   288,   622,   288,   287,   448,
     487,   287,   488,   287,   481,   563,   485,   176,   262,   534,
     632,   237,   565,   685,   177,   829,   485,   303,   316,   317,
     582,   219,   178,   444,   221,   336,   587,   298,   463,   298,
     431,   464,   298,  -170,   298,   209,   183,   210,   693,   164,
     165,   166,   586,   320,   321,   623,   167,   624,   692,   184,
     625,  -153,   626,   186,  -153,   249,   249,   457,   194,   458,
     658,   670,   269,   266,   660,   796,   162,   662,   168,   341,
     249,   345,   170,   568,   434,   163,   729,   164,   165,   166,
     204,   205,   206,   207,   167,   663,   198,   664,   689,   705,
     322,   706,   668,   738,   411,   739,   819,   340,   821,   579,
     580,   651,    79,  -451,    77,   342,   168,   169,   343,   283,
     170,   171,   766,   361,   347,   348,   249,   349,   350,   351,
     390,   578,   352,   420,   425,   249,   801,   448,   148,   621,
     749,   137,   254,   365,   254,   282,   328,  -149,   698,   198,
    -149,   388,   392,   393,   394,   395,   164,   165,   166,   404,
     405,   406,   430,   167,   435,   431,   249,   249,   766,   249,
     436,   444,   437,   445,   288,   452,   442,   787,   788,   786,
     287,   329,   677,   455,   475,   168,   285,   249,   476,   170,
     286,   801,   448,   249,   489,   249,   482,   249,  -471,   218,
     483,   795,   255,   766,   491,   766,   414,   326,   344,   518,
     220,   298,   501,   519,   520,   524,   670,   522,   521,   526,
     578,   741,   532,   766,   527,   766,   533,   743,   684,   724,
     534,  -357,   815,   750,   303,   538,   543,   542,   515,   229,
     230,   545,   546,   231,   232,   555,   557,   556,   554,   560,
     562,   568,   148,   148,   564,   137,   137,   571,   573,   117,
     574,   589,   576,   588,   249,   590,   591,   566,   567,   249,
     249,   572,   249,   597,   612,   613,   789,   790,   791,   792,
     614,  -452,   630,   633,   249,   634,   636,   249,   249,   331,
     333,   635,   645,   646,   148,   148,   647,   137,   137,   648,
     803,   237,   675,   676,   678,   682,   282,   680,   683,   687,
     688,   609,   775,   777,   412,   812,   695,   164,   165,   166,
     701,   642,   282,   328,   167,   714,   702,   148,   148,   715,
     137,   137,   719,   164,   165,   166,   824,   237,   726,   727,
     167,   736,   495,   746,   737,   762,   168,   285,   828,   782,
     336,   803,   237,   249,   783,   799,   807,   809,   329,   816,
     822,   823,   168,   285,   353,   775,   170,   286,   826,   650,
      79,   827,    77,   831,   148,   643,   324,   137,   500,   282,
     817,   818,   757,   148,   813,    28,   137,   249,   516,   735,
     164,   165,   166,   710,   249,   830,   249,   167,   429,   249,
      79,   492,    77,   569,   674,   751,   249,   432,   761,   752,
     713,   820,   776,   798,   669,   329,   806,   249,   134,   168,
     285,   508,   611,   170,   286,   136,   313,   731,   747,   528,
     608,   740,   748,   462,   249,   383,   249,   659,   723,   161,
     260,   596,   249,   156,   711,     0,   252,   427,     0,     0,
       0,     0,   249,     0,   249,     0,   249,   249,    88,    39,
      89,    90,    91,     0,    92,     0,    40,    93,     0,     0,
      94,    95,    96,    97,    98,     0,    99,     0,    42,     0,
     100,     0,   101,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,   104,   105,    60,    61,    62,    63,
      64,    65,   106,     0,     0,   282,     0,   107,   108,     0,
       0,     0,     0,     0,     0,     0,   164,   165,   166,     0,
       0,   109,     0,   167,     0,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,   112,   113,     0,     0,     0,
       0,   329,     0,     0,     0,   168,   285,     0,   114,   170,
     286,     0,   115,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,     0,     0,     0,     0,
       0,     0,   123,     0,   124,   125,    88,    39,    89,     0,
     753,     0,    92,     0,    40,    93,     0,     0,    94,    95,
      96,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,   104,   105,    60,    61,    62,    63,    64,    65,
     106,     0,     0,     0,     0,   107,   108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,   113,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     115,     0,   116,     0,     0,     0,     0,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,     0,     0,     0,     0,    88,    39,
      89,     0,   124,   125,    92,     0,    40,    93,     0,     0,
      94,    95,    96,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,   104,   105,    60,    61,    62,    63,
      64,    65,   106,     0,     0,     0,     0,   107,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,   112,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   115,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,     0,     0,    22,     0,
      88,    39,    89,     0,   124,   125,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   107,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,   112,   577,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    23,
     114,     0,     0,     0,   188,     0,   116,     0,     0,     0,
     599,     0,     0,     0,    67,   117,     0,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,     0,   105,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   107,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   282,     0,     0,   110,     0,     0,
       0,     0,     0,   111,     0,   301,   165,   166,     0,     0,
       0,     0,   167,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   188,   302,   116,     0,     0,     0,     0,   303,
     284,     0,    67,   117,   168,   285,    69,   118,   170,   286,
       0,     0,   119,   120,   121,   122,    88,    39,    89,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,   105,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   107,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   282,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,   165,   166,     0,     0,     0,     0,
     167,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     188,     0,   116,   311,     0,     0,     0,   303,   284,     0,
      67,   117,   168,   285,    69,   118,   170,   286,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   107,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     282,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   112,   165,   166,     0,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   114,   283,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,   284,     0,    67,   117,
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
       0,     0,   114,     0,     0,     0,   188,     0,   116,     0,
       0,     0,     0,     0,   284,     0,    67,   117,   168,   285,
      69,   118,   170,   286,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   107,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,   112,   577,     0,
       0,     0,     0,     0,     0,     0,     0,   598,     0,     0,
     114,     0,     0,     0,   188,     0,   116,     0,     0,     0,
     599,     0,     0,     0,    67,   117,     0,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,   102,    51,    52,    53,    54,    55,    56,    57,   103,
       0,    58,     0,     0,     0,   105,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,   107,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,   198,
       0,     0,     0,   111,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,   199,     0,     0,     0,   114,     0,
       0,     0,   188,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,    88,    39,    89,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,   375,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,   376,    58,
       0,     0,     0,   105,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,   107,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,   112,   377,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     188,     0,   116,     0,     0,     0,     0,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   107,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   112,     0,     0,     0,     0,     0,     0,     0,     0,
     457,     0,   458,     0,   114,     0,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,    88,    39,    89,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,   375,     0,
       0,     0,    42,   616,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,     0,   105,
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
       0,     0,     0,     0,     0,   111,     0,   112,   577,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,   188,     0,   116,     0,     0,     0,
     599,     0,     0,     0,    67,   117,     0,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    88,    39,
      89,     0,     0,     0,    92,     0,    40,    93,     0,     0,
       0,     0,     0,     0,   375,     0,     0,     0,    42,     0,
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
     188,     0,   116,     0,     0,     0,     0,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,   107,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,    88,    39,    89,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,   697,     0,     0,     0,   105,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,   107,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   188,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,   105,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,   107,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
       0,   461,     0,   111,     0,     0,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   188,     0,   116,     0,     0,     0,     0,     0,
       0,     0,    67,   117,     0,     0,    69,   118,     0,     0,
       0,     0,   119,   120,   121,   122,    88,    39,   274,     0,
       0,     0,    92,     0,    40,    93,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,    42,     0,     0,     0,
       0,    44,     0,    45,    46,    47,    48,    49,    50,   102,
      51,    52,    53,    54,    55,    56,    57,   103,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,   111,     0,     0,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
     188,     0,   116,     0,     0,     0,     0,     0,     0,     0,
      67,   117,     0,     0,    69,   118,     0,     0,     0,     0,
     119,   120,   121,   122,    88,    39,    89,     0,     0,     0,
      92,     0,    40,    93,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,   102,    51,    52,
      53,    54,    55,    56,    57,   103,     0,    58,     0,     0,
       0,   105,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   188,     0,
     116,     0,     0,     0,     0,     0,     0,     0,    67,   117,
       0,     0,    69,   118,     0,     0,     0,     0,   119,   120,
     121,   122,    88,    39,    89,     0,     0,     0,    92,     0,
      40,    93,     0,     0,     0,     0,     0,     0,    98,     0,
       0,     0,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,   102,    51,    52,    53,    54,
      55,    56,    57,   103,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,     0,     0,     0,     0,     0,   111,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   338,     0,     0,
       0,     0,   114,     0,     0,     0,   188,     0,   116,     0,
       0,     0,     0,     0,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,     0,     0,   119,   120,   121,   122,
      88,    39,    89,     0,     0,     0,    92,     0,    40,    93,
       0,     0,     0,     0,     0,     0,    98,     0,     0,     0,
      42,     0,     0,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,   102,    51,    52,    53,    54,    55,    56,
      57,   103,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   111,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,   188,     0,   116,     0,     0,    39,
       0,     0,     0,     0,    67,   117,    40,     0,    69,   118,
       0,     0,     0,     0,   119,   120,   121,   122,    42,     0,
       0,     0,     0,   369,     0,    45,    46,    47,   213,   214,
     215,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,   217,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   218,     0,     0,     0,     0,     0,
       0,   414,     0,   415,     0,   220,   221,   222,     0,     0,
       0,     0,     0,     0,   223,     0,     0,     0,   224,     0,
       0,    39,   225,   416,   226,     0,     0,     0,    40,   303,
     227,     0,   228,    68,   229,   230,     0,    70,   231,   232,
      42,     0,     0,     0,     0,   369,     0,    45,    46,    47,
     213,   214,   215,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   216,   217,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   218,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,   220,   221,   222,
       0,     0,     0,     0,     0,     0,   223,     0,     0,     0,
     224,     0,     0,    39,   225,     0,   226,   421,     0,     0,
      40,   303,   227,     0,   228,    68,   229,   230,     0,    70,
     231,   232,    42,     0,     0,     0,     0,   369,     0,    45,
      46,    47,   213,   214,   215,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   216,   217,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   218,     0,
       0,     0,     0,     0,     0,     0,     0,   219,     0,   220,
     221,   222,     0,     0,     0,     0,     0,     0,   223,     0,
       0,     0,   224,   412,     0,    39,   225,     0,   226,     0,
       0,     0,    40,     0,   227,     0,   228,    68,   229,   230,
       0,    70,   231,   232,    42,     0,     0,     0,     0,   369,
       0,    45,    46,    47,   213,   214,   215,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,   217,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     218,     0,     0,     0,     0,     0,     0,     0,     0,   219,
       0,   220,   221,   222,     0,     0,     0,     0,     0,     0,
     223,     0,     0,     0,   224,     0,     0,    39,   225,     0,
     226,     0,     0,     0,    40,     0,   227,     0,   228,    68,
     229,   230,     0,    70,   231,   232,    42,     0,     0,     0,
       0,     0,     0,    45,    46,    47,   213,   214,   215,     0,
       0,     0,    53,    54,    55,    56,    57,     0,     0,    58,
       0,     0,     0,     0,    60,    61,    62,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     216,   217,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   218,   637,     0,     0,     0,     0,     0,     0,
       0,   219,     0,   220,   221,   222,     0,     0,     0,     0,
       0,     0,   223,     0,     0,     0,   224,     0,     0,    39,
     225,     0,   226,     0,     0,     0,    40,     0,   227,     0,
     228,    68,   229,   230,     0,    70,   231,   232,    42,     0,
       0,     0,     0,     0,     0,    45,    46,    47,   213,   214,
     215,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,   217,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   218,     0,     0,     0,     0,     0,
       0,     0,     0,   219,     0,   220,   221,   222,     0,     0,
       0,     0,     0,     0,   223,     0,     0,     0,   224,     0,
       0,    39,   225,   802,   226,     0,     0,     0,    40,     0,
     227,     0,   228,    68,   229,   230,     0,    70,   231,   232,
      42,     0,     0,     0,     0,     0,     0,    45,    46,    47,
     213,   214,   215,     0,     0,     0,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   216,   217,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   218,     0,     0,     0,
       0,     0,     0,     0,     0,   219,     0,   220,   221,   222,
       0,     0,     0,     0,     0,     0,   223,     0,     0,     0,
     224,     0,   468,    39,   225,     0,   226,     0,     0,     0,
      40,     0,   227,     0,   228,    68,   229,   230,     0,    70,
     231,   232,    42,     0,     0,     0,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,    39,
      60,    61,    62,    63,    64,    65,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,     0,     0,     0,     0,    45,    46,    47,   213,   214,
     215,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,   217,     0,     0,    67,   117,     0,     0,
      69,   118,     0,     0,   282,     0,     0,     0,     0,     0,
       0,     0,     0,   219,  -171,     0,   221,   222,     0,     0,
       0,     0,    39,     0,   223,     0,     0,     0,   224,    40,
       0,     0,   225,     0,   226,     0,     0,     0,     0,     0,
     446,    42,   228,    68,     0,   285,     0,    70,    45,    46,
      47,   213,   214,   215,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   216,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
       0,     0,     0,     0,     0,     0,   219,     0,     0,   221,
     222,     0,     0,     0,     0,    39,     0,   223,     0,     0,
       0,   224,    40,     0,     0,   225,     0,   226,     0,     0,
       0,     0,     0,   446,    42,   228,    68,     0,   285,     0,
      70,    45,    46,    47,   213,   214,   215,     0,     0,     0,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   216,   217,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   219,
       0,     0,   221,   222,     0,     0,     0,     0,     0,     0,
     223,     0,     0,     0,   224,    39,     0,     0,   225,     0,
     226,     0,    40,     0,     0,     0,     0,     0,   228,    68,
       0,    41,     0,    70,    42,     0,    43,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
      59,    39,    60,    61,    62,    63,    64,    65,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      42,     0,    43,     0,     0,    44,     0,    45,    46,    47,
      48,    49,    50,     0,    51,    52,    53,    54,    55,    56,
      57,     0,     0,    58,     0,     0,     0,     0,    60,    61,
      62,    63,    64,    65,     0,     0,     0,     0,    66,    39,
       0,     0,     0,     0,     0,     0,    40,     0,    67,    68,
       0,     0,    69,    70,     0,   354,     0,     0,    42,     0,
       0,     0,     0,     0,     0,    45,    46,    47,   213,   214,
     215,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,    66,    39,    60,    61,    62,    63,
      64,    65,    40,     0,    67,    68,     0,     0,    69,    70,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,     0,     0,
       0,     0,    39,     0,     0,     0,     0,     0,     0,    40,
       0,     0,   228,    68,     0,     0,     0,    70,     0,   511,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,   270,    60,
      61,    62,    63,    64,    65,     0,     0,     0,    67,    39,
       0,     0,    69,     0,     0,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,   270,    60,    61,    62,    63,
      64,    65,     0,     0,     0,    67,     0,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   164,   165,   166,     0,    39,
       0,     0,   167,     0,     0,     0,    40,     0,     0,     0,
       0,     0,   250,     0,     0,     0,     0,     0,    42,     0,
     251,     0,    67,    44,   168,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    39,    60,    61,    62,    63,
      64,    65,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    42,     0,     0,     0,     0,    44,
       0,    45,    46,    47,    48,    49,    50,     0,    51,    52,
      53,    54,    55,    56,    57,     0,     0,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,   158,    39,
       0,     0,   258,     0,   160,     0,    40,     0,     0,     0,
       0,     0,    67,   117,     0,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,    39,     0,     0,     0,     0,    67,   117,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    42,     0,     0,  -388,     0,    44,     0,    45,
      46,    47,    48,    49,    50,     0,    51,    52,    53,    54,
      55,    56,    57,     0,   441,    58,     0,     0,     0,    39,
      60,    61,    62,    63,    64,    65,    40,     0,     0,   442,
       0,     0,    67,     0,     0,     0,     0,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,    39,    60,    61,    62,    63,
      64,    65,    40,     0,     0,     0,   250,     0,     0,     0,
       0,     0,     0,     0,    42,     0,    67,     0,     0,     0,
       0,    45,    46,    47,   213,   214,   215,     0,     0,    39,
      53,    54,    55,    56,    57,     0,    40,    58,     0,     0,
       0,     0,    60,    61,    62,    63,    64,    65,    42,     0,
       0,     0,     0,     0,     0,    45,    46,    47,   213,   214,
     215,     0,    67,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,   652,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   653,    39,
       0,     0,     0,     0,     0,     0,    40,     0,   228,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    42,     0,
       0,     0,   797,     0,     0,    45,    46,    47,   213,   214,
     215,     0,   653,     0,    53,    54,    55,    56,    57,     0,
       0,    58,   228,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,    39,     0,     0,     0,
       0,     0,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,     0,     0,
       0,   703,    45,    46,    47,   213,   214,   215,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,   228,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   228
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,    34,    66,    93,    34,    41,   194,   100,
     100,   258,    66,   136,   136,   263,   263,   233,   198,   201,
     114,   115,   116,   155,   328,   201,   114,   115,   116,   210,
     101,   123,   123,   445,   199,   178,   116,   136,   224,   225,
     226,   104,   375,   102,   109,   549,   109,   101,   327,     5,
     637,   367,   458,   115,   112,     1,   444,   114,   115,   116,
     104,   115,   366,   607,   115,   109,   531,   190,   190,     1,
     101,     1,    11,   104,   136,   114,   323,   116,    18,   108,
     160,    26,    20,    18,    77,   284,     1,   284,   103,    77,
     105,    80,    79,   225,   188,   110,   111,   104,   113,   779,
     188,   117,     1,    84,    77,   193,    77,   114,   159,   107,
      80,   113,   128,    88,     1,   100,   764,    79,     0,   100,
     118,   108,   338,   138,   122,   114,   188,   114,   190,   809,
     329,   188,   329,   120,   188,   128,    71,   188,   123,    84,
     128,   114,   328,   114,   114,   225,   226,   691,   177,   178,
     177,   178,   223,   115,   129,   471,   102,    80,   120,   663,
     664,    99,    13,   103,   629,   288,   288,   113,   107,   100,
     102,   819,   102,   821,    78,    80,    81,    79,   209,   210,
      84,   209,   210,   113,   360,   108,   274,   102,   653,   288,
      65,    66,   223,   599,   582,   782,   250,    84,   113,   293,
     227,   705,   706,   102,    84,   387,   108,   383,   270,   194,
     114,   107,   114,   103,   113,   102,   270,   107,   120,   109,
     251,   500,   124,   208,   180,   181,   288,   403,   118,   260,
     103,    99,   122,   101,   114,   327,   327,   649,   267,   224,
     225,   226,   227,   301,   430,   431,   107,   128,   233,    90,
      91,    92,   108,   238,   100,   128,    97,   118,   114,   445,
     275,   122,    77,   278,    21,   280,   108,   113,    77,   323,
     341,   103,   114,    26,    79,   107,   609,   109,   119,   402,
     402,   785,   458,    88,   464,    90,   118,   752,    79,    80,
     794,   320,   321,   473,    51,    52,   761,    60,   118,    90,
      91,    92,   122,   402,   332,    81,    97,   100,   100,    85,
     341,   340,   100,   100,   119,   120,   404,   376,   123,   124,
     113,   113,    47,    48,   115,   113,   113,   342,   119,   120,
     634,   635,   123,   124,   319,   110,   225,   226,   108,   114,
     402,   103,   327,   328,   114,   107,   532,   109,   534,   437,
     107,   439,   440,   338,   477,   477,   118,    99,   108,   101,
     122,   118,   377,   543,   114,   122,   552,   110,   113,   457,
     458,   114,   558,   461,   560,   463,   562,   442,   521,   442,
     474,   366,   476,   446,   472,   479,   474,   481,   476,   636,
     110,   479,   110,   481,   114,   110,   114,   108,   442,   114,
     491,   491,   110,   110,   114,   817,   114,   114,   500,   500,
     441,    88,   107,   444,    91,   477,   447,   474,    81,   476,
      86,    84,   479,    89,   481,    99,    77,   101,   604,    90,
      91,    92,   495,   124,   125,   474,    97,   476,   603,    33,
     479,    81,   481,   113,    84,   430,   431,    99,   107,   101,
     538,   637,   128,   103,   542,   759,    79,   545,   119,   114,
     445,   593,   123,   649,   495,    88,   652,    90,    91,    92,
      72,    73,    74,    75,    97,    99,    84,   101,    86,    99,
     129,   101,   553,    99,   555,   101,   799,    99,   801,   439,
     440,   525,   521,   114,   521,    87,   119,   120,   128,   104,
     123,   124,   718,    23,   108,   108,   491,   108,   108,   108,
     692,   599,   108,   593,   594,   500,   764,   764,   549,   699,
     700,   549,   553,    22,   555,    79,    80,    81,   616,    84,
      84,   113,    77,    77,    77,    77,    90,    91,    92,    14,
     138,   138,    81,    97,   129,    86,   531,   532,   764,   534,
     129,   582,    86,    80,   648,    77,   114,   738,   739,   735,
     648,   115,   577,    80,   104,   119,   120,   552,   114,   123,
     124,   819,   819,   558,    84,   560,   108,   562,   108,    79,
     108,   757,   636,   799,    89,   801,    86,    71,    88,   108,
      90,   648,   128,   108,   108,   128,   782,   100,   114,    77,
     688,   689,    89,   819,    77,   821,   108,   695,   108,   648,
     114,    85,   788,   701,   114,    85,   114,    86,   647,   119,
     120,    24,    26,   123,   124,   114,   108,   104,   100,   114,
      84,   817,   663,   664,   110,   663,   664,   108,   128,   118,
     128,   104,   108,   129,   629,    77,    77,   115,   115,   634,
     635,   115,   637,    77,   115,   115,   744,   745,   746,   753,
     115,   114,    26,    77,   649,    80,   114,   652,   653,   792,
     792,    80,   115,   100,   705,   706,   114,   705,   706,    81,
     771,   771,   128,   128,    26,   115,    79,   100,   104,   100,
     113,    26,   719,   727,   104,   783,    81,    90,    91,    92,
      78,    81,    79,    80,    97,    77,   114,   738,   739,    28,
     738,   739,     8,    90,    91,    92,   807,   807,   114,     4,
      97,   108,   115,    47,   108,    84,   119,   120,   816,    80,
     792,   822,   822,   718,    10,    89,    53,   107,   115,    48,
     114,   108,   119,   120,   177,   772,   123,   124,   108,   521,
     779,   108,   779,   108,   785,   502,   127,   785,   325,    79,
      80,   794,   706,   794,   785,    15,   794,   752,   341,   664,
      90,    91,    92,   635,   759,   822,   761,    97,   235,   764,
     809,   319,   809,   430,   562,   702,   771,   238,   715,   703,
     639,   799,   721,   762,   555,   115,   772,   782,    34,   119,
     120,   330,   466,   123,   124,    34,   116,   662,   699,   365,
     464,   688,   700,   278,   799,   196,   801,   540,   647,    59,
     104,   455,   807,    43,   636,    -1,   101,   227,    -1,    -1,
      -1,    -1,   817,    -1,   819,    -1,   821,   822,     3,     4,
       5,     6,     7,    -1,     9,    -1,    11,    12,    -1,    -1,
      15,    16,    17,    18,    19,    -1,    21,    -1,    23,    -1,
      25,    -1,    27,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    79,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    92,    -1,
      -1,    76,    -1,    97,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,   115,    -1,    -1,    -1,   119,   120,    -1,   103,   123,
     124,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,    -1,    -1,    -1,    -1,
      -1,    -1,   137,    -1,   139,   140,     3,     4,     5,    -1,
       7,    -1,     9,    -1,    11,    12,    -1,    -1,    15,    16,
      17,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,    -1,    -1,    -1,    -1,     3,     4,
       5,    -1,   139,   140,     9,    -1,    11,    12,    -1,    -1,
      15,    16,    17,    -1,    19,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,    -1,    -1,     1,    -1,
       3,     4,     5,    -1,   139,   140,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
     113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,     4,
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
      -1,    -1,   107,   108,   109,    -1,    -1,    -1,    -1,   114,
     115,    -1,   117,   118,   119,   120,   121,   122,   123,   124,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      97,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,   110,    -1,    -1,    -1,   114,   115,    -1,
     117,   118,   119,   120,   121,   122,   123,   124,    -1,    -1,
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
      -1,    -1,    -1,    -1,   103,   104,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,   115,    -1,   117,   118,
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
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,   115,    -1,   117,   118,   119,   120,
     121,   122,   123,   124,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
     113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    84,
      -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,
      -1,    -1,   127,   128,   129,   130,     3,     4,     5,    -1,
      -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    28,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,   101,    -1,   103,    -1,    -1,    -1,   107,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,
     129,   130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,
      11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      -1,    -1,    23,    24,    -1,    -1,    -1,    28,    -1,    30,
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
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,
     113,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,     4,
       5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,
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
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    86,    -1,    88,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
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
      -1,    88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
     107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,
     127,   128,   129,   130,     3,     4,     5,    -1,    -1,    -1,
       9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    -1,    -1,
      -1,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,
       3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,     4,
      -1,    -1,    -1,    -1,   117,   118,    11,    -1,   121,   122,
      -1,    -1,    -1,    -1,   127,   128,   129,   130,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    86,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
      -1,     4,   107,   108,   109,    -1,    -1,    -1,    11,   114,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,    -1,     4,   107,    -1,   109,   110,    -1,    -1,
      11,   114,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      41,    42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,
      91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,
      -1,    -1,   103,   104,    -1,     4,   107,    -1,   109,    -1,
      -1,    -1,    11,    -1,   115,    -1,   117,   118,   119,   120,
      -1,   122,   123,   124,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,    -1,
     109,    -1,    -1,    -1,    11,    -1,   115,    -1,   117,   118,
     119,   120,    -1,   122,   123,   124,    23,    -1,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    -1,
      -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    79,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,   103,    -1,    -1,     4,
     107,    -1,   109,    -1,    -1,    -1,    11,    -1,   115,    -1,
     117,   118,   119,   120,    -1,   122,   123,   124,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,
      -1,     4,   107,   108,   109,    -1,    -1,    -1,    11,    -1,
     115,    -1,   117,   118,   119,   120,    -1,   122,   123,   124,
      23,    -1,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,     3,     4,   107,    -1,   109,    -1,    -1,    -1,
      11,    -1,   115,    -1,   117,   118,   119,   120,    -1,   122,
     123,   124,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      41,    42,    43,    -1,    -1,    46,    -1,    -1,    -1,     4,
      51,    52,    53,    54,    55,    56,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,   117,   118,    -1,    -1,
     121,   122,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    89,    -1,    91,    92,    -1,    -1,
      -1,    -1,     4,    -1,    99,    -1,    -1,    -1,   103,    11,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
     115,    23,   117,   118,    -1,   120,    -1,   122,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,     4,    -1,    99,    -1,    -1,
      -1,   103,    11,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,   115,    23,   117,   118,    -1,   120,    -1,
     122,    30,    31,    32,    33,    34,    35,    -1,    -1,    -1,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      99,    -1,    -1,    -1,   103,     4,    -1,    -1,   107,    -1,
     109,    -1,    11,    -1,    -1,    -1,    -1,    -1,   117,   118,
      -1,    20,    -1,   122,    23,    -1,    25,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      49,     4,    51,    52,    53,    54,    55,    56,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,    25,    -1,    -1,    28,    -1,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,   107,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,   117,   118,
      -1,    -1,   121,   122,    -1,    78,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,   107,     4,    51,    52,    53,    54,
      55,    56,    11,    -1,   117,   118,    -1,    -1,   121,   122,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,   117,   118,    -1,    -1,    -1,   122,    -1,    78,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,   107,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,   117,     4,
      -1,    -1,   121,    -1,    -1,    -1,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,   107,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,   117,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    91,    92,    -1,     4,
      -1,    -1,    97,    -1,    -1,    -1,    11,    -1,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    23,    -1,
     115,    -1,   117,    28,   119,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,     4,    51,    52,    53,    54,
      55,    56,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,
      -1,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,   103,     4,
      -1,    -1,   107,    -1,   109,    -1,    11,    -1,    -1,    -1,
      -1,    -1,   117,   118,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,     4,    -1,    -1,    -1,    -1,   117,   118,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    -1,    -1,    80,    -1,    28,    -1,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      41,    42,    43,    -1,    99,    46,    -1,    -1,    -1,     4,
      51,    52,    53,    54,    55,    56,    11,    -1,    -1,   114,
      -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,     4,    51,    52,    53,    54,
      55,    56,    11,    -1,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,   117,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    -1,    -1,     4,
      39,    40,    41,    42,    43,    -1,    11,    46,    -1,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,   117,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    97,    -1,    -1,    30,    31,    32,    33,    34,
      35,    -1,   107,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,   117,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      -1,    86,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117
  };

  const unsigned short
  parser::yystos_[] =
  {
       0,    20,    99,   142,   143,   144,   147,   118,   122,   347,
     148,   161,     0,   148,    65,    66,   145,   100,   113,   149,
     162,   163,     1,   102,   346,   103,   128,   206,   206,   107,
     150,    13,   164,   175,   176,   128,   207,    77,    77,     4,
      11,    20,    23,    25,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    41,    42,    43,    46,    49,
      51,    52,    53,    54,    55,    56,   107,   117,   118,   121,
     122,   151,   152,   153,   158,   159,   313,   316,   317,   331,
     332,   333,   339,    26,    60,   165,   113,   160,     3,     5,
       6,     7,     9,    12,    15,    16,    17,    18,    19,    21,
      25,    27,    36,    44,    49,    50,    57,    62,    63,    76,
      82,    88,    90,    91,   103,   107,   109,   118,   122,   127,
     128,   129,   130,   137,   139,   140,   173,   177,   178,   179,
     180,   184,   188,   193,   253,   258,   263,   264,   268,   269,
     270,   271,   302,   303,   306,   307,   330,   331,   333,   341,
     342,   345,   104,   114,   347,   107,   312,   316,   103,   107,
     109,   302,    79,    88,    90,    91,    92,    97,   119,   120,
     123,   124,   336,   337,   338,   340,   108,   114,   107,   154,
      99,   101,   146,    77,    33,   166,   113,    63,   107,   261,
     262,   264,   265,   267,   107,    99,   101,   295,    84,    99,
     261,   288,   289,   290,    72,    73,    74,    75,   181,    99,
     101,   203,   204,    33,    34,    35,    67,    68,    79,    88,
      90,    91,    92,    99,   103,   107,   109,   115,   117,   119,
     120,   123,   124,   214,   215,   216,   221,   223,   225,   226,
     228,   311,   312,   314,   318,   319,   327,   328,   329,   339,
     107,   115,   321,   330,   333,   337,   295,   269,   107,   189,
     304,   305,   307,   333,   342,   269,   103,   259,   260,   128,
     107,   331,   304,   305,     5,    91,   269,   293,   294,   269,
     268,   269,    79,   104,   115,   120,   124,   261,   262,   272,
     274,   275,   309,   323,   324,   326,   335,   336,   338,   343,
     344,    90,   108,   114,   272,   273,   336,   337,   338,   343,
     348,   110,   272,   273,   348,   186,   219,   221,   223,    18,
     259,   259,   129,   172,   160,    18,    71,   187,    80,   115,
     208,   309,   322,   324,   325,   334,   336,   337,    98,   269,
      99,   114,    87,   128,    88,   318,   343,   108,   108,   108,
     108,   108,   108,   153,    78,   155,   156,   157,   158,   148,
     148,    23,   168,   117,   128,    22,    80,   322,   261,    28,
     217,   219,   221,   231,   232,    19,    45,    91,   261,   292,
     296,   297,   298,   296,   280,   281,   298,   288,   113,   266,
     290,   346,    77,    77,    77,    77,   210,   217,   229,   202,
     253,   254,   263,   202,    14,   138,   138,   212,   246,   247,
     248,   330,   104,   217,    86,    88,   108,   217,   232,   318,
     348,   110,   217,   232,   233,   348,   316,   329,   228,   215,
      81,    86,   226,   337,   333,   129,   129,    86,   344,    81,
      85,    99,   114,   190,   333,    80,   115,   308,   344,    88,
     129,   331,    77,    77,   128,    80,   209,    99,   101,   282,
     269,    86,   293,    81,    84,   255,   256,   257,     3,   332,
     341,   322,    78,    84,   114,   104,   114,   262,   108,   114,
     108,   114,   108,   108,   108,   114,   110,   110,   110,    84,
     236,    89,   229,   331,   331,   115,   174,   308,   320,   321,
     187,   128,   186,   210,   211,   217,   218,   332,   255,   264,
     228,    78,   299,   300,   301,   331,   212,   269,   108,   108,
     108,   114,   100,   346,   128,   167,    77,    77,   282,   210,
     265,   234,    89,   108,   114,   204,   295,   269,    85,   100,
     113,   346,    86,   114,   100,    24,    26,   201,   100,   113,
     346,   261,    80,   114,   100,   114,   104,   108,    80,   108,
     114,   108,    84,   110,   110,   110,   115,   115,   217,   218,
     217,   108,   115,   128,   128,   261,   108,    91,   261,   291,
     291,   191,   333,   305,   190,   211,   342,   333,   129,   104,
      77,    77,   103,   107,   109,   310,   311,    77,   100,   113,
     283,   284,   285,   291,   283,   346,   261,   261,   280,    26,
     205,   257,   115,   115,   115,   261,    24,   276,   277,   278,
     279,   298,   261,   272,   272,   272,   272,   237,   238,   239,
      26,   197,   221,    77,    80,    80,   114,    80,   185,   186,
      77,   128,    81,   185,   241,   115,   100,   114,    81,    80,
     157,   347,    97,   107,   235,   327,   217,   217,   261,   297,
     261,   298,   261,    99,   101,   200,   254,   217,   330,   248,
     217,   240,   217,   217,   233,   128,   128,   269,    26,   192,
     100,   190,   115,   104,   108,   110,   283,   100,   113,    86,
     286,   287,   288,   346,   205,    81,   204,    46,   261,    84,
     114,    78,   114,    86,   327,    99,   101,   196,   210,   213,
     213,   320,   240,   241,    77,    28,   242,   243,   244,     8,
     249,   250,   251,   300,   272,   211,   114,     4,   169,   217,
     327,   266,   198,   199,   254,   199,   108,   108,    99,   101,
     285,   261,   205,   261,    47,    48,    47,   277,   279,   298,
     261,   238,   239,     7,   194,   195,   254,   195,    77,   114,
      77,   234,    84,   220,   224,   227,   228,   245,    21,    51,
      52,   107,   182,   252,   315,   316,   251,   347,    11,   107,
     170,   171,    80,    10,   100,   113,   346,   202,   202,   261,
     261,   261,   262,   100,   113,   346,   210,    97,   243,    89,
     227,   308,   108,   221,   222,   230,   252,    53,   183,   107,
     151,   240,   261,   198,   100,   346,    48,    80,   194,   224,
     245,   224,   114,   108,   221,   151,   108,   108,   261,   211,
     230,   108
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
     274,   275,   275,   276,   277,   277,   278,   278,   278,   278,
     279,   279,   279,   279,   280,   281,   281,   282,   282,   282,
     282,   283,   283,   284,   284,   284,   285,   286,   287,   287,
     288,   288,   289,   289,   290,   291,   291,   292,   292,   293,
     293,   294,   294,   295,   295,   296,   296,   296,   296,   297,
     297,   298,   298,   298,   299,   299,   300,   300,   300,   301,
     301,   302,   302,   303,   303,   304,   304,   304,   305,   305,
     306,   306,   306,   306,   307,   307,   308,   308,   309,   309,
     310,   310,   310,   311,   311,   311,   311,   311,   312,   312,
     312,   313,   313,   313,   313,   313,   314,   314,   315,   316,
     316,   317,   318,   318,   318,   319,   319,   319,   319,   320,
     320,   321,   321,   322,   322,   322,   323,   323,   323,   324,
     325,   325,   326,   326,   327,   328,   329,   329,   329,   329,
     329,   330,   330,   331,   331,   331,   332,   332,   333,   333,
     333,   333,   333,   333,   333,   333,   334,   334,   335,   335,
     336,   337,   337,   338,   338,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   340,   340,   340,   341,   341,   342,   343,
     343,   344,   344,   345,   345,   345,   345,   346,   346,   347,
     347,   348,   348
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
       3,     3,     3,     1,     3,     1,     3,     3,     1,     1,
       2,     4,     4,     6,     1,     3,     1,     3,     3,     2,
       2,     1,     2,     3,     2,     1,     2,     2,     2,     1,
       2,     1,     3,     2,     4,     1,     2,     1,     2,     1,
       2,     2,     1,     3,     3,     3,     2,     1,     0,     1,
       2,     3,     1,     2,     1,     0,     3,     1,     1,     3,
       1,     1,     1,     1,     3,     1,     3,     1,     1,     3,
       2,     3,     2,     3,     1,     2,     1,     3,     1,     3,
       1,     2,     2,     1,     3,     3,     3,     2,     1,     3,
       3,     1,     3,     3,     3,     3,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     1,     3,
       1,     3,     1,     3,     1,     3,     1,     1,     1,     1,
       1,     1,     3,     1,     3,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1
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
  "aexp", "aexp1", "aexp2", "texp", "tup_exprs", "list", "lexps",
  "flattenedpquals", "pquals", "squals", "transformqual", "guardquals",
  "guardquals1", "altslist", "alts", "alts1", "alt", "alt_rhs", "ralt",
  "gdpats", "ifgdpats", "gdpat", "pat", "bindpat", "apat", "apats1",
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

#if YYDEBUG
  const unsigned short
  parser::yyrline_[] =
  {
       0,   502,   502,   519,   520,   522,   526,   527,   528,   530,
     531,   533,   534,   537,   539,   540,   541,   549,   550,   552,
     554,   555,   557,   558,   559,   561,   562,   564,   565,   567,
     568,   570,   571,   573,   574,   576,   577,   581,   582,   584,
     585,   587,   589,   590,   592,   599,   600,   602,   603,   605,
     606,   608,   609,   611,   612,   614,   615,   617,   618,   623,
     624,   626,   627,   628,   630,   631,   635,   637,   638,   640,
     641,   642,   645,   652,   653,   654,   655,   656,   657,   659,
     661,   663,   664,   667,   669,   670,   672,   673,   674,   675,
     676,   678,   679,   680,   682,   722,   723,   725,   726,   735,
     736,   738,   739,   740,   757,   758,   759,   761,   762,   763,
     765,   766,   768,   770,   771,   774,   778,   779,   781,   782,
     783,   784,   786,   787,   789,   790,   792,   794,   795,   796,
     797,   799,   800,   802,   803,   805,   806,   807,   808,   810,
     811,   813,   819,   820,   828,   829,   831,   832,   833,   841,
     842,   844,   845,   847,   849,   851,   852,   854,   855,   859,
     860,   861,   863,   864,   866,   867,   869,   870,   872,   874,
     883,   885,   887,   888,   890,   893,   895,   896,   898,   899,
     901,   902,   903,   909,   911,   912,   913,   914,   915,   916,
     917,   918,   919,   920,   921,   922,   923,   926,   928,   929,
     931,   932,   934,   935,   937,   938,   940,   941,   943,   944,
     946,   947,   949,   950,   952,   954,   955,   959,   965,   967,
     968,   970,   971,   973,   974,   976,   977,   979,   980,   982,
     983,   985,   987,   988,   990,   991,   993,   994,   995,   997,
     998,   999,  1004,  1005,  1007,  1008,  1011,  1014,  1015,  1017,
    1018,  1020,  1022,  1023,  1024,  1025,  1026,  1027,  1028,  1029,
    1030,  1031,  1032,  1034,  1035,  1037,  1038,  1042,  1043,  1045,
    1046,  1048,  1049,  1051,  1052,  1053,  1055,  1056,  1059,  1060,
    1062,  1063,  1067,  1068,  1069,  1070,  1072,  1073,  1074,  1075,
    1076,  1077,  1078,  1079,  1080,  1081,  1082,  1083,  1085,  1086,
    1088,  1089,  1090,  1091,  1092,  1093,  1094,  1095,  1096,  1101,
    1102,  1103,  1108,  1109,  1127,  1128,  1129,  1130,  1131,  1132,
    1133,  1135,  1136,  1141,  1143,  1144,  1146,  1147,  1148,  1149,
    1151,  1152,  1153,  1154,  1157,  1159,  1160,  1163,  1164,  1165,
    1166,  1168,  1169,  1171,  1172,  1173,  1175,  1177,  1179,  1180,
    1182,  1183,  1185,  1186,  1188,  1190,  1191,  1193,  1194,  1196,
    1197,  1199,  1200,  1203,  1204,  1206,  1207,  1208,  1209,  1214,
    1215,  1217,  1218,  1219,  1224,  1225,  1227,  1228,  1229,  1231,
    1232,  1264,  1265,  1267,  1268,  1270,  1271,  1272,  1274,  1275,
    1277,  1278,  1279,  1280,  1282,  1283,  1285,  1286,  1288,  1289,
    1292,  1293,  1294,  1296,  1297,  1298,  1299,  1300,  1302,  1303,
    1304,  1306,  1307,  1308,  1309,  1310,  1313,  1314,  1316,  1318,
    1319,  1323,  1325,  1326,  1327,  1329,  1330,  1331,  1332,  1337,
    1338,  1340,  1341,  1343,  1344,  1345,  1347,  1348,  1349,  1351,
    1353,  1354,  1356,  1357,  1361,  1363,  1365,  1366,  1367,  1368,
    1369,  1372,  1373,  1375,  1376,  1377,  1379,  1380,  1382,  1383,
    1384,  1385,  1386,  1387,  1388,  1389,  1391,  1392,  1394,  1395,
    1397,  1399,  1400,  1402,  1403,  1405,  1406,  1407,  1408,  1409,
    1410,  1411,  1412,  1413,  1414,  1415,  1416,  1417,  1418,  1419,
    1420,  1421,  1422,  1424,  1425,  1426,  1430,  1431,  1433,  1435,
    1436,  1438,  1439,  1443,  1444,  1445,  1446,  1451,  1454,  1458,
    1459,  1461,  1462
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
#line 5808 "parser.cc" // lalr1.cc:1181
#line 1471 "parser.y" // lalr1.cc:1182


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
    return new expression(AST_node("neg"),{exp});
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
    return new expression(AST_node("id",tuple_head(tup_exprs.size()).name()),tup_exprs);
}


expression_ref make_list(const vector<expression_ref>& pquals)
{
    expression_ref L = AST_node("id","[]");
    expression_ref cons = AST_node("id",":");
    for(int i=pquals.size()-1;i>=0;i--)
	L = {cons,pquals[i],L};
    return L;
}

expression_ref make_flattenedpquals(const vector<expression_ref>& pquals)
{
    if (pquals.size() == 1)
	return pquals[0];
    else
	return new expression(AST_node("ParQuals"),pquals);
}

expression_ref make_squals(const vector<expression_ref>& squals)
{
    return new expression(AST_node("SQuals"),squals);
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

