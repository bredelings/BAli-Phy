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
#line 75 "parser.y" // lalr1.cc:416

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
      case 170: // maybeimpspec
      case 171: // impspec
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
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.copy< int > (that.value);
        break;

      case 172: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< std::optional<std::string> > (that.value);
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
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
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
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
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
      case 170: // maybeimpspec
      case 171: // impspec
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
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.move< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.move< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.move< int > (that.value);
        break;

      case 172: // prec
        value.move< std::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.move< std::optional<std::string> > (that.value);
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
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
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
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
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
      case 170: // maybeimpspec
      case 171: // impspec
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
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        value.copy< expression_ref > (that.value);
        break;

      case 135: // "PRIMFLOAT"
        value.copy< float > (that.value);
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        value.copy< int > (that.value);
        break;

      case 172: // prec
        value.copy< std::optional<int> > (that.value);
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        value.copy< std::optional<std::string> > (that.value);
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
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
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
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
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
      case 170: // maybeimpspec
      case 171: // impspec
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
      case 286: // ifgdpats
      case 287: // gdpat
      case 288: // pat
      case 289: // bindpat
      case 290: // apat
      case 294: // stmt
      case 295: // qual
      case 342: // literal
        yylhs.value.build< expression_ref > ();
        break;

      case 135: // "PRIMFLOAT"
        yylhs.value.build< float > ();
        break;

      case 129: // "INTEGER"
      case 133: // "PRIMINTEGER"
      case 134: // "PRIMWORD"
      case 345: // commas
        yylhs.value.build< int > ();
        break;

      case 172: // prec
        yylhs.value.build< std::optional<int> > ();
        break;

      case 167: // maybe_pkg
      case 169: // maybeas
        yylhs.value.build< std::optional<std::string> > ();
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
      case 299: // qcon
      case 300: // gen_qcon
      case 301: // con
      case 303: // sysdcon_no_list
      case 304: // sysdcon
      case 305: // conop
      case 306: // qconop
      case 307: // gtycon
      case 308: // ntgtycon
      case 309: // oqtycon
      case 310: // oqtycon_no_varcon
      case 311: // qtyconop
      case 312: // qtycondoc
      case 313: // qtycon
      case 314: // tycon
      case 315: // qtyconsym
      case 316: // tyconsym
      case 317: // op
      case 318: // varop
      case 319: // qop
      case 320: // qopm
      case 321: // hole_op
      case 322: // qvarop
      case 323: // qvaropm
      case 324: // tyvar
      case 325: // tyvarop
      case 326: // tyvarid
      case 327: // var
      case 328: // qvar
      case 329: // qvarid
      case 330: // varid
      case 331: // qvarsym
      case 332: // qvarsym_no_minus
      case 333: // qvarsym1
      case 334: // varsym
      case 335: // varsym_no_minus
      case 336: // special_id
      case 337: // special_sym
      case 338: // qconid
      case 339: // conid
      case 340: // qconsym
      case 341: // consym
      case 344: // modid
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
      case 285: // gdpats
      case 291: // apats1
      case 292: // stmtlist
      case 293: // stmts
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
#line 499 "parser.y" // lalr1.cc:870
    {drv.result = yystack_[0].value.as< expression_ref > ();}
#line 1358 "parser.cc" // lalr1.cc:870
    break;

  case 3:
#line 516 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_module(yystack_[4].value.as< std::string > (),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1364 "parser.cc" // lalr1.cc:870
    break;

  case 4:
#line 517 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_module("Main",{},yystack_[0].value.as< expression_ref > ());}
#line 1370 "parser.cc" // lalr1.cc:870
    break;

  case 5:
#line 519 "parser.y" // lalr1.cc:870
    {drv.push_module_context();}
#line 1376 "parser.cc" // lalr1.cc:870
    break;

  case 9:
#line 527 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1382 "parser.cc" // lalr1.cc:870
    break;

  case 10:
#line 528 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1388 "parser.cc" // lalr1.cc:870
    break;

  case 11:
#line 530 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1394 "parser.cc" // lalr1.cc:870
    break;

  case 12:
#line 531 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1400 "parser.cc" // lalr1.cc:870
    break;

  case 13:
#line 534 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1406 "parser.cc" // lalr1.cc:870
    break;

  case 14:
#line 536 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1412 "parser.cc" // lalr1.cc:870
    break;

  case 15:
#line 537 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1418 "parser.cc" // lalr1.cc:870
    break;

  case 16:
#line 538 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_body(yystack_[0].value.as< std::vector<expression_ref> > (),{});}
#line 1424 "parser.cc" // lalr1.cc:870
    break;

  case 17:
#line 546 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_exports(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1430 "parser.cc" // lalr1.cc:870
    break;

  case 18:
#line 547 "parser.y" // lalr1.cc:870
    {}
#line 1436 "parser.cc" // lalr1.cc:870
    break;

  case 19:
#line 549 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1442 "parser.cc" // lalr1.cc:870
    break;

  case 20:
#line 551 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1448 "parser.cc" // lalr1.cc:870
    break;

  case 21:
#line 552 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1454 "parser.cc" // lalr1.cc:870
    break;

  case 22:
#line 554 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 1460 "parser.cc" // lalr1.cc:870
    break;

  case 23:
#line 555 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("module",yystack_[0].value.as< std::string > ());}
#line 1466 "parser.cc" // lalr1.cc:870
    break;

  case 24:
#line 556 "parser.y" // lalr1.cc:870
    {}
#line 1472 "parser.cc" // lalr1.cc:870
    break;

  case 27:
#line 561 "parser.y" // lalr1.cc:870
    {}
#line 1478 "parser.cc" // lalr1.cc:870
    break;

  case 28:
#line 562 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1484 "parser.cc" // lalr1.cc:870
    break;

  case 29:
#line 564 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[3].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ());}
#line 1490 "parser.cc" // lalr1.cc:870
    break;

  case 30:
#line 565 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1496 "parser.cc" // lalr1.cc:870
    break;

  case 31:
#line 567 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1502 "parser.cc" // lalr1.cc:870
    break;

  case 32:
#line 568 "parser.y" // lalr1.cc:870
    {}
#line 1508 "parser.cc" // lalr1.cc:870
    break;

  case 33:
#line 570 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1514 "parser.cc" // lalr1.cc:870
    break;

  case 34:
#line 571 "parser.y" // lalr1.cc:870
    {}
#line 1520 "parser.cc" // lalr1.cc:870
    break;

  case 35:
#line 573 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("qvar",yystack_[0].value.as< std::string > ()); }
#line 1526 "parser.cc" // lalr1.cc:870
    break;

  case 36:
#line 574 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("qvar",yystack_[0].value.as< std::string > ()); }
#line 1532 "parser.cc" // lalr1.cc:870
    break;

  case 41:
#line 584 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()), yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ()); }
#line 1538 "parser.cc" // lalr1.cc:870
    break;

  case 42:
#line 586 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ()); }
#line 1544 "parser.cc" // lalr1.cc:870
    break;

  case 43:
#line 587 "parser.y" // lalr1.cc:870
    { }
#line 1550 "parser.cc" // lalr1.cc:870
    break;

  case 44:
#line 589 "parser.y" // lalr1.cc:870
    {
    std::vector<expression_ref> e;
    if (yystack_[4].value.as< bool > ()) e.push_back(AST_node("qualified"));
    e.push_back(String(yystack_[2].value.as< std::string > ()));
    if (yystack_[1].value.as< std::optional<std::string> > ()) e.push_back(AST_node("as", *yystack_[1].value.as< std::optional<std::string> > ()));
    if (yystack_[0].value.as< expression_ref > ()) e.push_back(yystack_[0].value.as< expression_ref > ());
    yylhs.value.as< expression_ref > () = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}
#line 1563 "parser.cc" // lalr1.cc:870
    break;

  case 45:
#line 598 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1569 "parser.cc" // lalr1.cc:870
    break;

  case 46:
#line 599 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1575 "parser.cc" // lalr1.cc:870
    break;

  case 47:
#line 601 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1581 "parser.cc" // lalr1.cc:870
    break;

  case 48:
#line 602 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1587 "parser.cc" // lalr1.cc:870
    break;

  case 49:
#line 604 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::optional<std::string> > () = yystack_[0].value.as< std::string > (); }
#line 1593 "parser.cc" // lalr1.cc:870
    break;

  case 50:
#line 605 "parser.y" // lalr1.cc:870
    { }
#line 1599 "parser.cc" // lalr1.cc:870
    break;

  case 51:
#line 607 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = true; }
#line 1605 "parser.cc" // lalr1.cc:870
    break;

  case 52:
#line 608 "parser.y" // lalr1.cc:870
    { yylhs.value.as< bool > () = false; }
#line 1611 "parser.cc" // lalr1.cc:870
    break;

  case 53:
#line 610 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::optional<std::string> > () = yystack_[0].value.as< std::string > (); }
#line 1617 "parser.cc" // lalr1.cc:870
    break;

  case 54:
#line 611 "parser.y" // lalr1.cc:870
    { }
#line 1623 "parser.cc" // lalr1.cc:870
    break;

  case 55:
#line 613 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = yystack_[0].value.as< expression_ref > (); }
#line 1629 "parser.cc" // lalr1.cc:870
    break;

  case 56:
#line 614 "parser.y" // lalr1.cc:870
    { }
#line 1635 "parser.cc" // lalr1.cc:870
    break;

  case 57:
#line 616 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref{AST_node("only"),yystack_[1].value.as< std::vector<expression_ref> > ()}; }
#line 1641 "parser.cc" // lalr1.cc:870
    break;

  case 58:
#line 617 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref{AST_node("hiding"),yystack_[1].value.as< std::vector<expression_ref> > ()}; }
#line 1647 "parser.cc" // lalr1.cc:870
    break;

  case 59:
#line 622 "parser.y" // lalr1.cc:870
    { }
#line 1653 "parser.cc" // lalr1.cc:870
    break;

  case 60:
#line 623 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::optional<int> > () = yystack_[0].value.as< int > (); }
#line 1659 "parser.cc" // lalr1.cc:870
    break;

  case 61:
#line 625 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infix";  }
#line 1665 "parser.cc" // lalr1.cc:870
    break;

  case 62:
#line 626 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infixl"; }
#line 1671 "parser.cc" // lalr1.cc:870
    break;

  case 63:
#line 627 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "infixr"; }
#line 1677 "parser.cc" // lalr1.cc:870
    break;

  case 64:
#line 629 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<std::string> > (),yystack_[2].value.as< std::vector<std::string> > ()); yylhs.value.as< std::vector<std::string> > ().push_back(yystack_[0].value.as< std::string > ()); }
#line 1683 "parser.cc" // lalr1.cc:870
    break;

  case 65:
#line 630 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::vector<std::string> > () = {yystack_[0].value.as< std::string > ()}; }
#line 1689 "parser.cc" // lalr1.cc:870
    break;

  case 66:
#line 634 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ()); }
#line 1695 "parser.cc" // lalr1.cc:870
    break;

  case 67:
#line 636 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[1].value.as< expression_ref > ()); }
#line 1701 "parser.cc" // lalr1.cc:870
    break;

  case 68:
#line 637 "parser.y" // lalr1.cc:870
    { }
#line 1707 "parser.cc" // lalr1.cc:870
    break;

  case 69:
#line 639 "parser.y" // lalr1.cc:870
    {}
#line 1713 "parser.cc" // lalr1.cc:870
    break;

  case 70:
#line 640 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1719 "parser.cc" // lalr1.cc:870
    break;

  case 71:
#line 641 "parser.y" // lalr1.cc:870
    {}
#line 1725 "parser.cc" // lalr1.cc:870
    break;

  case 72:
#line 644 "parser.y" // lalr1.cc:870
    {}
#line 1731 "parser.cc" // lalr1.cc:870
    break;

  case 73:
#line 651 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1737 "parser.cc" // lalr1.cc:870
    break;

  case 74:
#line 652 "parser.y" // lalr1.cc:870
    {}
#line 1743 "parser.cc" // lalr1.cc:870
    break;

  case 75:
#line 653 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[3].value.as< std::string > (),yystack_[2].value.as< int > (),yystack_[1].value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1749 "parser.cc" // lalr1.cc:870
    break;

  case 76:
#line 654 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[2].value.as< std::string > (),yystack_[1].value.as< int > (),yystack_[0].value.as< std::string > ());}
#line 1755 "parser.cc" // lalr1.cc:870
    break;

  case 77:
#line 655 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[3].value.as< std::string > (),yystack_[2].value.as< int > (),yystack_[1].value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1761 "parser.cc" // lalr1.cc:870
    break;

  case 78:
#line 656 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_builtin_expr(yystack_[2].value.as< std::string > (),yystack_[1].value.as< int > (),yystack_[0].value.as< std::string > ());}
#line 1767 "parser.cc" // lalr1.cc:870
    break;

  case 80:
#line 660 "parser.y" // lalr1.cc:870
    {}
#line 1773 "parser.cc" // lalr1.cc:870
    break;

  case 81:
#line 662 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_data_or_newtype(yystack_[4].value.as< std::string > (),yystack_[2].value.as< expression_ref > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1779 "parser.cc" // lalr1.cc:870
    break;

  case 82:
#line 663 "parser.y" // lalr1.cc:870
    {}
#line 1785 "parser.cc" // lalr1.cc:870
    break;

  case 95:
#line 721 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > ()="data";}
#line 1791 "parser.cc" // lalr1.cc:870
    break;

  case 96:
#line 722 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > ()="newtype";}
#line 1797 "parser.cc" // lalr1.cc:870
    break;

  case 99:
#line 734 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1803 "parser.cc" // lalr1.cc:870
    break;

  case 100:
#line 735 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1809 "parser.cc" // lalr1.cc:870
    break;

  case 135:
#line 804 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1815 "parser.cc" // lalr1.cc:870
    break;

  case 136:
#line 805 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1821 "parser.cc" // lalr1.cc:870
    break;

  case 137:
#line 806 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1827 "parser.cc" // lalr1.cc:870
    break;

  case 138:
#line 807 "parser.y" // lalr1.cc:870
    {}
#line 1833 "parser.cc" // lalr1.cc:870
    break;

  case 139:
#line 809 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1839 "parser.cc" // lalr1.cc:870
    break;

  case 140:
#line 810 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 1845 "parser.cc" // lalr1.cc:870
    break;

  case 141:
#line 812 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decls"),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1851 "parser.cc" // lalr1.cc:870
    break;

  case 142:
#line 818 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1857 "parser.cc" // lalr1.cc:870
    break;

  case 143:
#line 819 "parser.y" // lalr1.cc:870
    {}
#line 1863 "parser.cc" // lalr1.cc:870
    break;

  case 149:
#line 840 "parser.y" // lalr1.cc:870
    {}
#line 1869 "parser.cc" // lalr1.cc:870
    break;

  case 150:
#line 841 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1875 "parser.cc" // lalr1.cc:870
    break;

  case 151:
#line 843 "parser.y" // lalr1.cc:870
    {}
#line 1881 "parser.cc" // lalr1.cc:870
    break;

  case 152:
#line 844 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 1887 "parser.cc" // lalr1.cc:870
    break;

  case 153:
#line 846 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1893 "parser.cc" // lalr1.cc:870
    break;

  case 154:
#line 848 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1899 "parser.cc" // lalr1.cc:870
    break;

  case 155:
#line 850 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< std::string > ());}
#line 1905 "parser.cc" // lalr1.cc:870
    break;

  case 156:
#line 851 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< std::string > ());}
#line 1911 "parser.cc" // lalr1.cc:870
    break;

  case 157:
#line 853 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1917 "parser.cc" // lalr1.cc:870
    break;

  case 158:
#line 854 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 1923 "parser.cc" // lalr1.cc:870
    break;

  case 159:
#line 858 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1929 "parser.cc" // lalr1.cc:870
    break;

  case 160:
#line 859 "parser.y" // lalr1.cc:870
    {}
#line 1935 "parser.cc" // lalr1.cc:870
    break;

  case 161:
#line 860 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::string > (),yystack_[0].value.as< std::string > ());}
#line 1941 "parser.cc" // lalr1.cc:870
    break;

  case 162:
#line 862 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = "!";}
#line 1947 "parser.cc" // lalr1.cc:870
    break;

  case 163:
#line 863 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = "~";}
#line 1953 "parser.cc" // lalr1.cc:870
    break;

  case 166:
#line 868 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("forall"),{make_tv_bndrs(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 1959 "parser.cc" // lalr1.cc:870
    break;

  case 167:
#line 869 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("context"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 1965 "parser.cc" // lalr1.cc:870
    break;

  case 168:
#line 871 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1971 "parser.cc" // lalr1.cc:870
    break;

  case 169:
#line 873 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1977 "parser.cc" // lalr1.cc:870
    break;

  case 170:
#line 882 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1983 "parser.cc" // lalr1.cc:870
    break;

  case 171:
#line 884 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 1989 "parser.cc" // lalr1.cc:870
    break;

  case 172:
#line 886 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 1995 "parser.cc" // lalr1.cc:870
    break;

  case 173:
#line 887 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps({make_type_id("->"),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 2001 "parser.cc" // lalr1.cc:870
    break;

  case 174:
#line 889 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2007 "parser.cc" // lalr1.cc:870
    break;

  case 175:
#line 892 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2013 "parser.cc" // lalr1.cc:870
    break;

  case 176:
#line 894 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2019 "parser.cc" // lalr1.cc:870
    break;

  case 177:
#line 895 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2025 "parser.cc" // lalr1.cc:870
    break;

  case 178:
#line 897 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2031 "parser.cc" // lalr1.cc:870
    break;

  case 179:
#line 898 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2037 "parser.cc" // lalr1.cc:870
    break;

  case 180:
#line 900 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2043 "parser.cc" // lalr1.cc:870
    break;

  case 181:
#line 901 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2049 "parser.cc" // lalr1.cc:870
    break;

  case 182:
#line 902 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2055 "parser.cc" // lalr1.cc:870
    break;

  case 183:
#line 908 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2061 "parser.cc" // lalr1.cc:870
    break;

  case 184:
#line 910 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2067 "parser.cc" // lalr1.cc:870
    break;

  case 185:
#line 911 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id(yystack_[0].value.as< std::string > ());}
#line 2073 "parser.cc" // lalr1.cc:870
    break;

  case 186:
#line 912 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("kind_star");}
#line 2079 "parser.cc" // lalr1.cc:870
    break;

  case 187:
#line 913 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("strictness"),{yystack_[1].value.as< std::string > (),yystack_[0].value.as< expression_ref > ()}};}
#line 2085 "parser.cc" // lalr1.cc:870
    break;

  case 188:
#line 914 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("FieldDecls"),yystack_[1].value.as< std::vector<expression_ref> > ()};}
#line 2091 "parser.cc" // lalr1.cc:870
    break;

  case 189:
#line 915 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_type_id("()");}
#line 2097 "parser.cc" // lalr1.cc:870
    break;

  case 190:
#line 916 "parser.y" // lalr1.cc:870
    {auto ts = yystack_[3].value.as< std::vector<expression_ref> > ();ts.push_back(yystack_[1].value.as< expression_ref > ());yylhs.value.as< expression_ref > () = expression_ref{AST_node("TupleType"),ts};}
#line 2103 "parser.cc" // lalr1.cc:870
    break;

  case 191:
#line 917 "parser.y" // lalr1.cc:870
    {}
#line 2109 "parser.cc" // lalr1.cc:870
    break;

  case 192:
#line 918 "parser.y" // lalr1.cc:870
    {}
#line 2115 "parser.cc" // lalr1.cc:870
    break;

  case 193:
#line 919 "parser.y" // lalr1.cc:870
    {}
#line 2121 "parser.cc" // lalr1.cc:870
    break;

  case 194:
#line 920 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("ListType"),{yystack_[1].value.as< expression_ref > ()}};}
#line 2127 "parser.cc" // lalr1.cc:870
    break;

  case 195:
#line 921 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2133 "parser.cc" // lalr1.cc:870
    break;

  case 196:
#line 922 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = expression_ref{AST_node("TypeOfKind"),{yystack_[3].value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ()}};}
#line 2139 "parser.cc" // lalr1.cc:870
    break;

  case 200:
#line 930 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2145 "parser.cc" // lalr1.cc:870
    break;

  case 201:
#line 931 "parser.y" // lalr1.cc:870
    {}
#line 2151 "parser.cc" // lalr1.cc:870
    break;

  case 202:
#line 933 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2157 "parser.cc" // lalr1.cc:870
    break;

  case 203:
#line 934 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2163 "parser.cc" // lalr1.cc:870
    break;

  case 206:
#line 939 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2169 "parser.cc" // lalr1.cc:870
    break;

  case 207:
#line 940 "parser.y" // lalr1.cc:870
    {}
#line 2175 "parser.cc" // lalr1.cc:870
    break;

  case 208:
#line 942 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("type_id",yystack_[0].value.as< std::string > ());}
#line 2181 "parser.cc" // lalr1.cc:870
    break;

  case 209:
#line 943 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("type_of_kind"),{AST_node("type_id",yystack_[3].value.as< std::string > ()),yystack_[1].value.as< expression_ref > ()});}
#line 2187 "parser.cc" // lalr1.cc:870
    break;

  case 217:
#line 958 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2193 "parser.cc" // lalr1.cc:870
    break;

  case 218:
#line 964 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2199 "parser.cc" // lalr1.cc:870
    break;

  case 219:
#line 966 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2205 "parser.cc" // lalr1.cc:870
    break;

  case 220:
#line 967 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2211 "parser.cc" // lalr1.cc:870
    break;

  case 221:
#line 969 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_context(yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2217 "parser.cc" // lalr1.cc:870
    break;

  case 222:
#line 970 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2223 "parser.cc" // lalr1.cc:870
    break;

  case 223:
#line 972 "parser.y" // lalr1.cc:870
    {if (yystack_[1].value.as< std::vector<expression_ref> > ().size()>1) yylhs.value.as< expression_ref > () = make_tv_bndrs(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2229 "parser.cc" // lalr1.cc:870
    break;

  case 224:
#line 973 "parser.y" // lalr1.cc:870
    {}
#line 2235 "parser.cc" // lalr1.cc:870
    break;

  case 225:
#line 975 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2241 "parser.cc" // lalr1.cc:870
    break;

  case 226:
#line 976 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_tyapps({AST_node("type_id",yystack_[1].value.as< std::string > ()),make_tyapps(yystack_[2].value.as< std::vector<expression_ref> > ()),make_tyapps(yystack_[0].value.as< std::vector<expression_ref> > ())});}
#line 2247 "parser.cc" // lalr1.cc:870
    break;

  case 227:
#line 978 "parser.y" // lalr1.cc:870
    {}
#line 2253 "parser.cc" // lalr1.cc:870
    break;

  case 228:
#line 979 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2259 "parser.cc" // lalr1.cc:870
    break;

  case 229:
#line 981 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2265 "parser.cc" // lalr1.cc:870
    break;

  case 230:
#line 982 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2271 "parser.cc" // lalr1.cc:870
    break;

  case 231:
#line 984 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("FieldDecl"),{make_sig_vars(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2277 "parser.cc" // lalr1.cc:870
    break;

  case 242:
#line 1003 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2283 "parser.cc" // lalr1.cc:870
    break;

  case 243:
#line 1005 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decl:Strict"),{(yystack_[1].value.as< expression_ref > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2289 "parser.cc" // lalr1.cc:870
    break;

  case 244:
#line 1007 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("Decl"),{make_infixexp(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()});}
#line 2295 "parser.cc" // lalr1.cc:870
    break;

  case 245:
#line 1008 "parser.y" // lalr1.cc:870
    {}
#line 2301 "parser.cc" // lalr1.cc:870
    break;

  case 246:
#line 1011 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2307 "parser.cc" // lalr1.cc:870
    break;

  case 247:
#line 1015 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_rhs(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2313 "parser.cc" // lalr1.cc:870
    break;

  case 248:
#line 1016 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_gdrhs(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2319 "parser.cc" // lalr1.cc:870
    break;

  case 249:
#line 1018 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2325 "parser.cc" // lalr1.cc:870
    break;

  case 250:
#line 1019 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2331 "parser.cc" // lalr1.cc:870
    break;

  case 251:
#line 1023 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_gdrh(yystack_[2].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2337 "parser.cc" // lalr1.cc:870
    break;

  case 252:
#line 1025 "parser.y" // lalr1.cc:870
    {}
#line 2343 "parser.cc" // lalr1.cc:870
    break;

  case 253:
#line 1026 "parser.y" // lalr1.cc:870
    {}
#line 2349 "parser.cc" // lalr1.cc:870
    break;

  case 254:
#line 1027 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_infix(yystack_[2].value.as< std::string > (),yystack_[1].value.as< std::optional<int> > (),yystack_[0].value.as< std::vector<std::string> > ()); }
#line 2355 "parser.cc" // lalr1.cc:870
    break;

  case 255:
#line 1028 "parser.y" // lalr1.cc:870
    {}
#line 2361 "parser.cc" // lalr1.cc:870
    break;

  case 256:
#line 1029 "parser.y" // lalr1.cc:870
    {}
#line 2367 "parser.cc" // lalr1.cc:870
    break;

  case 257:
#line 1030 "parser.y" // lalr1.cc:870
    {}
#line 2373 "parser.cc" // lalr1.cc:870
    break;

  case 258:
#line 1031 "parser.y" // lalr1.cc:870
    {}
#line 2379 "parser.cc" // lalr1.cc:870
    break;

  case 259:
#line 1032 "parser.y" // lalr1.cc:870
    {}
#line 2385 "parser.cc" // lalr1.cc:870
    break;

  case 260:
#line 1033 "parser.y" // lalr1.cc:870
    {}
#line 2391 "parser.cc" // lalr1.cc:870
    break;

  case 261:
#line 1034 "parser.y" // lalr1.cc:870
    {}
#line 2397 "parser.cc" // lalr1.cc:870
    break;

  case 262:
#line 1035 "parser.y" // lalr1.cc:870
    {}
#line 2403 "parser.cc" // lalr1.cc:870
    break;

  case 267:
#line 1045 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_typed_exp(make_infixexp(yystack_[2].value.as< std::vector<expression_ref> > ()),yystack_[0].value.as< expression_ref > ()); }
#line 2409 "parser.cc" // lalr1.cc:870
    break;

  case 268:
#line 1046 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_infixexp(yystack_[0].value.as< std::vector<expression_ref> > ()); }
#line 2415 "parser.cc" // lalr1.cc:870
    break;

  case 269:
#line 1048 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2421 "parser.cc" // lalr1.cc:870
    break;

  case 270:
#line 1049 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as< std::string > ())); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2427 "parser.cc" // lalr1.cc:870
    break;

  case 271:
#line 1051 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2433 "parser.cc" // lalr1.cc:870
    break;

  case 272:
#line 1052 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(make_id(yystack_[1].value.as< std::string > ())); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2439 "parser.cc" // lalr1.cc:870
    break;

  case 273:
#line 1054 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_minus(make_fexp(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2445 "parser.cc" // lalr1.cc:870
    break;

  case 274:
#line 1055 "parser.y" // lalr1.cc:870
    {}
#line 2451 "parser.cc" // lalr1.cc:870
    break;

  case 275:
#line 1056 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_fexp(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2457 "parser.cc" // lalr1.cc:870
    break;

  case 276:
#line 1058 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2463 "parser.cc" // lalr1.cc:870
    break;

  case 277:
#line 1059 "parser.y" // lalr1.cc:870
    {}
#line 2469 "parser.cc" // lalr1.cc:870
    break;

  case 282:
#line 1070 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2475 "parser.cc" // lalr1.cc:870
    break;

  case 283:
#line 1071 "parser.y" // lalr1.cc:870
    {}
#line 2481 "parser.cc" // lalr1.cc:870
    break;

  case 284:
#line 1072 "parser.y" // lalr1.cc:870
    {}
#line 2487 "parser.cc" // lalr1.cc:870
    break;

  case 285:
#line 1073 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2493 "parser.cc" // lalr1.cc:870
    break;

  case 286:
#line 1075 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_as_pattern(yystack_[2].value.as< std::string > (),yystack_[0].value.as< expression_ref > ());}
#line 2499 "parser.cc" // lalr1.cc:870
    break;

  case 287:
#line 1076 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_lazy_pattern(yystack_[0].value.as< expression_ref > ());}
#line 2505 "parser.cc" // lalr1.cc:870
    break;

  case 288:
#line 1077 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_lambda(yystack_[2].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2511 "parser.cc" // lalr1.cc:870
    break;

  case 289:
#line 1078 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_let(yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2517 "parser.cc" // lalr1.cc:870
    break;

  case 290:
#line 1079 "parser.y" // lalr1.cc:870
    {}
#line 2523 "parser.cc" // lalr1.cc:870
    break;

  case 291:
#line 1080 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_if(yystack_[6].value.as< expression_ref > (),yystack_[3].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2529 "parser.cc" // lalr1.cc:870
    break;

  case 292:
#line 1081 "parser.y" // lalr1.cc:870
    {}
#line 2535 "parser.cc" // lalr1.cc:870
    break;

  case 293:
#line 1082 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_case(yystack_[2].value.as< expression_ref > (),make_alts(yystack_[0].value.as< std::vector<expression_ref> > ()));}
#line 2541 "parser.cc" // lalr1.cc:870
    break;

  case 294:
#line 1083 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_do(yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2547 "parser.cc" // lalr1.cc:870
    break;

  case 295:
#line 1084 "parser.y" // lalr1.cc:870
    {}
#line 2553 "parser.cc" // lalr1.cc:870
    break;

  case 296:
#line 1085 "parser.y" // lalr1.cc:870
    {}
#line 2559 "parser.cc" // lalr1.cc:870
    break;

  case 297:
#line 1086 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2565 "parser.cc" // lalr1.cc:870
    break;

  case 298:
#line 1088 "parser.y" // lalr1.cc:870
    {}
#line 2571 "parser.cc" // lalr1.cc:870
    break;

  case 299:
#line 1089 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2577 "parser.cc" // lalr1.cc:870
    break;

  case 300:
#line 1091 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_id(yystack_[0].value.as< std::string > ());}
#line 2583 "parser.cc" // lalr1.cc:870
    break;

  case 301:
#line 1092 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_id(yystack_[0].value.as< std::string > ());}
#line 2589 "parser.cc" // lalr1.cc:870
    break;

  case 302:
#line 1093 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2595 "parser.cc" // lalr1.cc:870
    break;

  case 303:
#line 1094 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2601 "parser.cc" // lalr1.cc:870
    break;

  case 304:
#line 1095 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_tuple(yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2607 "parser.cc" // lalr1.cc:870
    break;

  case 305:
#line 1096 "parser.y" // lalr1.cc:870
    {}
#line 2613 "parser.cc" // lalr1.cc:870
    break;

  case 306:
#line 1097 "parser.y" // lalr1.cc:870
    {}
#line 2619 "parser.cc" // lalr1.cc:870
    break;

  case 307:
#line 1098 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ());}
#line 2625 "parser.cc" // lalr1.cc:870
    break;

  case 308:
#line 1099 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = AST_node("WildcardPattern");}
#line 2631 "parser.cc" // lalr1.cc:870
    break;

  case 309:
#line 1104 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2637 "parser.cc" // lalr1.cc:870
    break;

  case 310:
#line 1105 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("LeftSection"),{make_infixexp(yystack_[1].value.as< std::vector<expression_ref> > ()),make_id(yystack_[0].value.as< std::string > ())});}
#line 2643 "parser.cc" // lalr1.cc:870
    break;

  case 311:
#line 1106 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("RightSection"),{make_id(yystack_[1].value.as< std::string > ()),make_infixexp(yystack_[0].value.as< std::vector<expression_ref> > ())});}
#line 2649 "parser.cc" // lalr1.cc:870
    break;

  case 312:
#line 1111 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2655 "parser.cc" // lalr1.cc:870
    break;

  case 313:
#line 1112 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2661 "parser.cc" // lalr1.cc:870
    break;

  case 314:
#line 1130 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = {AST_node("id",":"),yystack_[0].value.as< expression_ref > (),AST_node("id","[]")}; }
#line 2667 "parser.cc" // lalr1.cc:870
    break;

  case 315:
#line 1131 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = make_list(yystack_[0].value.as< std::vector<expression_ref> > ()); }
#line 2673 "parser.cc" // lalr1.cc:870
    break;

  case 316:
#line 1132 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFrom"),{yystack_[1].value.as< expression_ref > ()}); }
#line 2679 "parser.cc" // lalr1.cc:870
    break;

  case 317:
#line 1133 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFromThen"),{yystack_[3].value.as< expression_ref > (),yystack_[1].value.as< expression_ref > ()}); }
#line 2685 "parser.cc" // lalr1.cc:870
    break;

  case 318:
#line 1134 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFromTo"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2691 "parser.cc" // lalr1.cc:870
    break;

  case 319:
#line 1135 "parser.y" // lalr1.cc:870
    { yylhs.value.as< expression_ref > () = expression_ref(AST_node("enumFromToThen"),{yystack_[4].value.as< expression_ref > (),yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()}); }
#line 2697 "parser.cc" // lalr1.cc:870
    break;

  case 320:
#line 1136 "parser.y" // lalr1.cc:870
    { auto quals = yystack_[0].value.as< std::vector<expression_ref> > (); quals.push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< expression_ref > () = expression_ref(AST_node("ListComprehension"),quals); }
#line 2703 "parser.cc" // lalr1.cc:870
    break;

  case 321:
#line 1138 "parser.y" // lalr1.cc:870
    { std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2709 "parser.cc" // lalr1.cc:870
    break;

  case 322:
#line 1139 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[2].value.as< expression_ref > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2715 "parser.cc" // lalr1.cc:870
    break;

  case 323:
#line 1151 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2721 "parser.cc" // lalr1.cc:870
    break;

  case 324:
#line 1152 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2727 "parser.cc" // lalr1.cc:870
    break;

  case 325:
#line 1153 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2733 "parser.cc" // lalr1.cc:870
    break;

  case 326:
#line 1154 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2739 "parser.cc" // lalr1.cc:870
    break;

  case 327:
#line 1156 "parser.y" // lalr1.cc:870
    {}
#line 2745 "parser.cc" // lalr1.cc:870
    break;

  case 328:
#line 1157 "parser.y" // lalr1.cc:870
    {}
#line 2751 "parser.cc" // lalr1.cc:870
    break;

  case 329:
#line 1158 "parser.y" // lalr1.cc:870
    {}
#line 2757 "parser.cc" // lalr1.cc:870
    break;

  case 330:
#line 1159 "parser.y" // lalr1.cc:870
    {}
#line 2763 "parser.cc" // lalr1.cc:870
    break;

  case 331:
#line 1162 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2769 "parser.cc" // lalr1.cc:870
    break;

  case 332:
#line 1164 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ());yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2775 "parser.cc" // lalr1.cc:870
    break;

  case 333:
#line 1165 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2781 "parser.cc" // lalr1.cc:870
    break;

  case 334:
#line 1168 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2787 "parser.cc" // lalr1.cc:870
    break;

  case 335:
#line 1169 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2793 "parser.cc" // lalr1.cc:870
    break;

  case 336:
#line 1170 "parser.y" // lalr1.cc:870
    {}
#line 2799 "parser.cc" // lalr1.cc:870
    break;

  case 337:
#line 1171 "parser.y" // lalr1.cc:870
    {}
#line 2805 "parser.cc" // lalr1.cc:870
    break;

  case 338:
#line 1173 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2811 "parser.cc" // lalr1.cc:870
    break;

  case 339:
#line 1174 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[0].value.as< std::vector<expression_ref> > ());}
#line 2817 "parser.cc" // lalr1.cc:870
    break;

  case 340:
#line 1176 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2823 "parser.cc" // lalr1.cc:870
    break;

  case 341:
#line 1177 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2829 "parser.cc" // lalr1.cc:870
    break;

  case 342:
#line 1178 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2835 "parser.cc" // lalr1.cc:870
    break;

  case 343:
#line 1180 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_alt(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2841 "parser.cc" // lalr1.cc:870
    break;

  case 344:
#line 1182 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_rhs(yystack_[1].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2847 "parser.cc" // lalr1.cc:870
    break;

  case 345:
#line 1183 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_gdrhs(yystack_[1].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2853 "parser.cc" // lalr1.cc:870
    break;

  case 346:
#line 1185 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2859 "parser.cc" // lalr1.cc:870
    break;

  case 347:
#line 1186 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2865 "parser.cc" // lalr1.cc:870
    break;

  case 348:
#line 1188 "parser.y" // lalr1.cc:870
    {}
#line 2871 "parser.cc" // lalr1.cc:870
    break;

  case 349:
#line 1189 "parser.y" // lalr1.cc:870
    {}
#line 2877 "parser.cc" // lalr1.cc:870
    break;

  case 350:
#line 1191 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > ()=make_gdrh(yystack_[2].value.as< std::vector<expression_ref> > (),yystack_[0].value.as< expression_ref > ());}
#line 2883 "parser.cc" // lalr1.cc:870
    break;

  case 351:
#line 1193 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2889 "parser.cc" // lalr1.cc:870
    break;

  case 352:
#line 1194 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_strict_pattern(yystack_[0].value.as< expression_ref > ());}
#line 2895 "parser.cc" // lalr1.cc:870
    break;

  case 353:
#line 1196 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2901 "parser.cc" // lalr1.cc:870
    break;

  case 354:
#line 1197 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_strict_pattern(yystack_[0].value.as< expression_ref > ());}
#line 2907 "parser.cc" // lalr1.cc:870
    break;

  case 355:
#line 1199 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ());}
#line 2913 "parser.cc" // lalr1.cc:870
    break;

  case 356:
#line 1200 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = make_strict_pattern(yystack_[0].value.as< expression_ref > ());}
#line 2919 "parser.cc" // lalr1.cc:870
    break;

  case 357:
#line 1202 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2925 "parser.cc" // lalr1.cc:870
    break;

  case 358:
#line 1203 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2931 "parser.cc" // lalr1.cc:870
    break;

  case 359:
#line 1206 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2937 "parser.cc" // lalr1.cc:870
    break;

  case 360:
#line 1207 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2943 "parser.cc" // lalr1.cc:870
    break;

  case 361:
#line 1209 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[2].value.as< std::vector<expression_ref> > ()); yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2949 "parser.cc" // lalr1.cc:870
    break;

  case 362:
#line 1210 "parser.y" // lalr1.cc:870
    {std::swap(yylhs.value.as< std::vector<expression_ref> > (),yystack_[1].value.as< std::vector<expression_ref> > ());}
#line 2955 "parser.cc" // lalr1.cc:870
    break;

  case 363:
#line 1211 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::vector<expression_ref> > ().push_back(yystack_[0].value.as< expression_ref > ());}
#line 2961 "parser.cc" // lalr1.cc:870
    break;

  case 364:
#line 1212 "parser.y" // lalr1.cc:870
    {}
#line 2967 "parser.cc" // lalr1.cc:870
    break;

  case 365:
#line 1217 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< expression_ref > ();}
#line 2973 "parser.cc" // lalr1.cc:870
    break;

  case 366:
#line 1218 "parser.y" // lalr1.cc:870
    {}
#line 2979 "parser.cc" // lalr1.cc:870
    break;

  case 367:
#line 1220 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("PatQual"),{yystack_[2].value.as< expression_ref > (),yystack_[0].value.as< expression_ref > ()});}
#line 2985 "parser.cc" // lalr1.cc:870
    break;

  case 368:
#line 1221 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("SimpleQual"),{yystack_[0].value.as< expression_ref > ()});}
#line 2991 "parser.cc" // lalr1.cc:870
    break;

  case 369:
#line 1222 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = new expression(AST_node("LetQual"),{yystack_[0].value.as< expression_ref > ()});}
#line 2997 "parser.cc" // lalr1.cc:870
    break;

  case 377:
#line 1267 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3003 "parser.cc" // lalr1.cc:870
    break;

  case 378:
#line 1268 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3009 "parser.cc" // lalr1.cc:870
    break;

  case 379:
#line 1270 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3015 "parser.cc" // lalr1.cc:870
    break;

  case 380:
#line 1271 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3021 "parser.cc" // lalr1.cc:870
    break;

  case 381:
#line 1273 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3027 "parser.cc" // lalr1.cc:870
    break;

  case 382:
#line 1274 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3033 "parser.cc" // lalr1.cc:870
    break;

  case 383:
#line 1275 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3039 "parser.cc" // lalr1.cc:870
    break;

  case 386:
#line 1280 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () =  "()"; }
#line 3045 "parser.cc" // lalr1.cc:870
    break;

  case 387:
#line 1281 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "("+std::string(yystack_[1].value.as< int > (),',')+")"; }
#line 3051 "parser.cc" // lalr1.cc:870
    break;

  case 388:
#line 1282 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(##)"; }
#line 3057 "parser.cc" // lalr1.cc:870
    break;

  case 389:
#line 1283 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(#"+std::string(yystack_[1].value.as< int > (),',')+"#)"; }
#line 3063 "parser.cc" // lalr1.cc:870
    break;

  case 390:
#line 1285 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3069 "parser.cc" // lalr1.cc:870
    break;

  case 391:
#line 1286 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "[]"; }
#line 3075 "parser.cc" // lalr1.cc:870
    break;

  case 392:
#line 1288 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3081 "parser.cc" // lalr1.cc:870
    break;

  case 393:
#line 1289 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3087 "parser.cc" // lalr1.cc:870
    break;

  case 394:
#line 1291 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3093 "parser.cc" // lalr1.cc:870
    break;

  case 395:
#line 1292 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3099 "parser.cc" // lalr1.cc:870
    break;

  case 396:
#line 1295 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3105 "parser.cc" // lalr1.cc:870
    break;

  case 397:
#line 1296 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "()"; }
#line 3111 "parser.cc" // lalr1.cc:870
    break;

  case 398:
#line 1297 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(##)"; }
#line 3117 "parser.cc" // lalr1.cc:870
    break;

  case 399:
#line 1299 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3123 "parser.cc" // lalr1.cc:870
    break;

  case 400:
#line 1300 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "("+std::string(yystack_[1].value.as< int > (),',')+")"; }
#line 3129 "parser.cc" // lalr1.cc:870
    break;

  case 401:
#line 1301 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "(#"+std::string(yystack_[1].value.as< int > (),',')+"#)"; }
#line 3135 "parser.cc" // lalr1.cc:870
    break;

  case 402:
#line 1302 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "->"; }
#line 3141 "parser.cc" // lalr1.cc:870
    break;

  case 403:
#line 1303 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "[]"; }
#line 3147 "parser.cc" // lalr1.cc:870
    break;

  case 404:
#line 1305 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3153 "parser.cc" // lalr1.cc:870
    break;

  case 405:
#line 1306 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3159 "parser.cc" // lalr1.cc:870
    break;

  case 406:
#line 1307 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "~"; }
#line 3165 "parser.cc" // lalr1.cc:870
    break;

  case 407:
#line 1309 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3171 "parser.cc" // lalr1.cc:870
    break;

  case 408:
#line 1310 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3177 "parser.cc" // lalr1.cc:870
    break;

  case 409:
#line 1311 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3183 "parser.cc" // lalr1.cc:870
    break;

  case 410:
#line 1312 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3189 "parser.cc" // lalr1.cc:870
    break;

  case 411:
#line 1313 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "~"; }
#line 3195 "parser.cc" // lalr1.cc:870
    break;

  case 412:
#line 1316 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3201 "parser.cc" // lalr1.cc:870
    break;

  case 413:
#line 1317 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3207 "parser.cc" // lalr1.cc:870
    break;

  case 414:
#line 1319 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3213 "parser.cc" // lalr1.cc:870
    break;

  case 415:
#line 1321 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3219 "parser.cc" // lalr1.cc:870
    break;

  case 416:
#line 1322 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3225 "parser.cc" // lalr1.cc:870
    break;

  case 417:
#line 1326 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3231 "parser.cc" // lalr1.cc:870
    break;

  case 418:
#line 1328 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3237 "parser.cc" // lalr1.cc:870
    break;

  case 419:
#line 1329 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3243 "parser.cc" // lalr1.cc:870
    break;

  case 420:
#line 1330 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3249 "parser.cc" // lalr1.cc:870
    break;

  case 421:
#line 1332 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3255 "parser.cc" // lalr1.cc:870
    break;

  case 422:
#line 1333 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3261 "parser.cc" // lalr1.cc:870
    break;

  case 423:
#line 1334 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3267 "parser.cc" // lalr1.cc:870
    break;

  case 424:
#line 1335 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "-"; }
#line 3273 "parser.cc" // lalr1.cc:870
    break;

  case 425:
#line 1340 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3279 "parser.cc" // lalr1.cc:870
    break;

  case 426:
#line 1341 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3285 "parser.cc" // lalr1.cc:870
    break;

  case 427:
#line 1343 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3291 "parser.cc" // lalr1.cc:870
    break;

  case 428:
#line 1344 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3297 "parser.cc" // lalr1.cc:870
    break;

  case 429:
#line 1346 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3303 "parser.cc" // lalr1.cc:870
    break;

  case 430:
#line 1347 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3309 "parser.cc" // lalr1.cc:870
    break;

  case 431:
#line 1348 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3315 "parser.cc" // lalr1.cc:870
    break;

  case 432:
#line 1350 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3321 "parser.cc" // lalr1.cc:870
    break;

  case 433:
#line 1351 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3327 "parser.cc" // lalr1.cc:870
    break;

  case 434:
#line 1352 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3333 "parser.cc" // lalr1.cc:870
    break;

  case 435:
#line 1354 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "_"; }
#line 3339 "parser.cc" // lalr1.cc:870
    break;

  case 436:
#line 1356 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3345 "parser.cc" // lalr1.cc:870
    break;

  case 437:
#line 1357 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3351 "parser.cc" // lalr1.cc:870
    break;

  case 438:
#line 1359 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () =yystack_[0].value.as< std::string > (); }
#line 3357 "parser.cc" // lalr1.cc:870
    break;

  case 439:
#line 1360 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3363 "parser.cc" // lalr1.cc:870
    break;

  case 440:
#line 1364 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3369 "parser.cc" // lalr1.cc:870
    break;

  case 441:
#line 1366 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3375 "parser.cc" // lalr1.cc:870
    break;

  case 442:
#line 1368 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3381 "parser.cc" // lalr1.cc:870
    break;

  case 443:
#line 1369 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3387 "parser.cc" // lalr1.cc:870
    break;

  case 444:
#line 1370 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unsafe"; }
#line 3393 "parser.cc" // lalr1.cc:870
    break;

  case 445:
#line 1371 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "safe"; }
#line 3399 "parser.cc" // lalr1.cc:870
    break;

  case 446:
#line 1372 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "interruptible"; }
#line 3405 "parser.cc" // lalr1.cc:870
    break;

  case 447:
#line 1375 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3411 "parser.cc" // lalr1.cc:870
    break;

  case 448:
#line 1376 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3417 "parser.cc" // lalr1.cc:870
    break;

  case 449:
#line 1378 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3423 "parser.cc" // lalr1.cc:870
    break;

  case 450:
#line 1379 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3429 "parser.cc" // lalr1.cc:870
    break;

  case 451:
#line 1380 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[1].value.as< std::string > (); }
#line 3435 "parser.cc" // lalr1.cc:870
    break;

  case 452:
#line 1382 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3441 "parser.cc" // lalr1.cc:870
    break;

  case 453:
#line 1383 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3447 "parser.cc" // lalr1.cc:870
    break;

  case 454:
#line 1385 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3453 "parser.cc" // lalr1.cc:870
    break;

  case 455:
#line 1386 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3459 "parser.cc" // lalr1.cc:870
    break;

  case 456:
#line 1387 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unsafe"; }
#line 3465 "parser.cc" // lalr1.cc:870
    break;

  case 457:
#line 1388 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "safe"; }
#line 3471 "parser.cc" // lalr1.cc:870
    break;

  case 458:
#line 1389 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "interruptible"; }
#line 3477 "parser.cc" // lalr1.cc:870
    break;

  case 459:
#line 1390 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "forall"; }
#line 3483 "parser.cc" // lalr1.cc:870
    break;

  case 460:
#line 1391 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "family"; }
#line 3489 "parser.cc" // lalr1.cc:870
    break;

  case 461:
#line 1392 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "role"; }
#line 3495 "parser.cc" // lalr1.cc:870
    break;

  case 462:
#line 1394 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3501 "parser.cc" // lalr1.cc:870
    break;

  case 463:
#line 1395 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3507 "parser.cc" // lalr1.cc:870
    break;

  case 464:
#line 1397 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3513 "parser.cc" // lalr1.cc:870
    break;

  case 465:
#line 1398 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3519 "parser.cc" // lalr1.cc:870
    break;

  case 466:
#line 1400 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3525 "parser.cc" // lalr1.cc:870
    break;

  case 467:
#line 1402 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3531 "parser.cc" // lalr1.cc:870
    break;

  case 468:
#line 1403 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "-"; }
#line 3537 "parser.cc" // lalr1.cc:870
    break;

  case 469:
#line 1405 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3543 "parser.cc" // lalr1.cc:870
    break;

  case 470:
#line 1406 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3549 "parser.cc" // lalr1.cc:870
    break;

  case 471:
#line 1408 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "as"; }
#line 3555 "parser.cc" // lalr1.cc:870
    break;

  case 472:
#line 1409 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "qualified"; }
#line 3561 "parser.cc" // lalr1.cc:870
    break;

  case 473:
#line 1410 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "hiding"; }
#line 3567 "parser.cc" // lalr1.cc:870
    break;

  case 474:
#line 1411 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "export"; }
#line 3573 "parser.cc" // lalr1.cc:870
    break;

  case 475:
#line 1412 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "label"; }
#line 3579 "parser.cc" // lalr1.cc:870
    break;

  case 476:
#line 1413 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "dynamic"; }
#line 3585 "parser.cc" // lalr1.cc:870
    break;

  case 477:
#line 1414 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "stdcall"; }
#line 3591 "parser.cc" // lalr1.cc:870
    break;

  case 478:
#line 1415 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "ccall"; }
#line 3597 "parser.cc" // lalr1.cc:870
    break;

  case 479:
#line 1416 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "capi"; }
#line 3603 "parser.cc" // lalr1.cc:870
    break;

  case 480:
#line 1417 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "prim"; }
#line 3609 "parser.cc" // lalr1.cc:870
    break;

  case 481:
#line 1418 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "javascript"; }
#line 3615 "parser.cc" // lalr1.cc:870
    break;

  case 482:
#line 1419 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "group"; }
#line 3621 "parser.cc" // lalr1.cc:870
    break;

  case 483:
#line 1420 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "stock"; }
#line 3627 "parser.cc" // lalr1.cc:870
    break;

  case 484:
#line 1421 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "anyclass"; }
#line 3633 "parser.cc" // lalr1.cc:870
    break;

  case 485:
#line 1422 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "via"; }
#line 3639 "parser.cc" // lalr1.cc:870
    break;

  case 486:
#line 1423 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "unit"; }
#line 3645 "parser.cc" // lalr1.cc:870
    break;

  case 487:
#line 1424 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "dependency"; }
#line 3651 "parser.cc" // lalr1.cc:870
    break;

  case 488:
#line 1425 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "signature"; }
#line 3657 "parser.cc" // lalr1.cc:870
    break;

  case 489:
#line 1427 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "!"; }
#line 3663 "parser.cc" // lalr1.cc:870
    break;

  case 490:
#line 1428 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "."; }
#line 3669 "parser.cc" // lalr1.cc:870
    break;

  case 491:
#line 1429 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = "*"; }
#line 3675 "parser.cc" // lalr1.cc:870
    break;

  case 492:
#line 1433 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3681 "parser.cc" // lalr1.cc:870
    break;

  case 493:
#line 1434 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3687 "parser.cc" // lalr1.cc:870
    break;

  case 494:
#line 1436 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3693 "parser.cc" // lalr1.cc:870
    break;

  case 495:
#line 1438 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3699 "parser.cc" // lalr1.cc:870
    break;

  case 496:
#line 1439 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3705 "parser.cc" // lalr1.cc:870
    break;

  case 497:
#line 1441 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > (); }
#line 3711 "parser.cc" // lalr1.cc:870
    break;

  case 498:
#line 1442 "parser.y" // lalr1.cc:870
    { yylhs.value.as< std::string > () = ":"; }
#line 3717 "parser.cc" // lalr1.cc:870
    break;

  case 499:
#line 1446 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< char > ();}
#line 3723 "parser.cc" // lalr1.cc:870
    break;

  case 500:
#line 1447 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yy_make_string(yystack_[0].value.as< std::string > ());}
#line 3729 "parser.cc" // lalr1.cc:870
    break;

  case 501:
#line 1448 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< int > ();}
#line 3735 "parser.cc" // lalr1.cc:870
    break;

  case 502:
#line 1449 "parser.y" // lalr1.cc:870
    {yylhs.value.as< expression_ref > () = yystack_[0].value.as< double > ();}
#line 3741 "parser.cc" // lalr1.cc:870
    break;

  case 504:
#line 1457 "parser.y" // lalr1.cc:870
    { yyerrok; drv.pop_error_message(); drv.pop_context();}
#line 3747 "parser.cc" // lalr1.cc:870
    break;

  case 505:
#line 1461 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3753 "parser.cc" // lalr1.cc:870
    break;

  case 506:
#line 1462 "parser.y" // lalr1.cc:870
    {yylhs.value.as< std::string > () = yystack_[0].value.as< std::string > ();}
#line 3759 "parser.cc" // lalr1.cc:870
    break;

  case 507:
#line 1464 "parser.y" // lalr1.cc:870
    {yylhs.value.as< int > () = yystack_[1].value.as< int > () + 1;}
#line 3765 "parser.cc" // lalr1.cc:870
    break;

  case 508:
#line 1465 "parser.y" // lalr1.cc:870
    {yylhs.value.as< int > () = 1;}
#line 3771 "parser.cc" // lalr1.cc:870
    break;


#line 3775 "parser.cc" // lalr1.cc:870
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


  const short parser::yypact_ninf_ = -693;

  const short parser::yytable_ninf_ = -468;

  const short
  parser::yypact_[] =
  {
      35,   214,  -693,   153,  -693,  -693,  -693,  -693,  -693,   272,
     -18,    49,  -693,    57,    -4,    -4,   111,  -693,  -693,  -693,
    -693,   248,  -693,  -693,  -693,   179,  -693,   150,   303,  4948,
     218,   265,   277,  -693,   832,  -693,   107,  -693,  -693,  -693,
    -693,   214,  -693,   128,  -693,  -693,  -693,  -693,  -693,  -693,
    -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,   130,
    -693,  -693,  -693,  -693,  -693,  -693,   951,  -693,  -693,  -693,
    -693,   304,   309,  -693,   298,  -693,  -693,  -693,  -693,  -693,
    -693,  -693,  -693,   108,   354,   403,  -693,   340,  -693,  2898,
    -693,   352,   172,  2002,  -693,  -693,  -693,   289,   196,  -693,
    4514,  5212,   172,  3794,  5302,  3794,   371,   349,  5155,   116,
    3410,  3794,  3538,  3794,  1618,  1362,  1490,  -693,  -693,  -693,
    -693,  -693,  -693,  4514,    34,   371,   350,   277,  -693,  -693,
    -693,    60,  -693,  -693,  -693,  -693,   505,  -693,  3666,  -693,
     381,  -693,  -693,  -693,  -693,  -693,   369,   394,   372,  -693,
    -693,  -693,  -693,   356,  -693,   318,  -693,  -693,   378,   185,
     175,  -693,   377,   380,  -693,  -693,  -693,  -693,  -693,   385,
    -693,   387,   388,   389,  -693,  -693,  -693,  4948,  4994,  -693,
    -693,  -693,  -693,  -693,  -693,   464,  -693,    48,  1362,   476,
     618,  -693,  -693,  2898,  4208,  2130,  2130,  -693,  2642,   415,
     393,    56,  -693,  -693,   423,   432,   435,   438,  4208,  1102,
    1102,  -693,   509,  -693,  -693,  -693,   391,   395,  -693,  -693,
    -693,  -693,  -693,  5456,  4106,  3902,  4004,  5052,  -693,  -693,
    -693,  -693,  -693,  4848,  -693,     9,   444,   441,  4514,  -693,
    -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,
     343,  5502,   406,   407,  -693,  -693,  -693,   446,   198,   275,
    5402,   457,  -693,    23,  -693,  -693,    18,  5155,  -693,   461,
     236,    -9,   428,   463,  2258,  3794,  -693,  -693,  3282,  -693,
    3666,    30,  -693,  -693,  4616,  -693,  -693,  -693,   618,    89,
     440,   439,  -693,  2898,  -693,  -693,  -693,  -693,  -693,  -693,
    -693,  3538,  -693,  -693,    66,   149,   388,   447,   448,   450,
     231,  -693,   264,   267,   269,   462,   460,  -693,    82,  4208,
    5155,  5155,  -693,   411,   340,   481,   426,  4514,  4208,  4616,
      30,  -693,  3154,  -693,  -693,  -693,  -693,  -693,  4848,  -693,
    5098,  5456,  3794,  -693,   452,   453,   450,  -693,  -693,  -693,
    -693,  -693,  -693,  -693,  -693,   456,   451,  -693,  -693,   467,
      57,  -693,   443,   492,   496,   249,  4208,  2898,  -693,  -693,
    -693,   486,  -693,   469,   465,   196,   172,  3794,   495,   497,
      99,  -693,  -693,    50,   501,   474,  -693,    91,  -693,   559,
    -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,   568,   167,
    -693,  -693,   664,    88,  2898,  -693,  -693,     1,   503,   491,
    -693,  -693,  -693,   502,   499,   452,  -693,    64,   500,   453,
     233,  -693,   526,   285,   507,   297,   504,   506,  -693,  -693,
    4208,  4208,  -693,   514,   508,   487,   498,  2898,   519,  2770,
    2770,  5502,   116,  -693,  5502,  4208,   517,  5502,  -693,   510,
     528,   560,  -693,  -693,   561,   144,   565,  1874,  1234,  -693,
    -693,  2898,  -693,  2898,  2642,  -693,    46,  -693,   531,   535,
     540,  2898,  2898,  2386,  1746,  -693,  1746,   521,  -693,  1746,
    -693,  1746,   542,  -693,  -693,  -693,  -693,  -693,  -693,  -693,
     631,  4514,   581,   579,   580,  5348,   548,  -693,  -693,  -693,
    4310,    -8,   291,  -693,  -693,   104,  -693,   550,  -693,  -693,
    -693,  -693,   572,  -693,   554,   596,    52,  -693,  -693,  -693,
    -693,  4994,  -693,  -693,  -693,   214,  -693,  -693,  -693,  -693,
    -693,  5548,  4208,  -693,  4208,   509,  -693,  -693,  2898,  -693,
    2130,  -693,  2898,  2642,  -693,  2898,   359,  -693,  -693,  1102,
    -693,  -693,  4208,  5456,  -693,  5456,  -693,  -693,  4208,  -693,
    4208,  -693,  4208,  -693,  -693,  -693,  -693,  -693,  -693,  -693,
    -693,  -693,  -693,   551,   552,  -693,  -693,  3794,  -693,  -693,
     652,   582,  5502,  -693,  -693,  -693,   566,  -693,   583,  -693,
    -693,  -693,   587,   200,   329,  -693,  -693,  -693,  -693,  2514,
     584,   573,  -693,   379,    57,  -693,  -693,   659,   611,   196,
    -693,  -693,  -693,  -693,  -693,  -693,  3026,   585,  -693,  -693,
     615,  -693,  -693,  -693,  -693,   586,  -693,  5652,   367,  -693,
    -693,  -693,  4208,  4208,   411,  4208,  -693,   613,  -693,   619,
     667,  -693,   693,  -693,  -693,  5098,  1746,  4208,   588,   701,
    4208,  5709,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,
     393,  1102,  1102,  -693,  -693,  -693,  -693,  -693,  -693,   603,
     606,   526,  -693,  -693,  -693,  -693,   368,  -693,  -693,  -693,
    -693,  -693,  -693,  -693,  -693,  -693,  2770,  2898,  -693,    58,
    -693,  -693,  2898,  -693,   370,   669,  2386,  2898,  -693,  -693,
    -693,   970,   970,  -693,  -693,    -2,    80,  -693,  -693,  -693,
    -693,  -693,   633,  -693,  4848,   224,  -693,   693,  -693,  -693,
    -693,  -693,  -693,   214,    54,  -693,   638,   709,  -693,   169,
    -693,   102,  -693,  -693,  1102,  1102,  -693,   659,  -693,  -693,
    2898,  2898,  2898,  -693,  -693,  -693,  -693,  5709,  2898,  -693,
     181,  -693,   103,  -693,  4208,  -693,  5582,   667,   632,  4662,
    -693,  -693,  -693,  -693,  -693,  -693,  4412,   195,   670,  -693,
    -693,  -693,  -693,   622,  4948,  -693,  -693,  4208,  2898,  -693,
    1102,  -693,   221,    88,  -693,   672,  -693,  -693,   813,  -693,
     970,  -693,  -693,  -693,  -693,  4848,  -693,  4848,  -693,  -693,
     608,   617,  -693,  4514,  -693,  4948,   623,   624,  -693,  -693,
    -693,  -693,  2898,  4208,  -693,  4755,  -693,  4848,  4514,  -693,
    -693,   626,  -693,  -693,  -693,  -693,  -693,  -693
  };

  const unsigned short
  parser::yydefact_[] =
  {
       5,     0,    40,     0,     2,    40,     4,   505,   506,     8,
       0,    43,     1,     0,     0,     0,    18,    11,    39,    13,
      16,    68,   504,   503,    12,   148,   144,     0,     0,     0,
       0,    46,    41,    15,    14,   147,     0,     6,     7,   471,
     473,     0,   472,     0,   459,   474,   475,   476,   457,   458,
     456,   460,   461,   477,   478,   479,   480,   481,   482,     0,
     483,   484,   485,   486,   488,   487,     0,   454,   417,   453,
     415,     0,    19,    21,    25,    33,    36,   407,   416,    35,
     449,   452,   455,     0,     0,    48,    38,    42,   308,     0,
      95,     0,     0,     0,    61,    62,    63,    90,     0,    96,
       0,     0,     0,     0,     0,     0,   263,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   494,   493,   499,
     500,   501,   502,     0,   263,   263,    59,    66,    69,    70,
      71,   103,   245,   255,    73,   242,    74,   271,   275,   285,
     297,   299,   301,   377,   390,   378,     0,   300,   452,   379,
     492,   302,   145,     0,    23,     0,    34,   404,     0,     0,
       0,    24,     0,     0,   468,   489,   491,   490,   469,     0,
     466,     0,     0,     0,   467,   470,    17,     0,    27,    22,
      40,    40,     3,    45,    47,    52,    37,     0,     0,     0,
     268,   276,   269,     0,   201,   364,   364,   294,     0,     0,
     279,     0,   292,   347,     0,     0,     0,     0,     0,   138,
     138,   141,     0,   445,   446,   444,     0,     0,   423,   163,
     424,   162,   186,   227,     0,     0,     0,     0,   442,   422,
     421,   419,   418,     0,   159,   160,     0,   172,   175,   178,
     180,   184,   399,   181,   412,   420,   185,   182,   440,   443,
       0,     0,     0,     0,   447,   427,   295,     0,     0,     0,
     110,     0,   383,     0,   381,   284,     0,     0,   264,     0,
       0,     0,   384,   151,     0,     0,   355,   358,     0,   287,
     273,     0,   498,   391,     0,   497,   496,   309,   268,   314,
       0,   315,   433,     0,   434,   432,   438,   465,   464,   394,
     495,   468,   386,   508,     0,     0,   465,     0,   464,   394,
       0,   388,     0,     0,     0,   210,     0,   100,   172,     0,
       0,     0,    60,     0,    67,   103,     0,     0,     0,     0,
       0,   430,     0,   431,   429,   436,   463,   462,     0,   282,
     371,     0,     0,   146,     0,     0,     0,   410,   411,   409,
     408,   451,   450,    20,    32,     0,    28,    30,    31,     0,
       0,    51,    50,     0,     0,     0,     0,     0,   277,   207,
     202,     0,   168,     0,   200,     0,     0,     0,   368,     0,
       0,   363,   365,     0,     0,   331,   333,     0,   278,     0,
     346,   349,    87,    86,    88,    89,   197,   153,   134,     0,
     246,   137,   149,     0,     0,   164,   165,     0,     0,   228,
     230,   156,   403,     0,     0,   163,   189,   202,     0,   412,
       0,   191,   202,     0,     0,     0,     0,     0,   187,   161,
       0,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   110,     0,     0,     0,   392,     0,
       0,     0,   274,   258,     0,     0,     0,     0,     0,   290,
     356,     0,   357,     0,     0,   243,   143,   250,     0,     0,
       0,   310,   316,     0,     0,   307,     0,   311,   303,     0,
     304,     0,   450,   380,   387,   507,   305,   306,   389,   215,
     125,     0,     0,     0,     0,     0,   254,   426,    65,   425,
      97,     0,    97,   150,   252,   169,   154,     0,   244,   272,
     283,   374,     0,   370,   373,   376,     0,   286,   406,   405,
      26,     0,     9,    10,    49,     0,   281,   280,   293,   267,
     270,     0,     0,    72,     0,   369,   366,   354,     0,   359,
     362,   360,     0,     0,   348,     0,     0,    83,   139,   136,
     140,   289,     0,     0,   188,     0,   194,   402,     0,   195,
       0,   400,     0,   192,   193,   401,   413,   441,   169,    80,
     173,   448,   428,    78,    76,   296,   382,     0,   351,   104,
     105,     0,   110,   385,   111,   115,     0,   108,     0,   265,
     257,   259,     0,     0,     0,   152,   396,   256,   336,     0,
       0,   338,   342,     0,     0,   337,   288,   143,     0,     0,
     248,   249,   435,   439,   395,   318,     0,   320,   325,   326,
     309,   322,   321,   313,   312,   211,   213,     0,     0,    79,
      99,   262,     0,     0,     0,     0,    85,     0,   102,     0,
     224,    82,   232,   437,   298,     0,     0,     0,     0,    54,
       0,     0,   206,   208,   167,   203,   367,   361,   350,   332,
     279,   130,   130,   133,   135,   231,   155,   229,   217,     0,
     203,   204,   205,    77,    75,   352,     0,   106,   109,   112,
     393,   266,   397,   398,   339,   334,   341,     0,   343,   143,
     335,   247,     0,   142,   482,   327,     0,   317,   215,   215,
     216,   121,   121,   124,   157,     0,     0,    64,    98,    84,
     101,   207,   218,   220,     0,     0,    81,   233,   235,   372,
     375,   253,    29,     0,    56,   166,     0,     0,   129,     0,
     126,     0,   196,   190,   138,   138,   340,   143,   345,   251,
       0,     0,     0,   323,   324,   319,   212,   214,     0,   120,
       0,   116,     0,   260,     0,   261,     0,   224,     0,   225,
     176,   183,   222,    93,    91,    92,     0,     0,   236,   239,
     414,   234,    53,     0,     0,    44,    55,     0,     0,   131,
     128,   132,     0,     0,   344,     0,   329,   328,     0,   122,
     119,   123,   158,   223,   219,     0,   177,     0,   240,   174,
     198,     0,   237,     0,   238,     0,     0,     0,   291,   127,
     113,   114,     0,     0,   118,   225,   221,   226,     0,   241,
      94,     0,    57,   209,   330,   117,   199,    58
  };

  const short
  parser::yypgoto_[] =
  {
    -693,  -693,  -693,  -693,  -693,  -693,  -693,    51,  -693,  -693,
    -688,  -693,   558,  -693,  -693,  -693,   215,  -156,  -693,   612,
    -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,
    -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,  -693,
    -693,  -693,  -693,  -693,   244,  -286,   425,  -693,  -693,  -399,
    -693,  -693,  -693,   -42,    55,  -693,  -693,   -22,    98,  -693,
    -693,  -195,  -693,  -337,  -560,   736,  -693,  -693,  -693,  -303,
    -425,   412,   131,  -693,   530,  -693,  -130,   336,   -97,  -693,
     -91,  -693,   -90,  -325,  -693,   529,  -692,  -225,   449,   -49,
    -693,   219,   209,    62,  -693,  -693,  -693,    76,    78,  -593,
     143,  -693,    24,  -693,   -10,  -693,  -693,   234,  -693,  -693,
      69,    25,   756,  -506,   466,  -693,   325,  -693,   324,  -693,
     -88,   -87,   760,   -28,  -317,   135,  -693,   -73,   -20,  -693,
    -693,   -83,   681,  -693,  -693,  -693,   105,   334,  -693,   434,
    -412,  -693,   114,  -693,  -176,  -693,  -185,    36,  -693,   524,
    -693,   -78,   607,   266,  -177,  -693,   159,  -693,   746,  -693,
     703,   -75,  -693,   -74,  -249,  -124,  -693,   353,   766,  -693,
    -693,  -693,   -27,  -693,  -137,  -693,   176,   711,   -99,  -693,
    -119,  -693,  -693,  -491,  -693,   589,   -82,   -29,  -208,   -31,
    -693,  -693,   -62,   -53,   -55,    16,  -693,  -207,   -17,   -61,
    -214,  -693,  -190,   -34,   -80
  };

  const short
  parser::yydefgoto_[] =
  {
      -1,     3,     4,     5,    16,   182,     6,    10,    19,    30,
      71,    72,    73,   179,   355,   356,   357,    74,    75,    87,
      11,    20,    21,    32,    85,   185,   525,   362,   724,   775,
     776,   323,   126,   496,    33,    34,   127,   128,   129,   130,
     208,   767,   804,   131,   636,   315,   327,   132,   259,   443,
     581,   677,   133,   749,   750,   703,   629,   728,   729,   663,
     547,   399,   211,   212,   610,    27,    36,   330,   456,   396,
     504,   407,   705,   233,   234,   235,   397,   506,   371,   758,
     372,   800,   318,   759,   238,   239,   760,   240,   398,   801,
     373,   374,   424,   531,   652,   490,   625,   626,   627,   669,
     642,   712,   713,   714,   762,   408,   409,   410,   716,   717,
     718,   768,   400,   401,   465,   466,   467,   135,   267,   268,
     287,   190,   402,   191,   192,   389,   193,   138,   139,   140,
     141,   304,   305,   290,   291,   617,   618,   384,   385,   459,
     600,   601,   602,   688,   201,   202,   203,   603,   379,   277,
     278,   197,   380,   381,   382,   512,   513,   514,   142,   143,
     272,   261,   144,   145,   497,   292,   595,   241,   242,    76,
     243,   769,   157,    78,   244,   245,   498,   499,   367,   293,
     294,   334,   295,   246,   247,   248,   146,   147,    80,    81,
     335,   296,   297,   337,   174,    82,   175,   149,   150,   299,
     300,   151,    24,     9,   310
  };

  const short
  parser::yytable_[] =
  {
      79,   189,    77,   148,   172,   200,   137,   154,   428,   236,
     237,   391,   331,   173,   447,   403,   390,   333,   345,   253,
     585,   386,   358,   387,   256,   503,   316,   288,   288,   288,
     262,   289,   317,   312,   273,   262,   314,   332,   535,   280,
     653,   502,   708,   664,   438,   584,   604,   691,   255,   448,
     530,    22,   319,   306,   309,     1,    13,    22,    22,   298,
     308,   298,   307,   529,   370,   773,   331,   796,   453,   638,
     254,   333,   609,   263,   336,   753,   469,   470,   325,   271,
     314,   552,    17,   257,   609,   265,   806,   264,   419,    22,
     276,   279,   264,   281,   413,   417,   422,   219,   346,    25,
     221,   288,   282,    22,    22,   368,   449,   378,   378,   448,
     378,   463,   754,   510,   464,   553,   249,   821,   339,   454,
     639,   507,   470,   796,    26,   796,   306,   309,   336,   738,
     464,   326,   647,   308,     2,   173,   700,   266,   446,   249,
     198,   411,   198,   285,   558,   420,   425,   450,    79,    79,
      77,    77,    23,    12,   530,   730,   730,   755,    23,    23,
     726,   774,    18,   540,   331,   363,   553,   472,   431,   333,
     523,  -170,   559,   473,   478,   198,   364,   784,   148,   148,
     479,   137,   137,   679,   807,  -153,   189,   684,  -153,   471,
      23,   544,   254,   541,   754,   751,   751,   433,   505,   539,
     426,   549,   390,   474,    23,    23,   477,   180,   172,   181,
     249,   152,   540,   550,   637,   780,   790,   173,    29,   158,
     434,   153,   721,   258,   249,   160,   336,    37,   280,   444,
     316,   359,   360,   158,   117,   155,   317,   159,   451,   160,
     249,   249,   249,   249,    83,   763,    68,   592,   117,   249,
      70,   593,   118,   594,   249,   460,   700,   480,   276,   411,
     339,    31,    68,   481,   282,   653,    70,   548,   605,   779,
     255,   195,   693,   196,   730,   764,   765,   282,   331,   218,
     549,   789,   780,   333,   751,   311,   414,   386,   344,   303,
     220,   493,   494,   302,   790,   209,   619,   210,   536,   303,
     568,   570,   766,   332,   509,   285,   302,    35,   682,   286,
     254,   515,   303,    68,   303,   568,   551,    70,   285,   229,
     230,   810,   517,   231,   232,    84,   164,   165,   166,   704,
     704,   766,     7,   167,   549,   249,     8,    14,    15,   484,
     336,   561,    68,   249,   249,   485,    70,   485,   457,   575,
     458,   578,   578,   331,   249,   168,   439,   537,   333,   170,
     440,   204,   205,   206,   207,   358,   659,   583,   262,   578,
     578,   635,   640,   606,   486,   607,   378,   487,   479,   488,
      38,   481,   249,   485,   615,   378,   620,   288,   825,   288,
      86,   621,   288,   622,   288,   563,   623,   218,   624,   534,
     630,   237,   654,   316,   655,   178,   344,   565,   220,   317,
     582,   485,   176,   444,   690,   336,   587,   740,   741,   298,
     448,   298,   665,   177,   298,   264,   298,   689,   668,   586,
     670,   183,   671,   164,   165,   166,   184,   229,   230,   683,
     167,   231,   232,   303,   418,   423,   249,   249,   320,   321,
     656,   792,   378,   186,   658,   378,   345,   660,   661,   194,
     662,   249,   168,   198,   434,   687,   701,   734,   702,   735,
     815,   666,   817,   411,   266,   579,   580,   269,   586,   322,
     340,   342,   283,   341,   343,   347,  -447,   361,   348,   761,
     282,   649,    79,   349,    77,   350,   351,   352,   365,   198,
     392,   164,   165,   166,   390,   668,   388,   249,   167,   393,
     797,   578,   394,   420,   425,   395,   249,   568,   148,   744,
     725,   137,   254,   404,   254,   430,   495,   431,   695,   405,
     168,   285,   437,   406,   761,   435,   436,   445,   452,   782,
     783,   781,   442,   455,   475,   448,   489,   249,   249,   491,
     249,   444,   326,   476,   501,   482,  -467,   675,   483,   288,
     518,   519,   791,   720,   520,   521,   797,   522,   249,   526,
     761,   524,   761,   527,   249,   532,   249,   533,   249,   534,
    -353,   255,   538,   545,   282,   328,  -149,   542,   543,  -149,
     761,   298,   761,   811,   546,   164,   165,   166,   578,   737,
     282,   448,   167,   554,   739,   555,   556,   557,   378,   745,
     562,   164,   165,   166,   560,   573,   515,   564,   167,   566,
     329,   567,   571,   572,   168,   285,   574,   576,   170,   286,
     148,   148,   589,   137,   137,   117,   329,   590,   591,   588,
     168,   285,   597,   249,   170,   286,   612,   668,   249,   249,
     613,   249,   785,   786,   787,   614,  -448,   628,   631,   632,
     633,   788,   634,   249,   331,   643,   249,   249,   645,   333,
     148,   148,   644,   137,   137,   799,   237,   646,   676,   673,
     674,   680,   678,   568,   685,   609,   686,   681,   770,   772,
     808,   412,   692,   697,   640,   711,   710,   282,   366,   696,
     698,   715,   722,   148,   148,   723,   137,   137,   164,   165,
     166,   732,   820,   237,   733,   167,   742,   757,   777,   778,
     812,   795,   818,   803,   824,   819,   336,   799,   237,   805,
     249,   822,   823,   329,   827,   353,   648,   168,   285,   324,
     770,   170,   286,   282,   328,    79,   641,    77,   814,   148,
     500,    28,   137,   516,   164,   165,   166,   752,   809,   148,
     731,   167,   137,   249,   706,   429,   569,   432,   492,   826,
     249,   672,   249,   756,   746,   249,    79,   747,    77,   329,
     709,   794,   249,   168,   285,   816,   771,   170,   286,   667,
     134,   611,   802,   249,   136,   727,   508,   313,   608,   528,
     736,   743,   462,   383,   719,   161,   657,   260,   596,   156,
     707,   249,   252,   249,     0,     0,   427,     0,     0,   249,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   249,
       0,   249,     0,   249,   249,    88,    39,    89,    90,    91,
       0,    92,     0,    40,    93,     0,     0,    94,    95,    96,
      97,    98,     0,    99,     0,    42,     0,   100,     0,   101,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,   104,   105,    60,    61,    62,    63,    64,    65,   106,
       0,     0,   282,   813,   107,   108,     0,     0,     0,     0,
       0,     0,     0,   164,   165,   166,     0,     0,   109,     0,
     167,     0,     0,     0,   110,     0,     0,     0,     0,     0,
     111,     0,   112,   113,     0,     0,     0,     0,   329,     0,
       0,     0,   168,   285,     0,   114,   170,   286,     0,   115,
       0,   116,     0,     0,     0,     0,     0,     0,     0,    67,
     117,     0,     0,    69,   118,     0,     0,     0,     0,   119,
     120,   121,   122,     0,     0,     0,     0,     0,     0,   123,
       0,   124,   125,    88,    39,    89,     0,   748,     0,    92,
       0,    40,    93,     0,     0,    94,    95,    96,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,     0,    58,     0,     0,   104,
     105,    60,    61,    62,    63,    64,    65,   106,     0,     0,
     162,     0,   107,   108,     0,     0,     0,     0,     0,   163,
       0,   164,   165,   166,     0,     0,   109,     0,   167,     0,
       0,     0,   110,     0,     0,     0,     0,     0,   111,     0,
     112,   113,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,     0,   114,   170,   171,     0,   115,     0,   116,
       0,     0,     0,     0,     0,     0,     0,    67,   117,     0,
       0,    69,   118,     0,     0,     0,     0,   119,   120,   121,
     122,     0,     0,     0,     0,    88,    39,    89,     0,   124,
     125,    92,     0,    40,    93,     0,     0,    94,    95,    96,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,   104,   105,    60,    61,    62,    63,    64,    65,   106,
       0,     0,     0,     0,   107,   108,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,   110,     0,     0,     0,     0,     0,
     111,     0,   112,   113,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,   115,
       0,   116,     0,     0,     0,     0,     0,     0,     0,    67,
     117,     0,     0,    69,   118,     0,     0,     0,     0,   119,
     120,   121,   122,     0,     0,    22,     0,    88,    39,    89,
       0,   124,   125,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,     0,   105,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,   107,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,   111,     0,   112,   577,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    23,   114,     0,     0,
       0,   188,     0,   116,     0,     0,     0,   599,     0,     0,
       0,    67,   117,     0,     0,    69,   118,     0,     0,     0,
       0,   119,   120,   121,   122,    88,    39,    89,     0,     0,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,     0,   105,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,   107,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   282,     0,     0,   110,     0,     0,     0,     0,     0,
     111,     0,   301,   165,   166,     0,     0,     0,     0,   167,
       0,     0,     0,     0,     0,   114,     0,     0,     0,   188,
     302,   116,     0,     0,     0,     0,   303,   284,     0,    67,
     117,   168,   285,    69,   118,   170,   286,     0,     0,   119,
     120,   121,   122,    88,    39,    89,     0,     0,     0,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,     0,    58,     0,     0,     0,
     105,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,   107,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   282,
       0,     0,   110,     0,     0,     0,     0,     0,   111,     0,
     112,   165,   166,     0,     0,     0,     0,   167,     0,     0,
       0,     0,     0,   114,     0,     0,     0,   188,     0,   116,
     311,     0,     0,     0,   303,   284,     0,    67,   117,   168,
     285,    69,   118,   170,   286,     0,     0,   119,   120,   121,
     122,    88,    39,    89,     0,     0,     0,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,   103,     0,    58,     0,     0,     0,   105,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     107,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   282,     0,     0,
     110,     0,     0,     0,     0,     0,   111,     0,   112,   165,
     166,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,   114,   283,     0,     0,   188,     0,   116,     0,     0,
       0,     0,     0,   284,     0,    67,   117,   168,   285,    69,
     118,   170,   286,     0,     0,   119,   120,   121,   122,    88,
      39,    89,     0,     0,     0,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,    58,     0,     0,     0,   105,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,   107,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,   110,     0,
       0,     0,     0,     0,   111,     0,   112,   165,   166,     0,
       0,     0,     0,   167,     0,     0,     0,     0,     0,   114,
       0,     0,     0,   188,     0,   116,     0,     0,     0,     0,
       0,   284,     0,    67,   117,   168,   285,    69,   118,   170,
     286,     0,     0,   119,   120,   121,   122,    88,    39,    89,
       0,     0,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,     0,   105,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,   107,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,   111,     0,   112,   577,     0,     0,     0,     0,
       0,     0,     0,     0,   598,     0,     0,   114,     0,     0,
       0,   188,     0,   116,     0,     0,     0,   599,     0,     0,
       0,    67,   117,     0,     0,    69,   118,     0,     0,     0,
       0,   119,   120,   121,   122,    88,    39,    89,     0,     0,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,     0,   105,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,   107,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,   198,     0,     0,     0,
     111,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,   199,     0,     0,     0,   114,     0,     0,     0,   188,
       0,   116,     0,     0,     0,     0,     0,     0,     0,    67,
     117,     0,     0,    69,   118,     0,     0,     0,     0,   119,
     120,   121,   122,    88,    39,    89,     0,     0,     0,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,   375,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,   376,    58,     0,     0,     0,
     105,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,   107,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,   111,     0,
     112,   377,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,   188,     0,   116,
       0,     0,     0,     0,     0,     0,     0,    67,   117,     0,
       0,    69,   118,     0,     0,     0,     0,   119,   120,   121,
     122,    88,    39,    89,     0,     0,     0,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,   103,     0,    58,     0,     0,     0,   105,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     107,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,   457,     0,   458,
       0,   114,     0,     0,     0,   188,     0,   116,     0,     0,
       0,     0,     0,     0,     0,    67,   117,     0,     0,    69,
     118,     0,     0,     0,     0,   119,   120,   121,   122,    88,
      39,    89,     0,     0,     0,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,   375,     0,     0,     0,    42,
     616,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,    58,     0,     0,     0,   105,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,   107,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,   111,     0,   112,   377,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,   188,     0,   116,     0,     0,     0,     0,
       0,     0,     0,    67,   117,     0,     0,    69,   118,     0,
       0,     0,     0,   119,   120,   121,   122,    88,    39,    89,
       0,     0,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,     0,   105,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,   107,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,   111,     0,   112,   577,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   188,     0,   116,     0,     0,     0,   599,     0,     0,
       0,    67,   117,     0,     0,    69,   118,     0,     0,     0,
       0,   119,   120,   121,   122,    88,    39,    89,     0,     0,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,   375,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,     0,   105,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,   107,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,     0,     0,
     111,     0,   112,   377,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,   188,
       0,   116,     0,     0,     0,     0,     0,     0,     0,    67,
     117,     0,     0,    69,   118,     0,     0,     0,     0,   119,
     120,   121,   122,    88,    39,    89,     0,     0,     0,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,     0,    58,     0,     0,     0,
     105,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,   107,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,   111,     0,
     112,   577,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,   188,     0,   116,
       0,     0,     0,     0,     0,     0,     0,    67,   117,     0,
       0,    69,   118,     0,     0,     0,     0,   119,   120,   121,
     122,    88,    39,    89,     0,     0,     0,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,   103,     0,    58,     0,     0,     0,   105,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
     107,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,   111,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,     0,     0,     0,   188,     0,   116,     0,     0,
       0,     0,     0,     0,     0,    67,   117,     0,     0,    69,
     118,     0,     0,     0,     0,   119,   120,   121,   122,    88,
      39,    89,     0,     0,     0,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,   694,     0,     0,     0,   105,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,   107,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,   111,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,   188,     0,   116,     0,     0,     0,     0,
       0,     0,     0,    67,   117,     0,     0,    69,   118,     0,
       0,     0,     0,   119,   120,   121,   122,    88,    39,    89,
       0,     0,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,     0,   105,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,   111,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   188,     0,   116,     0,     0,     0,     0,     0,     0,
       0,    67,   117,     0,     0,    69,   118,     0,     0,     0,
       0,   119,   120,   121,   122,    88,    39,    89,     0,     0,
       0,    92,     0,    40,    93,     0,     0,     0,     0,     0,
       0,    98,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,   102,    51,
      52,    53,    54,    55,    56,    57,   103,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,     0,   461,     0,
     111,     0,     0,   275,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,   188,
       0,   116,     0,     0,     0,     0,     0,     0,     0,    67,
     117,     0,     0,    69,   118,     0,     0,     0,     0,   119,
     120,   121,   122,    88,    39,   274,     0,     0,     0,    92,
       0,    40,    93,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,    42,     0,     0,     0,     0,    44,     0,
      45,    46,    47,    48,    49,    50,   102,    51,    52,    53,
      54,    55,    56,    57,   103,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,   111,     0,
       0,   275,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,   188,     0,   116,
       0,     0,     0,     0,     0,     0,     0,    67,   117,     0,
       0,    69,   118,     0,     0,     0,     0,   119,   120,   121,
     122,    88,    39,    89,     0,     0,     0,    92,     0,    40,
      93,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,   102,    51,    52,    53,    54,    55,
      56,    57,   103,     0,    58,     0,     0,     0,   105,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,   111,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,     0,     0,     0,   188,     0,   116,     0,     0,
       0,     0,     0,     0,     0,    67,   117,     0,     0,    69,
     118,     0,     0,     0,     0,   119,   120,   121,   122,    88,
      39,    89,     0,     0,     0,    92,     0,    40,    93,     0,
       0,     0,     0,     0,     0,    98,     0,     0,     0,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,   102,    51,    52,    53,    54,    55,    56,    57,
     103,     0,    58,     0,     0,     0,     0,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,   111,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,     0,     0,     0,     0,   114,
       0,     0,     0,   188,     0,   116,     0,     0,     0,     0,
       0,     0,     0,    67,   117,     0,     0,    69,   118,     0,
       0,     0,     0,   119,   120,   121,   122,    88,    39,    89,
       0,     0,     0,    92,     0,    40,    93,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,    42,     0,     0,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
     102,    51,    52,    53,    54,    55,    56,    57,   103,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,   111,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   188,     0,   116,     0,     0,    39,     0,     0,     0,
       0,    67,   117,    40,     0,    69,   118,     0,     0,     0,
       0,   119,   120,   121,   122,    42,     0,     0,     0,     0,
     369,     0,    45,    46,    47,   213,   214,   215,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   216,
     217,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   218,     0,     0,     0,     0,     0,     0,   414,     0,
     415,     0,   220,   221,   222,     0,     0,     0,     0,     0,
       0,   223,     0,     0,     0,   224,     0,     0,    39,   225,
     416,   226,     0,     0,     0,    40,   303,   227,     0,   228,
      68,   229,   230,     0,    70,   231,   232,    42,     0,     0,
       0,     0,   369,     0,    45,    46,    47,   213,   214,   215,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,   217,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   218,     0,     0,     0,     0,     0,     0,
       0,     0,   219,     0,   220,   221,   222,     0,     0,     0,
       0,     0,     0,   223,     0,     0,     0,   224,     0,     0,
      39,   225,     0,   226,   421,     0,     0,    40,   303,   227,
       0,   228,    68,   229,   230,     0,    70,   231,   232,    42,
       0,     0,     0,     0,   369,     0,    45,    46,    47,   213,
     214,   215,     0,     0,     0,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,     0,    60,    61,    62,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   216,   217,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   218,     0,     0,     0,     0,
       0,     0,     0,     0,   219,     0,   220,   221,   222,     0,
       0,     0,     0,     0,     0,   223,     0,     0,     0,   224,
     412,     0,    39,   225,     0,   226,     0,     0,     0,    40,
       0,   227,     0,   228,    68,   229,   230,     0,    70,   231,
     232,    42,     0,     0,     0,     0,   369,     0,    45,    46,
      47,   213,   214,   215,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   216,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   218,     0,     0,
       0,     0,     0,     0,     0,     0,   219,     0,   220,   221,
     222,     0,     0,     0,     0,     0,     0,   223,     0,     0,
       0,   224,     0,     0,    39,   225,     0,   226,     0,     0,
       0,    40,     0,   227,     0,   228,    68,   229,   230,     0,
      70,   231,   232,    42,     0,     0,     0,     0,     0,     0,
      45,    46,    47,   213,   214,   215,     0,     0,     0,    53,
      54,    55,    56,    57,     0,     0,    58,     0,     0,     0,
       0,    60,    61,    62,    63,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   216,   217,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   218,
     635,     0,     0,     0,     0,     0,     0,     0,   219,     0,
     220,   221,   222,     0,     0,     0,     0,     0,     0,   223,
       0,     0,     0,   224,     0,     0,    39,   225,     0,   226,
       0,     0,     0,    40,     0,   227,     0,   228,    68,   229,
     230,     0,    70,   231,   232,    42,     0,     0,     0,     0,
       0,     0,    45,    46,    47,   213,   214,   215,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   216,
     217,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   218,     0,     0,     0,     0,     0,     0,     0,     0,
     219,     0,   220,   221,   222,     0,     0,     0,     0,     0,
       0,   223,     0,     0,     0,   224,     0,     0,    39,   225,
     798,   226,     0,     0,     0,    40,     0,   227,     0,   228,
      68,   229,   230,     0,    70,   231,   232,    42,     0,     0,
       0,     0,     0,     0,    45,    46,    47,   213,   214,   215,
       0,     0,     0,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   216,   217,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   218,     0,     0,     0,     0,     0,     0,
       0,     0,   219,     0,   220,   221,   222,     0,     0,     0,
       0,     0,     0,   223,     0,     0,     0,   224,     0,   468,
      39,   225,     0,   226,     0,     0,     0,    40,     0,   227,
       0,   228,    68,   229,   230,     0,    70,   231,   232,    42,
       0,     0,     0,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    55,    56,    57,
       0,     0,    58,     0,     0,     0,    39,    60,    61,    62,
      63,    64,    65,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,     0,     0,
       0,     0,    45,    46,    47,   213,   214,   215,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   216,
     217,     0,     0,    67,   117,     0,     0,    69,   118,     0,
       0,   282,     0,     0,     0,     0,     0,     0,     0,     0,
     219,  -171,     0,   221,   222,     0,     0,     0,     0,    39,
       0,   223,     0,     0,     0,   224,    40,     0,     0,   225,
       0,   226,     0,     0,     0,     0,     0,   446,    42,   228,
      68,     0,   285,     0,    70,    45,    46,    47,   213,   214,
     215,     0,     0,     0,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,     0,    60,    61,    62,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   216,   217,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   282,     0,     0,     0,     0,     0,
       0,     0,     0,   219,     0,     0,   221,   222,     0,     0,
       0,     0,    39,     0,   223,     0,     0,     0,   224,    40,
       0,     0,   225,     0,   226,     0,     0,     0,     0,     0,
     446,    42,   228,    68,     0,   285,     0,    70,    45,    46,
      47,   213,   214,   215,     0,     0,     0,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   216,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   219,     0,     0,   221,
     222,     0,     0,     0,     0,     0,     0,   223,     0,     0,
       0,   224,    39,     0,     0,   225,     0,   226,     0,    40,
       0,     0,     0,     0,     0,   228,    68,     0,    41,     0,
      70,    42,     0,    43,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,    59,    39,    60,
      61,    62,    63,    64,    65,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    42,     0,    43,
       0,     0,    44,     0,    45,    46,    47,    48,    49,    50,
       0,    51,    52,    53,    54,    55,    56,    57,     0,     0,
      58,     0,     0,     0,     0,    60,    61,    62,    63,    64,
      65,     0,     0,     0,     0,    66,    39,     0,     0,     0,
       0,     0,     0,    40,     0,    67,    68,     0,     0,    69,
      70,     0,   354,     0,     0,    42,     0,     0,     0,     0,
       0,     0,    45,    46,    47,   213,   214,   215,     0,     0,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,    66,    39,    60,    61,    62,    63,    64,    65,    40,
       0,    67,    68,     0,     0,    69,    70,     0,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,     0,     0,     0,     0,    39,
       0,     0,     0,     0,     0,     0,    40,     0,     0,   228,
      68,     0,     0,     0,    70,     0,   511,     0,    42,     0,
       0,     0,     0,    44,     0,    45,    46,    47,    48,    49,
      50,     0,    51,    52,    53,    54,    55,    56,    57,     0,
       0,    58,     0,     0,     0,   270,    60,    61,    62,    63,
      64,    65,     0,     0,     0,    67,    39,     0,     0,    69,
       0,     0,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,   270,    60,    61,    62,    63,    64,    65,     0,
       0,     0,    67,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   164,   165,   166,     0,    39,     0,     0,   167,
       0,     0,     0,    40,     0,     0,     0,     0,     0,   250,
       0,     0,     0,     0,     0,    42,     0,   251,     0,    67,
      44,   168,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    39,    60,    61,    62,    63,    64,    65,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,     0,     0,     0,     0,    44,     0,    45,    46,
      47,    48,    49,    50,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,   158,    39,     0,     0,   258,
       0,   160,     0,    40,     0,     0,     0,     0,     0,    67,
     117,     0,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
      39,     0,     0,     0,     0,    67,   117,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    42,
       0,     0,  -384,     0,    44,     0,    45,    46,    47,    48,
      49,    50,     0,    51,    52,    53,    54,    55,    56,    57,
       0,   441,    58,     0,     0,     0,    39,    60,    61,    62,
      63,    64,    65,    40,     0,     0,   442,     0,     0,    67,
       0,     0,     0,     0,     0,    42,     0,     0,     0,     0,
      44,     0,    45,    46,    47,    48,    49,    50,     0,    51,
      52,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,    39,    60,    61,    62,    63,    64,    65,    40,
       0,     0,     0,   250,     0,     0,     0,     0,     0,     0,
       0,    42,     0,    67,     0,     0,     0,     0,    45,    46,
      47,   213,   214,   215,     0,     0,    39,    53,    54,    55,
      56,    57,     0,    40,    58,     0,     0,     0,     0,    60,
      61,    62,    63,    64,    65,    42,     0,     0,     0,     0,
       0,     0,    45,    46,    47,   213,   214,   215,     0,    67,
       0,    53,    54,    55,    56,    57,     0,     0,    58,     0,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,     0,     0,   650,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   651,    39,     0,     0,     0,
       0,     0,     0,    40,     0,   228,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    42,     0,     0,     0,   793,
       0,     0,    45,    46,    47,   213,   214,   215,     0,   651,
       0,    53,    54,    55,    56,    57,     0,     0,    58,   228,
       0,     0,     0,    60,    61,    62,    63,    64,    65,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    42,     0,     0,     0,     0,     0,   699,    45,
      46,    47,   213,   214,   215,     0,     0,     0,    53,    54,
      55,    56,    57,     0,     0,    58,     0,     0,     0,     0,
      60,    61,    62,    63,    64,    65,     0,     0,     0,   228,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   228
  };

  const short
  parser::yycheck_[] =
  {
      29,    89,    29,    34,    66,    93,    34,    41,   233,   100,
     100,   201,   136,    66,   263,   210,   201,   136,   155,   101,
     445,   198,   178,   199,   102,   328,   123,   114,   115,   116,
     104,   114,   123,   116,   109,   109,   116,   136,   375,   112,
     531,   327,   635,   549,   258,   444,   458,   607,   101,   263,
     367,     1,    18,   115,   115,    20,     5,     1,     1,   114,
     115,   116,   115,   366,   194,    11,   190,   759,    77,    77,
     101,   190,    26,   104,   136,    77,   284,   284,    18,   108,
     160,    80,   100,   103,    26,   105,   774,   104,   225,     1,
     110,   111,   109,   113,   224,   225,   226,    88,   159,   103,
      91,   188,    79,     1,     1,   193,    88,   195,   196,   323,
     198,    81,   114,   338,    84,   114,   100,   805,   138,   128,
     128,   329,   329,   815,   128,   817,   188,   188,   190,   689,
      84,    71,    80,   188,    99,   188,   627,   103,   115,   123,
      84,   223,    84,   120,    80,   225,   226,   129,   177,   178,
     177,   178,   102,     0,   471,   661,   662,    77,   102,   102,
     651,   107,   113,   113,   288,   117,   114,    78,    86,   288,
     360,    89,   108,    84,   108,    84,   128,   737,   209,   210,
     114,   209,   210,   582,   777,    81,   274,   599,    84,   288,
     102,   100,   223,   383,   114,   701,   702,   250,   328,   100,
     227,   113,   387,   114,   102,   102,   293,    99,   270,   101,
     194,   104,   113,   403,   500,   113,   113,   270,   107,   103,
     251,   114,   647,   107,   208,   109,   288,    77,   301,   260,
     327,   180,   181,   103,   118,   107,   327,   107,   267,   109,
     224,   225,   226,   227,    26,    21,   118,   103,   118,   233,
     122,   107,   122,   109,   238,   275,   747,   108,   278,   341,
     280,    13,   118,   114,    79,   756,   122,   100,   458,   100,
     323,    99,   609,   101,   780,    51,    52,    79,   402,    79,
     113,   100,   113,   402,   790,   110,    86,   464,    88,   114,
      90,   320,   321,   108,   113,    99,   473,   101,   376,   114,
     430,   431,   107,   402,   332,   120,   108,   128,   108,   124,
     341,   340,   114,   118,   114,   445,   404,   122,   120,   119,
     120,   100,   342,   123,   124,    60,    90,    91,    92,   632,
     633,   107,   118,    97,   113,   319,   122,    65,    66,   108,
     402,   108,   118,   327,   328,   114,   122,   114,    99,   437,
     101,   439,   440,   477,   338,   119,    81,   377,   477,   123,
      85,    72,    73,    74,    75,   521,   543,   442,   442,   457,
     458,    80,    81,   461,   110,   463,   464,   110,   114,   110,
      77,   114,   366,   114,   472,   473,   474,   474,   813,   476,
     113,   474,   479,   476,   481,   110,   479,    79,   481,   114,
     491,   491,   532,   500,   534,   107,    88,   110,    90,   500,
     441,   114,   108,   444,   604,   477,   447,    47,    48,   474,
     634,   476,   552,   114,   479,   442,   481,   603,   558,   446,
     560,    77,   562,    90,    91,    92,    33,   119,   120,   110,
      97,   123,   124,   114,   225,   226,   430,   431,   124,   125,
     538,   754,   540,   113,   542,   543,   593,   545,    99,   107,
     101,   445,   119,    84,   495,    86,    99,    99,   101,   101,
     795,   553,   797,   555,   103,   439,   440,   128,   495,   129,
      99,    87,   104,   114,   128,   108,   114,    23,   108,   714,
      79,   525,   521,   108,   521,   108,   108,   108,    22,    84,
      77,    90,    91,    92,   689,   635,   113,   491,    97,    77,
     759,   599,    77,   593,   594,    77,   500,   647,   549,   696,
     650,   549,   553,    14,   555,    81,   115,    86,   616,   138,
     119,   120,    86,   138,   759,   129,   129,    80,    77,   734,
     735,   731,   114,    80,   104,   759,    84,   531,   532,    89,
     534,   582,    71,   114,   128,   108,   108,   577,   108,   646,
     108,   108,   752,   646,   108,   114,   815,   100,   552,    77,
     795,   128,   797,    77,   558,    89,   560,   108,   562,   114,
      85,   634,    85,    24,    79,    80,    81,    86,   114,    84,
     815,   646,   817,   783,    26,    90,    91,    92,   686,   687,
      79,   815,    97,   100,   692,   114,   104,   108,   696,   697,
      84,    90,    91,    92,   114,   128,   645,   110,    97,   115,
     115,   115,   108,   115,   119,   120,   128,   108,   123,   124,
     661,   662,   104,   661,   662,   118,   115,    77,    77,   129,
     119,   120,    77,   627,   123,   124,   115,   777,   632,   633,
     115,   635,   740,   741,   742,   115,   114,    26,    77,    80,
      80,   748,   114,   647,   788,   115,   650,   651,   114,   788,
     701,   702,   100,   701,   702,   766,   766,    81,    26,   128,
     128,   115,   100,   813,   100,    26,   113,   104,   715,   723,
     778,   104,    81,    78,    81,    28,    77,    79,    80,   114,
     114,     8,   114,   734,   735,     4,   734,   735,    90,    91,
      92,   108,   803,   803,   108,    97,    47,    84,    80,    10,
      48,    89,   114,    53,   812,   108,   788,   818,   818,   107,
     714,   108,   108,   115,   108,   177,   521,   119,   120,   127,
     767,   123,   124,    79,    80,   774,   502,   774,   790,   780,
     325,    15,   780,   341,    90,    91,    92,   702,   780,   790,
     662,    97,   790,   747,   633,   235,   430,   238,   319,   818,
     754,   562,   756,   711,   698,   759,   805,   699,   805,   115,
     637,   757,   766,   119,   120,   795,   717,   123,   124,   555,
      34,   466,   767,   777,    34,   660,   330,   116,   464,   365,
     686,   696,   278,   196,   645,    59,   540,   104,   455,    43,
     634,   795,   101,   797,    -1,    -1,   227,    -1,    -1,   803,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,
      -1,   815,    -1,   817,   818,     3,     4,     5,     6,     7,
      -1,     9,    -1,    11,    12,    -1,    -1,    15,    16,    17,
      18,    19,    -1,    21,    -1,    23,    -1,    25,    -1,    27,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    79,    80,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    92,    -1,    -1,    76,    -1,
      97,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    -1,    -1,    -1,    -1,   115,    -1,
      -1,    -1,   119,   120,    -1,   103,   123,   124,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,    -1,    -1,    -1,    -1,    -1,   137,
      -1,   139,   140,     3,     4,     5,    -1,     7,    -1,     9,
      -1,    11,    12,    -1,    -1,    15,    16,    17,    -1,    19,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    -1,    -1,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      79,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    88,
      -1,    90,    91,    92,    -1,    -1,    76,    -1,    97,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,    -1,   103,   123,   124,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,    -1,    -1,    -1,    -1,     3,     4,     5,    -1,   139,
     140,     9,    -1,    11,    12,    -1,    -1,    15,    16,    17,
      -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    -1,
      -1,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,    -1,    -1,     1,    -1,     3,     4,     5,
      -1,   139,   140,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    -1,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,     3,     4,     5,    -1,    -1,
      -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    -1,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
     108,   109,    -1,    -1,    -1,    -1,   114,   115,    -1,   117,
     118,   119,   120,   121,   122,   123,   124,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,     9,
      -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    -1,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
     110,    -1,    -1,    -1,   114,   115,    -1,   117,   118,   119,
     120,   121,   122,   123,   124,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,   103,   104,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,   115,    -1,   117,   118,   119,   120,   121,
     122,   123,   124,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,
      -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,   115,    -1,   117,   118,   119,   120,   121,   122,   123,
     124,    -1,    -1,   127,   128,   129,   130,     3,     4,     5,
      -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    -1,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,     3,     4,     5,    -1,    -1,
      -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    -1,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    84,    -1,    -1,    -1,
      88,    -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,     9,
      -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,
      24,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,     3,     4,     5,
      -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    -1,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,   113,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,     3,     4,     5,    -1,    -1,
      -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    -1,
      -1,    -1,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,     9,
      -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    -1,    -1,    -1,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,
      -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    -1,    -1,    -1,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,     3,     4,     5,
      -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    -1,    -1,    -1,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,     3,     4,     5,    -1,    -1,
      -1,     9,    -1,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    86,    -1,
      88,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,   121,   122,    -1,    -1,    -1,    -1,   127,
     128,   129,   130,     3,     4,     5,    -1,    -1,    -1,     9,
      -1,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    28,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    -1,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,   107,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   117,   118,    -1,
      -1,   121,   122,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,     3,     4,     5,    -1,    -1,    -1,     9,    -1,    11,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,   107,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    -1,    -1,    -1,   127,   128,   129,   130,     3,
       4,     5,    -1,    -1,    -1,     9,    -1,    11,    12,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,
      -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    -1,    -1,   127,   128,   129,   130,     3,     4,     5,
      -1,    -1,    -1,     9,    -1,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    -1,    -1,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,   107,    -1,   109,    -1,    -1,     4,    -1,    -1,    -1,
      -1,   117,   118,    11,    -1,   121,   122,    -1,    -1,    -1,
      -1,   127,   128,   129,   130,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,
     108,   109,    -1,    -1,    -1,    11,   114,   115,    -1,   117,
     118,   119,   120,    -1,   122,   123,   124,    23,    -1,    -1,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,    -1,
       4,   107,    -1,   109,   110,    -1,    -1,    11,   114,   115,
      -1,   117,   118,   119,   120,    -1,   122,   123,   124,    23,
      -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    -1,    -1,    39,    40,    41,    42,    43,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    90,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,   103,
     104,    -1,     4,   107,    -1,   109,    -1,    -1,    -1,    11,
      -1,   115,    -1,   117,   118,   119,   120,    -1,   122,   123,
     124,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    90,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,   103,    -1,    -1,     4,   107,    -1,   109,    -1,    -1,
      -1,    11,    -1,   115,    -1,   117,   118,   119,   120,    -1,
     122,   123,   124,    23,    -1,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    -1,    -1,    -1,    39,
      40,    41,    42,    43,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    79,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,
      90,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,    -1,    -1,     4,   107,    -1,   109,
      -1,    -1,    -1,    11,    -1,   115,    -1,   117,   118,   119,
     120,    -1,   122,   123,   124,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    90,    91,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,   103,    -1,    -1,     4,   107,
     108,   109,    -1,    -1,    -1,    11,    -1,   115,    -1,   117,
     118,   119,   120,    -1,   122,   123,   124,    23,    -1,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,    -1,    90,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,   103,    -1,     3,
       4,   107,    -1,   109,    -1,    -1,    -1,    11,    -1,   115,
      -1,   117,   118,   119,   120,    -1,   122,   123,   124,    23,
      -1,    -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    46,    -1,    -1,    -1,     4,    51,    52,    53,
      54,    55,    56,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      68,    -1,    -1,   117,   118,    -1,    -1,   121,   122,    -1,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    89,    -1,    91,    92,    -1,    -1,    -1,    -1,     4,
      -1,    99,    -1,    -1,    -1,   103,    11,    -1,    -1,   107,
      -1,   109,    -1,    -1,    -1,    -1,    -1,   115,    23,   117,
     118,    -1,   120,    -1,   122,    30,    31,    32,    33,    34,
      35,    -1,    -1,    -1,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,     4,    -1,    99,    -1,    -1,    -1,   103,    11,
      -1,    -1,   107,    -1,   109,    -1,    -1,    -1,    -1,    -1,
     115,    23,   117,   118,    -1,   120,    -1,   122,    30,    31,
      32,    33,    34,    35,    -1,    -1,    -1,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,   103,     4,    -1,    -1,   107,    -1,   109,    -1,    11,
      -1,    -1,    -1,    -1,    -1,   117,   118,    -1,    20,    -1,
     122,    23,    -1,    25,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    49,     4,    51,
      52,    53,    54,    55,    56,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    25,
      -1,    -1,    28,    -1,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    41,    42,    43,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,   107,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    11,    -1,   117,   118,    -1,    -1,   121,
     122,    -1,    78,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,   107,     4,    51,    52,    53,    54,    55,    56,    11,
      -1,   117,   118,    -1,    -1,   121,   122,    -1,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,    -1,   117,
     118,    -1,    -1,    -1,   122,    -1,    78,    -1,    23,    -1,
      -1,    -1,    -1,    28,    -1,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    -1,    -1,    -1,   107,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,   117,     4,    -1,    -1,   121,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,   107,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,   117,    -1,    -1,    -1,   121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    91,    92,    -1,     4,    -1,    -1,    97,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    23,    -1,   115,    -1,   117,
      28,   119,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,     4,    51,    52,    53,    54,    55,    56,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    -1,    28,    -1,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,   103,     4,    -1,    -1,   107,
      -1,   109,    -1,    11,    -1,    -1,    -1,    -1,    -1,   117,
     118,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
       4,    -1,    -1,    -1,    -1,   117,   118,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    23,
      -1,    -1,    80,    -1,    28,    -1,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    41,    42,    43,
      -1,    99,    46,    -1,    -1,    -1,     4,    51,    52,    53,
      54,    55,    56,    11,    -1,    -1,   114,    -1,    -1,   117,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      28,    -1,    30,    31,    32,    33,    34,    35,    -1,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,     4,    51,    52,    53,    54,    55,    56,    11,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    -1,   117,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    -1,    -1,     4,    39,    40,    41,
      42,    43,    -1,    11,    46,    -1,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,   117,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   107,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    11,    -1,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,    97,
      -1,    -1,    30,    31,    32,    33,    34,    35,    -1,   107,
      -1,    39,    40,    41,    42,    43,    -1,    -1,    46,   117,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    -1,    -1,    -1,    -1,    -1,    86,    30,
      31,    32,    33,    34,    35,    -1,    -1,    -1,    39,    40,
      41,    42,    43,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117
  };

  const unsigned short
  parser::yystos_[] =
  {
       0,    20,    99,   142,   143,   144,   147,   118,   122,   344,
     148,   161,     0,   148,    65,    66,   145,   100,   113,   149,
     162,   163,     1,   102,   343,   103,   128,   206,   206,   107,
     150,    13,   164,   175,   176,   128,   207,    77,    77,     4,
      11,    20,    23,    25,    28,    30,    31,    32,    33,    34,
      35,    37,    38,    39,    40,    41,    42,    43,    46,    49,
      51,    52,    53,    54,    55,    56,   107,   117,   118,   121,
     122,   151,   152,   153,   158,   159,   310,   313,   314,   328,
     329,   330,   336,    26,    60,   165,   113,   160,     3,     5,
       6,     7,     9,    12,    15,    16,    17,    18,    19,    21,
      25,    27,    36,    44,    49,    50,    57,    62,    63,    76,
      82,    88,    90,    91,   103,   107,   109,   118,   122,   127,
     128,   129,   130,   137,   139,   140,   173,   177,   178,   179,
     180,   184,   188,   193,   253,   258,   263,   264,   268,   269,
     270,   271,   299,   300,   303,   304,   327,   328,   330,   338,
     339,   342,   104,   114,   344,   107,   309,   313,   103,   107,
     109,   299,    79,    88,    90,    91,    92,    97,   119,   120,
     123,   124,   333,   334,   335,   337,   108,   114,   107,   154,
      99,   101,   146,    77,    33,   166,   113,    63,   107,   261,
     262,   264,   265,   267,   107,    99,   101,   292,    84,    99,
     261,   285,   286,   287,    72,    73,    74,    75,   181,    99,
     101,   203,   204,    33,    34,    35,    67,    68,    79,    88,
      90,    91,    92,    99,   103,   107,   109,   115,   117,   119,
     120,   123,   124,   214,   215,   216,   221,   223,   225,   226,
     228,   308,   309,   311,   315,   316,   324,   325,   326,   336,
     107,   115,   318,   327,   330,   334,   292,   269,   107,   189,
     301,   302,   304,   330,   339,   269,   103,   259,   260,   128,
     107,   328,   301,   302,     5,    91,   269,   290,   291,   269,
     268,   269,    79,   104,   115,   120,   124,   261,   262,   272,
     274,   275,   306,   320,   321,   323,   332,   333,   335,   340,
     341,    90,   108,   114,   272,   273,   333,   334,   335,   340,
     345,   110,   272,   273,   345,   186,   219,   221,   223,    18,
     259,   259,   129,   172,   160,    18,    71,   187,    80,   115,
     208,   306,   319,   321,   322,   331,   333,   334,    98,   269,
      99,   114,    87,   128,    88,   315,   340,   108,   108,   108,
     108,   108,   108,   153,    78,   155,   156,   157,   158,   148,
     148,    23,   168,   117,   128,    22,    80,   319,   261,    28,
     217,   219,   221,   231,   232,    19,    45,    91,   261,   289,
     293,   294,   295,   293,   278,   279,   295,   285,   113,   266,
     287,   343,    77,    77,    77,    77,   210,   217,   229,   202,
     253,   254,   263,   202,    14,   138,   138,   212,   246,   247,
     248,   327,   104,   217,    86,    88,   108,   217,   232,   315,
     345,   110,   217,   232,   233,   345,   313,   326,   228,   215,
      81,    86,   226,   334,   330,   129,   129,    86,   341,    81,
      85,    99,   114,   190,   330,    80,   115,   305,   341,    88,
     129,   328,    77,    77,   128,    80,   209,    99,   101,   280,
     269,    86,   290,    81,    84,   255,   256,   257,     3,   329,
     338,   319,    78,    84,   114,   104,   114,   262,   108,   114,
     108,   114,   108,   108,   108,   114,   110,   110,   110,    84,
     236,    89,   229,   328,   328,   115,   174,   305,   317,   318,
     187,   128,   186,   210,   211,   217,   218,   329,   255,   264,
     228,    78,   296,   297,   298,   328,   212,   269,   108,   108,
     108,   114,   100,   343,   128,   167,    77,    77,   280,   210,
     265,   234,    89,   108,   114,   204,   292,   269,    85,   100,
     113,   343,    86,   114,   100,    24,    26,   201,   100,   113,
     343,   261,    80,   114,   100,   114,   104,   108,    80,   108,
     114,   108,    84,   110,   110,   110,   115,   115,   217,   218,
     217,   108,   115,   128,   128,   261,   108,    91,   261,   288,
     288,   191,   330,   302,   190,   211,   339,   330,   129,   104,
      77,    77,   103,   107,   109,   307,   308,    77,   100,   113,
     281,   282,   283,   288,   281,   343,   261,   261,   278,    26,
     205,   257,   115,   115,   115,   261,    24,   276,   277,   295,
     261,   272,   272,   272,   272,   237,   238,   239,    26,   197,
     221,    77,    80,    80,   114,    80,   185,   186,    77,   128,
      81,   185,   241,   115,   100,   114,    81,    80,   157,   344,
      97,   107,   235,   324,   217,   217,   261,   294,   261,   295,
     261,    99,   101,   200,   254,   217,   327,   248,   217,   240,
     217,   217,   233,   128,   128,   269,    26,   192,   100,   190,
     115,   104,   108,   110,   281,   100,   113,    86,   284,   285,
     343,   205,    81,   204,    46,   261,   114,    78,   114,    86,
     324,    99,   101,   196,   210,   213,   213,   317,   240,   241,
      77,    28,   242,   243,   244,     8,   249,   250,   251,   297,
     272,   211,   114,     4,   169,   217,   324,   266,   198,   199,
     254,   199,   108,   108,    99,   101,   283,   261,   205,   261,
      47,    48,    47,   277,   295,   261,   238,   239,     7,   194,
     195,   254,   195,    77,   114,    77,   234,    84,   220,   224,
     227,   228,   245,    21,    51,    52,   107,   182,   252,   312,
     313,   251,   344,    11,   107,   170,   171,    80,    10,   100,
     113,   343,   202,   202,   205,   261,   261,   261,   262,   100,
     113,   343,   210,    97,   243,    89,   227,   305,   108,   221,
     222,   230,   252,    53,   183,   107,   151,   240,   261,   198,
     100,   343,    48,    80,   194,   224,   245,   224,   114,   108,
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
     282,   282,   282,   283,   284,   284,   285,   285,   286,   286,
     287,   288,   288,   289,   289,   290,   290,   291,   291,   292,
     292,   293,   293,   293,   293,   294,   294,   295,   295,   295,
     296,   296,   297,   297,   297,   298,   298,   299,   299,   300,
     300,   301,   301,   301,   302,   302,   303,   303,   303,   303,
     304,   304,   305,   305,   306,   306,   307,   307,   307,   308,
     308,   308,   308,   308,   309,   309,   309,   310,   310,   310,
     310,   310,   311,   311,   312,   313,   313,   314,   315,   315,
     315,   316,   316,   316,   316,   317,   317,   318,   318,   319,
     319,   319,   320,   320,   320,   321,   322,   322,   323,   323,
     324,   325,   326,   326,   326,   326,   326,   327,   327,   328,
     328,   328,   329,   329,   330,   330,   330,   330,   330,   330,
     330,   330,   331,   331,   332,   332,   333,   334,   334,   335,
     335,   336,   336,   336,   336,   336,   336,   336,   336,   336,
     336,   336,   336,   336,   336,   336,   336,   336,   336,   337,
     337,   337,   338,   338,   339,   340,   340,   341,   341,   342,
     342,   342,   342,   343,   343,   344,   344,   345,   345
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
       3,     2,     1,     2,     3,     2,     2,     1,     3,     2,
       4,     1,     2,     1,     2,     1,     2,     2,     1,     3,
       3,     3,     2,     1,     0,     1,     2,     3,     1,     2,
       1,     0,     3,     1,     1,     3,     1,     1,     1,     1,
       3,     1,     3,     1,     1,     3,     2,     3,     2,     3,
       1,     2,     1,     3,     1,     3,     1,     2,     2,     1,
       3,     3,     3,     2,     1,     3,     3,     1,     3,     3,
       3,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     1,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     1,     1,     1,     1,     1,     3,     1,
       3,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1
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

#if YYDEBUG
  const unsigned short
  parser::yyrline_[] =
  {
       0,   499,   499,   516,   517,   519,   523,   524,   525,   527,
     528,   530,   531,   534,   536,   537,   538,   546,   547,   549,
     551,   552,   554,   555,   556,   558,   559,   561,   562,   564,
     565,   567,   568,   570,   571,   573,   574,   578,   579,   581,
     582,   584,   586,   587,   589,   598,   599,   601,   602,   604,
     605,   607,   608,   610,   611,   613,   614,   616,   617,   622,
     623,   625,   626,   627,   629,   630,   634,   636,   637,   639,
     640,   641,   644,   651,   652,   653,   654,   655,   656,   658,
     660,   662,   663,   666,   668,   669,   671,   672,   673,   674,
     675,   677,   678,   679,   681,   721,   722,   724,   725,   734,
     735,   737,   738,   739,   756,   757,   758,   760,   761,   762,
     764,   765,   767,   769,   770,   773,   777,   778,   780,   781,
     782,   783,   785,   786,   788,   789,   791,   793,   794,   795,
     796,   798,   799,   801,   802,   804,   805,   806,   807,   809,
     810,   812,   818,   819,   827,   828,   830,   831,   832,   840,
     841,   843,   844,   846,   848,   850,   851,   853,   854,   858,
     859,   860,   862,   863,   865,   866,   868,   869,   871,   873,
     882,   884,   886,   887,   889,   892,   894,   895,   897,   898,
     900,   901,   902,   908,   910,   911,   912,   913,   914,   915,
     916,   917,   918,   919,   920,   921,   922,   925,   927,   928,
     930,   931,   933,   934,   936,   937,   939,   940,   942,   943,
     945,   946,   948,   949,   951,   953,   954,   958,   964,   966,
     967,   969,   970,   972,   973,   975,   976,   978,   979,   981,
     982,   984,   986,   987,   989,   990,   992,   993,   994,   996,
     997,   998,  1003,  1005,  1007,  1008,  1011,  1015,  1016,  1018,
    1019,  1023,  1025,  1026,  1027,  1028,  1029,  1030,  1031,  1032,
    1033,  1034,  1035,  1037,  1038,  1040,  1041,  1045,  1046,  1048,
    1049,  1051,  1052,  1054,  1055,  1056,  1058,  1059,  1062,  1063,
    1065,  1066,  1070,  1071,  1072,  1073,  1075,  1076,  1077,  1078,
    1079,  1080,  1081,  1082,  1083,  1084,  1085,  1086,  1088,  1089,
    1091,  1092,  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1104,
    1105,  1106,  1111,  1112,  1130,  1131,  1132,  1133,  1134,  1135,
    1136,  1138,  1139,  1151,  1152,  1153,  1154,  1156,  1157,  1158,
    1159,  1162,  1164,  1165,  1168,  1169,  1170,  1171,  1173,  1174,
    1176,  1177,  1178,  1180,  1182,  1183,  1185,  1186,  1188,  1189,
    1191,  1193,  1194,  1196,  1197,  1199,  1200,  1202,  1203,  1206,
    1207,  1209,  1210,  1211,  1212,  1217,  1218,  1220,  1221,  1222,
    1227,  1228,  1230,  1231,  1232,  1234,  1235,  1267,  1268,  1270,
    1271,  1273,  1274,  1275,  1277,  1278,  1280,  1281,  1282,  1283,
    1285,  1286,  1288,  1289,  1291,  1292,  1295,  1296,  1297,  1299,
    1300,  1301,  1302,  1303,  1305,  1306,  1307,  1309,  1310,  1311,
    1312,  1313,  1316,  1317,  1319,  1321,  1322,  1326,  1328,  1329,
    1330,  1332,  1333,  1334,  1335,  1340,  1341,  1343,  1344,  1346,
    1347,  1348,  1350,  1351,  1352,  1354,  1356,  1357,  1359,  1360,
    1364,  1366,  1368,  1369,  1370,  1371,  1372,  1375,  1376,  1378,
    1379,  1380,  1382,  1383,  1385,  1386,  1387,  1388,  1389,  1390,
    1391,  1392,  1394,  1395,  1397,  1398,  1400,  1402,  1403,  1405,
    1406,  1408,  1409,  1410,  1411,  1412,  1413,  1414,  1415,  1416,
    1417,  1418,  1419,  1420,  1421,  1422,  1423,  1424,  1425,  1427,
    1428,  1429,  1433,  1434,  1436,  1438,  1439,  1441,  1442,  1446,
    1447,  1448,  1449,  1454,  1457,  1461,  1462,  1464,  1465
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
#line 5805 "parser.cc" // lalr1.cc:1181
#line 1474 "parser.y" // lalr1.cc:1182


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
