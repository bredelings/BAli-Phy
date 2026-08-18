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


// Take the name prefix into account.
#define yylex   zzlex



#include "parser.hh"


// Unqualified %code blocks.
#line 41 "parser.y"

# include "driver.hh"
# include "parse.H"
# include "util/myexception.H"

CM::UntypedExpr make_call(const std::string& name, const std::vector<CM::Arg<CM::NoAnn>>& args);
CM::UntypedExpr make_list(const std::vector<CM::Arg<CM::NoAnn>>& args);
CM::UntypedExpr make_list(const std::vector<CM::UntypedExpr>& elements);
CM::UntypedExpr make_sample(const CM::UntypedExpr& dist);
CM::UntypedExpr make_model_tuple(const std::vector<CM::UntypedExpr>& elements);

#line 60 "parser.cc"


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
#if ZZDEBUG

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

#else // !ZZDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YY_USE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !ZZDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

#line 6 "parser.y"
namespace zz {
#line 153 "parser.cc"

  /// Build a parser object.
  parser::parser (zz_driver& drv_yyarg)
#if ZZDEBUG
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
      case symbol_kind::S_arg: // arg
        value.YY_MOVE_OR_COPY< CM::Arg<CM::NoAnn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_defs: // defs
        value.YY_MOVE_OR_COPY< CM::Decls<CM::NoAnn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.YY_MOVE_OR_COPY< CM::Type > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_infix_exp: // infix_exp
      case symbol_kind::S_prefix_exp: // prefix_exp
      case symbol_kind::S_atom: // atom
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< CM::UntypedExpr > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_pattern: // pattern
        value.YY_MOVE_OR_COPY< CM::UntypedPattern > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix_operator: // infix_operator
        value.YY_MOVE_OR_COPY< Located<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_def: // def
        value.YY_MOVE_OR_COPY< std::pair<std::string,CM::UntypedExpr> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_args: // args
        value.YY_MOVE_OR_COPY< std::vector<CM::Arg<CM::NoAnn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.YY_MOVE_OR_COPY< std::vector<CM::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.YY_MOVE_OR_COPY< std::vector<CM::UntypedExpr> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.YY_MOVE_OR_COPY< std::vector<CM::UntypedPattern> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix_terms: // infix_terms
        value.YY_MOVE_OR_COPY< std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_arg: // arg
        value.move< CM::Arg<CM::NoAnn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_defs: // defs
        value.move< CM::Decls<CM::NoAnn> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.move< CM::Type > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_infix_exp: // infix_exp
      case symbol_kind::S_prefix_exp: // prefix_exp
      case symbol_kind::S_atom: // atom
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.move< CM::UntypedExpr > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_pattern: // pattern
        value.move< CM::UntypedPattern > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix_operator: // infix_operator
        value.move< Located<std::string> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_def: // def
        value.move< std::pair<std::string,CM::UntypedExpr> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.move< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_args: // args
        value.move< std::vector<CM::Arg<CM::NoAnn>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.move< std::vector<CM::Type> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<CM::UntypedExpr> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.move< std::vector<CM::UntypedPattern> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_infix_terms: // infix_terms
        value.move< std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_arg: // arg
        value.copy< CM::Arg<CM::NoAnn> > (that.value);
        break;

      case symbol_kind::S_defs: // defs
        value.copy< CM::Decls<CM::NoAnn> > (that.value);
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.copy< CM::Type > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_infix_exp: // infix_exp
      case symbol_kind::S_prefix_exp: // prefix_exp
      case symbol_kind::S_atom: // atom
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.copy< CM::UntypedExpr > (that.value);
        break;

      case symbol_kind::S_pattern: // pattern
        value.copy< CM::UntypedPattern > (that.value);
        break;

      case symbol_kind::S_infix_operator: // infix_operator
        value.copy< Located<std::string> > (that.value);
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_def: // def
        value.copy< std::pair<std::string,CM::UntypedExpr> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.copy< std::string > (that.value);
        break;

      case symbol_kind::S_args: // args
        value.copy< std::vector<CM::Arg<CM::NoAnn>> > (that.value);
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.copy< std::vector<CM::Type> > (that.value);
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.copy< std::vector<CM::UntypedExpr> > (that.value);
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.copy< std::vector<CM::UntypedPattern> > (that.value);
        break;

      case symbol_kind::S_infix_terms: // infix_terms
        value.copy< std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > (that.value);
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
      case symbol_kind::S_arg: // arg
        value.move< CM::Arg<CM::NoAnn> > (that.value);
        break;

      case symbol_kind::S_defs: // defs
        value.move< CM::Decls<CM::NoAnn> > (that.value);
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.move< CM::Type > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_infix_exp: // infix_exp
      case symbol_kind::S_prefix_exp: // prefix_exp
      case symbol_kind::S_atom: // atom
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.move< CM::UntypedExpr > (that.value);
        break;

      case symbol_kind::S_pattern: // pattern
        value.move< CM::UntypedPattern > (that.value);
        break;

      case symbol_kind::S_infix_operator: // infix_operator
        value.move< Located<std::string> > (that.value);
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (that.value);
        break;

      case symbol_kind::S_def: // def
        value.move< std::pair<std::string,CM::UntypedExpr> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.move< std::string > (that.value);
        break;

      case symbol_kind::S_args: // args
        value.move< std::vector<CM::Arg<CM::NoAnn>> > (that.value);
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.move< std::vector<CM::Type> > (that.value);
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<CM::UntypedExpr> > (that.value);
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.move< std::vector<CM::UntypedPattern> > (that.value);
        break;

      case symbol_kind::S_infix_terms: // infix_terms
        value.move< std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > (that.value);
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

#if ZZDEBUG
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

#if ZZDEBUG
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
#endif // ZZDEBUG

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
      case symbol_kind::S_arg: // arg
        yylhs.value.emplace< CM::Arg<CM::NoAnn> > ();
        break;

      case symbol_kind::S_defs: // defs
        yylhs.value.emplace< CM::Decls<CM::NoAnn> > ();
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        yylhs.value.emplace< CM::Type > ();
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_infix_exp: // infix_exp
      case symbol_kind::S_prefix_exp: // prefix_exp
      case symbol_kind::S_atom: // atom
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< CM::UntypedExpr > ();
        break;

      case symbol_kind::S_pattern: // pattern
        yylhs.value.emplace< CM::UntypedPattern > ();
        break;

      case symbol_kind::S_infix_operator: // infix_operator
        yylhs.value.emplace< Located<std::string> > ();
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        yylhs.value.emplace< double > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_def: // def
        yylhs.value.emplace< std::pair<std::string,CM::UntypedExpr> > ();
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_VARSYM: // "VARSYM"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_QVARSYM: // "QVARSYM"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        yylhs.value.emplace< std::string > ();
        break;

      case symbol_kind::S_args: // args
        yylhs.value.emplace< std::vector<CM::Arg<CM::NoAnn>> > ();
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        yylhs.value.emplace< std::vector<CM::Type> > ();
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        yylhs.value.emplace< std::vector<CM::UntypedExpr> > ();
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        yylhs.value.emplace< std::vector<CM::UntypedPattern> > ();
        break;

      case symbol_kind::S_infix_terms: // infix_terms
        yylhs.value.emplace< std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > ();
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
  case 2: // start: START_EXP exp
#line 132 "parser.y"
                     {drv.expression_result = yystack_[0].value.as < CM::UntypedExpr > ();}
#line 927 "parser.cc"
    break;

  case 3: // start: START_TYPE type
#line 133 "parser.y"
                       {drv.type_result = yystack_[0].value.as < CM::Type > ();}
#line 933 "parser.cc"
    break;

  case 4: // start: START_DEFS defs
#line 134 "parser.y"
                       {drv.defs_result = yystack_[0].value.as < CM::Decls<CM::NoAnn> > ();}
#line 939 "parser.cc"
    break;

  case 5: // def: varid "=" exp
#line 136 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,CM::UntypedExpr> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < CM::UntypedExpr > ()}; }
#line 945 "parser.cc"
    break;

  case 6: // def: fncall "=" exp
#line 137 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,CM::UntypedExpr> > () = make_function_def(drv,yystack_[2].location,yystack_[2].value.as < CM::UntypedExpr > (),yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 951 "parser.cc"
    break;

  case 7: // def: varid "~" exp
#line 138 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,CM::UntypedExpr> > () = {yystack_[2].value.as < std::string > (),make_sample(yystack_[0].value.as < CM::UntypedExpr > ())}; }
#line 957 "parser.cc"
    break;

  case 8: // defs: %empty
#line 140 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = {}; }
#line 963 "parser.cc"
    break;

  case 9: // defs: def
#line 141 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = {yystack_[0].value.as < std::pair<std::string,CM::UntypedExpr> > ()}; }
#line 969 "parser.cc"
    break;

  case 10: // defs: defs ";" def
#line 142 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = yystack_[2].value.as < CM::Decls<CM::NoAnn> > (); yylhs.value.as < CM::Decls<CM::NoAnn> > ().push_back(yystack_[0].value.as < std::pair<std::string,CM::UntypedExpr> > ()); }
#line 975 "parser.cc"
    break;

  case 11: // defs: defs ";"
#line 143 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = yystack_[1].value.as < CM::Decls<CM::NoAnn> > (); }
#line 981 "parser.cc"
    break;

  case 12: // exp: infix_exp
#line 145 "parser.y"
                                   { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 987 "parser.cc"
    break;

  case 13: // exp: exp "where" "{" defs "}"
#line 146 "parser.y"
                                   { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Let<CM::NoAnn>{yystack_[1].value.as < CM::Decls<CM::NoAnn> > (), yystack_[4].value.as < CM::UntypedExpr > ()}}; }
#line 993 "parser.cc"
    break;

  case 14: // infix_exp: prefix_exp
#line 148 "parser.y"
                                   { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 999 "parser.cc"
    break;

  case 15: // infix_exp: prefix_exp infix_terms
#line 149 "parser.y"
                                   { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Infix<CM::NoAnn>{yystack_[1].value.as < CM::UntypedExpr > (), yystack_[0].value.as < std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > ()}}; }
#line 1005 "parser.cc"
    break;

  case 16: // infix_terms: infix_operator prefix_exp
#line 152 "parser.y"
             { yylhs.value.as < std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > () = {{yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < CM::UntypedExpr > ()}}; }
#line 1011 "parser.cc"
    break;

  case 17: // infix_terms: infix_terms infix_operator prefix_exp
#line 154 "parser.y"
             { yylhs.value.as < std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > () = yystack_[2].value.as < std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > (); yylhs.value.as < std::vector<std::pair<Located<std::string>,CM::UntypedExpr>> > ().push_back({yystack_[1].value.as < Located<std::string> > (), yystack_[0].value.as < CM::UntypedExpr > ()}); }
#line 1017 "parser.cc"
    break;

  case 18: // infix_operator: "VARSYM"
#line 156 "parser.y"
                        { yylhs.value.as < Located<std::string> > () = Located<std::string>{yystack_[0].location, yystack_[0].value.as < std::string > ()}; }
#line 1023 "parser.cc"
    break;

  case 19: // infix_operator: "QVARSYM"
#line 157 "parser.y"
                        { yylhs.value.as < Located<std::string> > () = Located<std::string>{yystack_[0].location, yystack_[0].value.as < std::string > ()}; }
#line 1029 "parser.cc"
    break;

  case 20: // infix_operator: "`" "VARID" "`"
#line 158 "parser.y"
                               { yylhs.value.as < Located<std::string> > () = Located<std::string>{yystack_[1].location, yystack_[1].value.as < std::string > ()}; }
#line 1035 "parser.cc"
    break;

  case 21: // infix_operator: "`" "QVARID" "`"
#line 159 "parser.y"
                               { yylhs.value.as < Located<std::string> > () = Located<std::string>{yystack_[1].location, yystack_[1].value.as < std::string > ()}; }
#line 1041 "parser.cc"
    break;

  case 22: // infix_operator: "-"
#line 160 "parser.y"
                        { yylhs.value.as < Located<std::string> > () = Located<std::string>{yystack_[0].location, "-"}; }
#line 1047 "parser.cc"
    break;

  case 23: // infix_operator: "+>"
#line 161 "parser.y"
                        { yylhs.value.as < Located<std::string> > () = Located<std::string>{yystack_[0].location, "+>"}; }
#line 1053 "parser.cc"
    break;

  case 24: // prefix_exp: atom
#line 163 "parser.y"
                                 { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 1059 "parser.cc"
    break;

  case 25: // prefix_exp: "~" prefix_exp
#line 164 "parser.y"
                                 { yylhs.value.as < CM::UntypedExpr > () = make_sample(yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1065 "parser.cc"
    break;

  case 26: // prefix_exp: "-" prefix_exp
#line 165 "parser.y"
                                 { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::PrefixNeg<CM::NoAnn>{{yystack_[1].location, "-"}, yystack_[0].value.as < CM::UntypedExpr > ()}}; }
#line 1071 "parser.cc"
    break;

  case 27: // atom: qvarid
#line 168 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Var{yystack_[0].value.as < std::string > ()}}; }
#line 1077 "parser.cc"
    break;

  case 28: // atom: "@" varid
#line 169 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::ArgRef{yystack_[0].value.as < std::string > ()}}; }
#line 1083 "parser.cc"
    break;

  case 29: // atom: fncall
#line 170 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 1089 "parser.cc"
    break;

  case 30: // atom: "[" args "]"
#line 171 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_list(yystack_[1].value.as < std::vector<CM::Arg<CM::NoAnn>> > ()); }
#line 1095 "parser.cc"
    break;

  case 31: // atom: "[" "]"
#line 172 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::List<CM::NoAnn>{}}; }
#line 1101 "parser.cc"
    break;

  case 32: // atom: "(" tup_args "," exp ")"
#line 173 "parser.y"
                                  { yystack_[3].value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[1].value.as < CM::UntypedExpr > ()); yylhs.value.as < CM::UntypedExpr > () = make_model_tuple(yystack_[3].value.as < std::vector<CM::UntypedExpr> > ()); }
#line 1107 "parser.cc"
    break;

  case 33: // atom: literal
#line 174 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 1113 "parser.cc"
    break;

  case 34: // atom: "{" ditems "}"
#line 175 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Dictionary<CM::NoAnn>{yystack_[1].value.as < std::vector<CM::UntypedExpr> > ()}}; }
#line 1119 "parser.cc"
    break;

  case 35: // atom: "{" "}"
#line 176 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Dictionary<CM::NoAnn>{}}; }
#line 1125 "parser.cc"
    break;

  case 36: // atom: "|" patterns ":" exp "|"
#line 177 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_function(yystack_[3].value.as < std::vector<CM::UntypedPattern> > (), yystack_[1].value.as < CM::UntypedExpr > ());}
#line 1131 "parser.cc"
    break;

  case 37: // atom: "(" exp ")"
#line 178 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Infix<CM::NoAnn>{yystack_[1].value.as < CM::UntypedExpr > (), {}}}; }
#line 1137 "parser.cc"
    break;

  case 38: // atom: "_"
#line 179 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Placeholder{}}; }
#line 1143 "parser.cc"
    break;

  case 39: // patterns: pattern
#line 182 "parser.y"
                            { yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ()); }
#line 1149 "parser.cc"
    break;

  case 40: // patterns: patterns pattern
#line 183 "parser.y"
                             { yylhs.value.as < std::vector<CM::UntypedPattern> > () = yystack_[1].value.as < std::vector<CM::UntypedPattern> > (); yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ()); }
#line 1155 "parser.cc"
    break;

  case 41: // pattern: varid
#line 185 "parser.y"
                                                    { yylhs.value.as < CM::UntypedPattern > () = CM::UntypedPattern{CM::NoAnn{}, CM::VarPattern{yystack_[0].value.as < std::string > ()}}; }
#line 1161 "parser.cc"
    break;

  case 42: // pattern: "(" pattern_tup_args "," pattern ")"
#line 186 "parser.y"
                                                    { yystack_[3].value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[1].value.as < CM::UntypedPattern > ()); yylhs.value.as < CM::UntypedPattern > () = CM::UntypedPattern{CM::NoAnn{}, CM::TuplePattern<CM::NoAnn>{yystack_[3].value.as < std::vector<CM::UntypedPattern> > ()}}; }
#line 1167 "parser.cc"
    break;

  case 43: // pattern_tup_args: pattern
#line 188 "parser.y"
                                                    { yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ());}
#line 1173 "parser.cc"
    break;

  case 44: // pattern_tup_args: pattern_tup_args "," pattern
#line 189 "parser.y"
                                                    { yylhs.value.as < std::vector<CM::UntypedPattern> > () = yystack_[2].value.as < std::vector<CM::UntypedPattern> > (); yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ());}
#line 1179 "parser.cc"
    break;

  case 45: // fncall: qvarid "(" args ")"
#line 191 "parser.y"
                                    { yylhs.value.as < CM::UntypedExpr > () = make_call(yystack_[3].value.as < std::string > (),yystack_[1].value.as < std::vector<CM::Arg<CM::NoAnn>> > ()); }
#line 1185 "parser.cc"
    break;

  case 46: // ditems: ditem
#line 193 "parser.y"
                                  { yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1191 "parser.cc"
    break;

  case 47: // ditems: ditems "," ditem
#line 194 "parser.y"
                                  { yylhs.value.as < std::vector<CM::UntypedExpr> > () = yystack_[2].value.as < std::vector<CM::UntypedExpr> > (); yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1197 "parser.cc"
    break;

  case 48: // ditem: exp ":" exp
#line 196 "parser.y"
                    { yylhs.value.as < CM::UntypedExpr > () = make_model_tuple({yystack_[2].value.as < CM::UntypedExpr > (),yystack_[0].value.as < CM::UntypedExpr > ()}); }
#line 1203 "parser.cc"
    break;

  case 49: // args: arg
#line 198 "parser.y"
                          { yylhs.value.as < std::vector<CM::Arg<CM::NoAnn>> > ().push_back(yystack_[0].value.as < CM::Arg<CM::NoAnn> > ()); }
#line 1209 "parser.cc"
    break;

  case 50: // args: args "," arg
#line 199 "parser.y"
                          { yylhs.value.as < std::vector<CM::Arg<CM::NoAnn>> > () = yystack_[2].value.as < std::vector<CM::Arg<CM::NoAnn>> > (); yylhs.value.as < std::vector<CM::Arg<CM::NoAnn>> > ().push_back(yystack_[0].value.as < CM::Arg<CM::NoAnn> > ()); }
#line 1215 "parser.cc"
    break;

  case 51: // arg: varid "=" exp
#line 201 "parser.y"
                   { yylhs.value.as < CM::Arg<CM::NoAnn> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < CM::UntypedExpr > (),false,false,std::nullopt}; }
#line 1221 "parser.cc"
    break;

  case 52: // arg: varid "~" exp
#line 202 "parser.y"
                   { yylhs.value.as < CM::Arg<CM::NoAnn> > () = {yystack_[2].value.as < std::string > (),make_sample(yystack_[0].value.as < CM::UntypedExpr > ()),false,false,std::nullopt}; }
#line 1227 "parser.cc"
    break;

  case 53: // arg: exp
#line 203 "parser.y"
                   { yylhs.value.as < CM::Arg<CM::NoAnn> > () = {"",yystack_[0].value.as < CM::UntypedExpr > (),false,false,std::nullopt}; }
#line 1233 "parser.cc"
    break;

  case 54: // tup_args: exp
#line 205 "parser.y"
                            { yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ());}
#line 1239 "parser.cc"
    break;

  case 55: // tup_args: tup_args "," exp
#line 206 "parser.y"
                            { yylhs.value.as < std::vector<CM::UntypedExpr> > () = yystack_[2].value.as < std::vector<CM::UntypedExpr> > (); yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ());}
#line 1245 "parser.cc"
    break;

  case 56: // qvarid: varid
#line 211 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1251 "parser.cc"
    break;

  case 57: // qvarid: "QVARID"
#line 212 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1257 "parser.cc"
    break;

  case 58: // qvarid: "(" "QVARSYM" ")"
#line 213 "parser.y"
                        { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 1263 "parser.cc"
    break;

  case 59: // varid: "VARID"
#line 215 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1269 "parser.cc"
    break;

  case 60: // varid: "(" "VARSYM" ")"
#line 216 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 1275 "parser.cc"
    break;

  case 61: // varid: "(" ":" ")"
#line 217 "parser.y"
                    { yylhs.value.as < std::string > () = ":"; }
#line 1281 "parser.cc"
    break;

  case 62: // varid: "(" "-" ")"
#line 218 "parser.y"
                    { yylhs.value.as < std::string > () = "-"; }
#line 1287 "parser.cc"
    break;

  case 63: // literal: "STRING"
#line 220 "parser.y"
                     {yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::StringLiteral{yystack_[0].value.as < std::string > ()}};}
#line 1293 "parser.cc"
    break;

  case 64: // literal: "INTEGER"
#line 221 "parser.y"
                     {yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::IntLiteral{yystack_[0].value.as < int > ()}};}
#line 1299 "parser.cc"
    break;

  case 65: // literal: "FLOAT"
#line 222 "parser.y"
                     {yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::DoubleLiteral{yystack_[0].value.as < double > ()}};}
#line 1305 "parser.cc"
    break;

  case 66: // type: btype
#line 226 "parser.y"
                                        { yylhs.value.as < CM::Type > () = yystack_[0].value.as < CM::Type > (); }
#line 1311 "parser.cc"
    break;

  case 67: // type: btype "->" type
#line 227 "parser.y"
                                        { yylhs.value.as < CM::Type > () = CM::type_apps("Function",{yystack_[2].value.as < CM::Type > (),yystack_[0].value.as < CM::Type > ()});  }
#line 1317 "parser.cc"
    break;

  case 68: // btype: atype
#line 229 "parser.y"
                                        { yylhs.value.as < CM::Type > () = yystack_[0].value.as < CM::Type > (); }
#line 1323 "parser.cc"
    break;

  case 69: // btype: atype "<" type_tup_args ">"
#line 230 "parser.y"
                                        { yylhs.value.as < CM::Type > () = CM::type_apps(yystack_[3].value.as < CM::Type > (), yystack_[1].value.as < std::vector<CM::Type> > ()); }
#line 1329 "parser.cc"
    break;

  case 70: // atype: varid
#line 232 "parser.y"
                                        { yylhs.value.as < CM::Type > () = CM::type_atom(yystack_[0].value.as < std::string > ()); }
#line 1335 "parser.cc"
    break;

  case 71: // atype: "(" type ")"
#line 233 "parser.y"
                                        { yylhs.value.as < CM::Type > () = yystack_[1].value.as < CM::Type > (); }
#line 1341 "parser.cc"
    break;

  case 72: // atype: "(" type_tup_args "," type ")"
#line 234 "parser.y"
                                        { yystack_[3].value.as < std::vector<CM::Type> > ().push_back(yystack_[1].value.as < CM::Type > ()); yylhs.value.as < CM::Type > () = CM::type_apps(CM::type_con("Tuple"),yystack_[3].value.as < std::vector<CM::Type> > ()); }
#line 1347 "parser.cc"
    break;

  case 73: // type_tup_args: type
#line 236 "parser.y"
                                        { yylhs.value.as < std::vector<CM::Type> > ().push_back(yystack_[0].value.as < CM::Type > ());}
#line 1353 "parser.cc"
    break;

  case 74: // type_tup_args: type_tup_args "," type
#line 237 "parser.y"
                                        { yylhs.value.as < std::vector<CM::Type> > () = yystack_[2].value.as < std::vector<CM::Type> > (); yylhs.value.as < std::vector<CM::Type> > ().push_back(yystack_[0].value.as < CM::Type > ());}
#line 1359 "parser.cc"
    break;


#line 1363 "parser.cc"

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


  const signed char parser::yypact_ninf_ = -31;

  const signed char parser::yytable_ninf_ = -1;

  const short
  parser::yypact_[] =
  {
     143,   172,     4,    34,    13,    37,   100,    12,   124,    57,
     172,   172,   -31,   -31,   -31,   -31,   -31,   -31,    46,   -31,
     167,   -31,   -31,    77,   -31,   -31,    38,   -31,   -31,    54,
      70,    48,   -31,    95,   110,    77,    40,   -31,   181,   109,
     -31,   -31,   -31,    46,    45,   -31,    66,   111,   148,   116,
     127,    15,   121,   -31,     8,    91,   -31,     7,   -31,   -31,
     -31,   128,    87,   -31,   -31,   -31,   -31,   167,   172,   172,
     135,   138,   137,     4,     4,    34,   172,   172,   172,   -31,
     147,   172,   -31,   -31,   172,   172,   172,   -31,   -31,   -31,
     -31,   -31,   172,   172,   -31,   172,    34,   141,   146,   172,
     -31,    41,   -31,     4,   -31,   -31,    63,   -31,    46,    46,
      46,    37,   118,   -31,    46,    46,    20,    46,   -31,    84,
     -31,   -31,   -31,   -31,   153,     4,   -31,   156,   -31,   -31,
     -31,   -31,   -31,   -31
  };

  const signed char
  parser::yydefact_[] =
  {
       0,     0,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,    38,    59,    57,    63,    64,    65,     2,    12,
      14,    24,    29,    27,    56,    33,     0,    70,     3,    66,
      68,     0,     9,     4,     0,     0,    56,     1,     0,     0,
      39,    41,    31,    53,     0,    49,    56,     0,     0,     0,
       0,    54,     0,    35,     0,     0,    46,     0,    28,    25,
      26,     0,     0,    22,    23,    18,    19,    15,     0,     0,
       0,    73,     0,     0,     0,    11,     0,     0,     0,    43,
       0,     0,    40,    30,     0,     0,     0,    61,    62,    60,
      58,    37,     0,     0,    34,     0,     8,     0,     0,     0,
      16,     0,    71,     0,    67,    73,     0,    10,     6,     5,
       7,     0,     0,    50,    51,    52,    55,    48,    47,     0,
      20,    21,    17,    45,    74,     0,    69,    44,    36,    32,
      13,    72,    74,    42
  };

  const signed char
  parser::yypgoto_[] =
  {
     -31,   -31,    96,    76,    11,   -31,   -31,   106,     0,   -31,
     -31,   -30,   -31,    -1,   -31,    81,   115,    94,   -31,     2,
      -2,   -31,   -20,   -31,   -31,   122
  };

  const signed char
  parser::yydefgoto_[] =
  {
       0,     4,    32,    33,    43,    19,    67,    68,    20,    21,
      39,    40,    80,    22,    55,    56,    44,    45,    52,    23,
      24,    25,    28,    29,    30,    72
  };

  const unsigned char
  parser::yytable_[] =
  {
      27,    36,    34,    41,    46,    35,    71,    58,    79,    82,
      59,    60,    18,    37,    61,    47,    93,    26,    51,    54,
      47,    61,     5,     6,    27,     7,    61,     8,    70,    91,
       9,    13,    10,    48,   129,    49,    41,    41,    12,    13,
      49,    14,    50,    15,    16,    17,    47,    31,    60,    77,
      38,    26,    61,   104,   105,   123,    47,    83,    84,    70,
      78,    13,    84,    14,    13,    13,    49,    46,   100,    70,
      57,    27,    27,    36,    34,    85,    49,    35,    50,    73,
     125,   127,    46,   124,    13,   126,    86,   108,   109,   110,
      69,    75,   112,    74,    36,    34,   114,   115,    35,   122,
     130,    27,    75,   116,   117,   132,    54,    94,    95,    41,
       5,     6,    42,     7,    97,     8,    98,    81,     9,    76,
      10,    11,    38,    27,    61,    87,    12,    13,   128,    14,
      89,    15,    16,    17,     5,     6,    13,     7,    92,     8,
      53,    90,     9,    96,    10,    11,     1,     2,     3,    88,
      12,    13,   102,    14,   103,    15,    16,    17,     5,     6,
     120,     7,    88,     8,   111,   121,     9,   131,    10,    11,
     133,   107,   119,    99,    12,    13,   118,    14,   113,    15,
      16,    17,     5,     6,   101,     7,    62,     8,    63,    47,
       9,    64,    10,    11,    38,    65,   106,    66,    12,    13,
       0,    14,    70,    15,    16,    17,     0,     0,    13,    49
  };

  const signed char
  parser::yycheck_[] =
  {
       2,     3,     3,     5,     6,     3,    26,     9,    38,    39,
      10,    11,     1,     0,     6,     8,     8,    13,     7,     8,
       8,     6,    10,    11,    26,    13,     6,    15,    21,    14,
      18,    27,    20,    21,    14,    28,    38,    39,    26,    27,
      28,    29,    30,    31,    32,    33,     8,    13,    48,     9,
      13,    13,     6,    73,    74,    14,     8,    12,    17,    21,
      20,    27,    17,    29,    27,    27,    28,    69,    68,    21,
      13,    73,    74,    75,    75,     9,    28,    75,    30,    25,
      17,   111,    84,   103,    27,    22,    20,    76,    77,    78,
      13,     7,    81,    23,    96,    96,    85,    86,    96,    99,
      16,   103,     7,    92,    93,   125,    95,    16,    17,   111,
      10,    11,    12,    13,    27,    15,    29,     8,    18,     9,
      20,    21,    13,   125,     6,    14,    26,    27,    10,    29,
      14,    31,    32,    33,    10,    11,    27,    13,    17,    15,
      16,    14,    18,    15,    20,    21,     3,     4,     5,    14,
      26,    27,    14,    29,    17,    31,    32,    33,    10,    11,
      19,    13,    14,    15,    17,    19,    18,    14,    20,    21,
      14,    75,    96,    67,    26,    27,    95,    29,    84,    31,
      32,    33,    10,    11,    69,    13,    19,    15,    21,     8,
      18,    24,    20,    21,    13,    28,    74,    30,    26,    27,
      -1,    29,    21,    31,    32,    33,    -1,    -1,    27,    28
  };

  const signed char
  parser::yystos_[] =
  {
       0,     3,     4,     5,    35,    10,    11,    13,    15,    18,
      20,    21,    26,    27,    29,    31,    32,    33,    38,    39,
      42,    43,    47,    53,    54,    55,    13,    54,    56,    57,
      58,    13,    36,    37,    47,    53,    54,     0,    13,    44,
      45,    54,    12,    38,    50,    51,    54,     8,    21,    28,
      30,    38,    52,    16,    38,    48,    49,    13,    54,    42,
      42,     6,    19,    21,    24,    28,    30,    40,    41,    13,
      21,    56,    59,    25,    23,     7,     9,     9,    20,    45,
      46,     8,    45,    12,    17,     9,    20,    14,    14,    14,
      14,    14,    17,     8,    16,    17,    15,    27,    29,    41,
      42,    50,    14,    17,    56,    56,    59,    36,    38,    38,
      38,    17,    38,    51,    38,    38,    38,    38,    49,    37,
      19,    19,    42,    14,    56,    17,    22,    45,    10,    14,
      16,    14,    56,    14
  };

  const signed char
  parser::yyr1_[] =
  {
       0,    34,    35,    35,    35,    36,    36,    36,    37,    37,
      37,    37,    38,    38,    39,    39,    40,    40,    41,    41,
      41,    41,    41,    41,    42,    42,    42,    43,    43,    43,
      43,    43,    43,    43,    43,    43,    43,    43,    43,    44,
      44,    45,    45,    46,    46,    47,    48,    48,    49,    50,
      50,    51,    51,    51,    52,    52,    53,    53,    53,    54,
      54,    54,    54,    55,    55,    55,    56,    56,    57,    57,
      58,    58,    58,    59,    59
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     2,     2,     2,     3,     3,     3,     0,     1,
       3,     2,     1,     5,     1,     2,     2,     3,     1,     1,
       3,     3,     1,     1,     1,     2,     2,     1,     2,     1,
       3,     2,     5,     1,     3,     2,     5,     3,     1,     1,
       2,     1,     5,     1,     3,     4,     1,     3,     3,     1,
       3,     3,     3,     1,     1,     3,     1,     1,     3,     1,
       3,     3,     3,     1,     1,     1,     1,     3,     1,     4,
       1,     3,     5,     1,     3
  };


#if ZZDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "START_EXP",
  "START_TYPE", "START_DEFS", "\"where\"", "\";\"", "\":\"", "\"=\"",
  "\"|\"", "\"[\"", "\"]\"", "\"(\"", "\")\"", "\"{\"", "\"}\"", "\",\"",
  "\"@\"", "\"`\"", "\"~\"", "\"-\"", "\">\"", "\"<\"", "\"+>\"", "\"->\"",
  "\"_\"", "\"VARID\"", "\"VARSYM\"", "\"QVARID\"", "\"QVARSYM\"",
  "\"STRING\"", "\"INTEGER\"", "\"FLOAT\"", "$accept", "start", "def",
  "defs", "exp", "infix_exp", "infix_terms", "infix_operator",
  "prefix_exp", "atom", "patterns", "pattern", "pattern_tup_args",
  "fncall", "ditems", "ditem", "args", "arg", "tup_args", "qvarid",
  "varid", "literal", "type", "btype", "atype", "type_tup_args", YY_NULLPTR
  };
#endif


#if ZZDEBUG
  const unsigned char
  parser::yyrline_[] =
  {
       0,   132,   132,   133,   134,   136,   137,   138,   140,   141,
     142,   143,   145,   146,   148,   149,   151,   153,   156,   157,
     158,   159,   160,   161,   163,   164,   165,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   182,
     183,   185,   186,   188,   189,   191,   193,   194,   196,   198,
     199,   201,   202,   203,   205,   206,   211,   212,   213,   215,
     216,   217,   218,   220,   221,   222,   226,   227,   229,   230,
     232,   233,   234,   236,   237
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
#endif // ZZDEBUG


#line 6 "parser.y"
} // zz
#line 1935 "parser.cc"

#line 245 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
zz::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}

// Builds one ordinary call expression, handling parser-level special forms that
// used to be recognized by ptree conversion.
CM::UntypedExpr make_call(const string& name, const vector<CM::Arg<CM::NoAnn>>& args)
{
    if (name == "get_state")
    {
        if (args.size() != 1)
            throw myexception()<<"get_state: got "<<args.size()<<" arguments, 1 argument required.";
        if (not args[0].name.empty() or not args[0].value)
            throw myexception()<<"get_state: first argument must be an unquoted state name.";
        auto& state = *args[0].value;
        if (auto var = state.to<CM::Var>())
            return {CM::NoAnn{}, CM::GetState{var->name}};
        if (auto str = state.to<CM::StringLiteral>())
            return {CM::NoAnn{}, CM::GetState{str->value}};
        throw myexception()<<"get_state: first argument must be an unquoted state name.";
    }

    return {CM::NoAnn{}, CM::Call<CM::NoAnn>{name, args}};
}

// Builds a list expression from parser argument syntax, preserving the old
// parser behavior that ignored names inside list syntax.
CM::UntypedExpr make_list(const vector<CM::Arg<CM::NoAnn>>& args)
{
    CM::List<CM::NoAnn> list;
    for(auto& arg: args)
    {
        if (not arg.value)
            throw myexception()<<"List element must have a value.";
        list.elements.push_back(*arg.value);
    }
    return {CM::NoAnn{}, std::move(list)};
}

// Builds a list expression from already parsed element expressions.
CM::UntypedExpr make_list(const vector<CM::UntypedExpr>& elements)
{
    return {CM::NoAnn{}, CM::List<CM::NoAnn>{elements}};
}

// Builds a tuple expression after checking the tuple arity.
CM::UntypedExpr make_model_tuple(const vector<CM::UntypedExpr>& elements)
{
    if (elements.size() < 2)
        throw myexception()<<"Tuple's of 1 element not allowed.";
    return {CM::NoAnn{}, CM::Tuple<CM::NoAnn>{elements}};
}

// Builds one sample-sugar expression.
CM::UntypedExpr make_sample(const CM::UntypedExpr& dist)
{
    return {CM::NoAnn{}, CM::Sample<CM::NoAnn>{dist}};
}

// Builds nested unary lambda nodes for the parser's lambda syntax.
CM::UntypedExpr make_function(const vector<CM::UntypedPattern>& patterns, const CM::UntypedExpr& body)
{
    auto f = body;
    for(auto& pattern: patterns | views::reverse)
    {
        f = {
            CM::NoAnn{},
            CM::Lambda<CM::NoAnn>{std::move(pattern), std::move(f)}
        };
    }
    return f;
}

// Converts a function-definition left-hand side into nested lambda binders,
// reporting parser errors for non-variable argument patterns.
pair<string,CM::UntypedExpr> make_function_def(zz_driver& drv, const yy::location& l, const CM::UntypedExpr& fncall, const CM::UntypedExpr& body)
{
    auto call = fncall.to<CM::Call<CM::NoAnn>>();
    assert(call);

    auto fname = call->function;
    if (fname.find('.') != string::npos)
	drv.push_error_message(l, "Function name cannot contain '.'");

    vector<CM::UntypedPattern> patterns;
    for(auto& arg: call->args)
    {
	if (not arg.name.empty())
	    drv.push_error_message(l, "Named arguments not allowed in function definitions");

        if (not arg.value)
	    drv.push_error_message(l, "Arguments in function definition must be variables");
	else if (auto var = arg.value->to<CM::Var>())
	    patterns.push_back(CM::UntypedPattern{CM::NoAnn{}, CM::VarPattern{var->name}});
	else
	    drv.push_error_message(l, "Arguments in function definition must be variables");
    }
    
    return {fname, make_function(patterns, body)};
}
