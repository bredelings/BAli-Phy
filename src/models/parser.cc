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
#line 34 "parser.y"

# include "driver.hh"
# include "parse.H"

ptree fold_terms(const std::vector<ptree>& terms);

#line 55 "parser.cc"


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
#line 148 "parser.cc"

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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< ptree > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_arg: // arg
        value.YY_MOVE_OR_COPY< std::pair<std::string,ptree> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.YY_MOVE_OR_COPY< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_terms: // terms
        value.YY_MOVE_OR_COPY< std::vector<ptree> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.YY_MOVE_OR_COPY< std::vector<std::pair<std::string,ptree>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.move< ptree > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_arg: // arg
        value.move< std::pair<std::string,ptree> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.move< std::string > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_terms: // terms
        value.move< std::vector<ptree> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<std::pair<std::string,ptree>> > (YY_MOVE (that.value));
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.copy< ptree > (that.value);
        break;

      case symbol_kind::S_arg: // arg
        value.copy< std::pair<std::string,ptree> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.copy< std::string > (that.value);
        break;

      case symbol_kind::S_terms: // terms
        value.copy< std::vector<ptree> > (that.value);
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.copy< std::vector<std::pair<std::string,ptree>> > (that.value);
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (that.value);
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.move< ptree > (that.value);
        break;

      case symbol_kind::S_arg: // arg
        value.move< std::pair<std::string,ptree> > (that.value);
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        value.move< std::string > (that.value);
        break;

      case symbol_kind::S_terms: // terms
        value.move< std::vector<ptree> > (that.value);
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<std::pair<std::string,ptree>> > (that.value);
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        yylhs.value.emplace< double > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< ptree > ();
        break;

      case symbol_kind::S_arg: // arg
        yylhs.value.emplace< std::pair<std::string,ptree> > ();
        break;

      case symbol_kind::S_VARID: // "VARID"
      case symbol_kind::S_QVARID: // "QVARID"
      case symbol_kind::S_STRING: // "STRING"
      case symbol_kind::S_qvarid: // qvarid
      case symbol_kind::S_varid: // varid
        yylhs.value.emplace< std::string > ();
        break;

      case symbol_kind::S_terms: // terms
        yylhs.value.emplace< std::vector<ptree> > ();
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        yylhs.value.emplace< std::vector<std::pair<std::string,ptree>> > ();
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
  case 2: // unit: exp
#line 90 "parser.y"
          {drv.result = yystack_[0].value.as < ptree > ();}
#line 727 "parser.cc"
    break;

  case 3: // exp: terms
#line 93 "parser.y"
                                              { yylhs.value.as < ptree > () = fold_terms(yystack_[0].value.as < std::vector<ptree> > ()); }
#line 733 "parser.cc"
    break;

  case 4: // exp: "(" exp ")"
#line 94 "parser.y"
                                              { yylhs.value.as < ptree > () = yystack_[1].value.as < ptree > (); }
#line 739 "parser.cc"
    break;

  case 5: // exp: varid "=" exp ";" exp
#line 95 "parser.y"
                                              { yylhs.value.as < ptree > () = ptree("let",{{yystack_[4].value.as < std::string > (),yystack_[2].value.as < ptree > ()},{"",yystack_[0].value.as < ptree > ()}}); }
#line 745 "parser.cc"
    break;

  case 6: // terms: term
#line 97 "parser.y"
                            { yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 751 "parser.cc"
    break;

  case 7: // terms: terms "+" term
#line 98 "parser.y"
                            { yylhs.value.as < std::vector<ptree> > () = yystack_[2].value.as < std::vector<ptree> > (); yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 757 "parser.cc"
    break;

  case 8: // terms: terms "+>" term
#line 99 "parser.y"
                             { yylhs.value.as < std::vector<ptree> > () = yystack_[2].value.as < std::vector<ptree> > (); yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 763 "parser.cc"
    break;

  case 9: // term: qvarid
#line 102 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < std::string > ()); }
#line 769 "parser.cc"
    break;

  case 10: // term: "@" varid
#line 103 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("@"+yystack_[0].value.as < std::string > ()); }
#line 775 "parser.cc"
    break;

  case 11: // term: qvarid "[" args "]"
#line 104 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree(yystack_[3].value.as < std::string > (),yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 781 "parser.cc"
    break;

  case 12: // term: qvarid "[" "," args "]"
#line 105 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree(yystack_[4].value.as < std::string > ()); yylhs.value.as < ptree > ().push_back({}); yylhs.value.as < ptree > ().insert(yylhs.value.as < ptree > ().end(),yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ().begin(), yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ().end()); }
#line 787 "parser.cc"
    break;

  case 13: // term: qvarid "(" args ")"
#line 106 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree(yystack_[3].value.as < std::string > (),yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 793 "parser.cc"
    break;

  case 14: // term: qvarid "(" "," args ")"
#line 107 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree(yystack_[4].value.as < std::string > ()); yylhs.value.as < ptree > ().push_back({}); yylhs.value.as < ptree > ().insert(yylhs.value.as < ptree > ().end(),yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ().begin(), yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ().end()); }
#line 799 "parser.cc"
    break;

  case 15: // term: "[" args "]"
#line 108 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 805 "parser.cc"
    break;

  case 16: // term: "[" "]"
#line 109 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",{}); }
#line 811 "parser.cc"
    break;

  case 17: // term: "(" tup_args "," exp ")"
#line 110 "parser.y"
                                  { yystack_[3].value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[1].value.as < ptree > ()}); yylhs.value.as < ptree > () = ptree("Tuple",yystack_[3].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 817 "parser.cc"
    break;

  case 18: // term: "~" term
#line 111 "parser.y"
                                  { yylhs.value.as < ptree > () = add_sample(yystack_[0].value.as < ptree > ()); }
#line 823 "parser.cc"
    break;

  case 19: // term: literal
#line 112 "parser.y"
                                  { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 829 "parser.cc"
    break;

  case 20: // term: "{" ditems "}"
#line 113 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 835 "parser.cc"
    break;

  case 21: // term: "{" "}"
#line 114 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",{}); }
#line 841 "parser.cc"
    break;

  case 22: // term: "function" "(" varid ":" exp ")"
#line 115 "parser.y"
                                              { yylhs.value.as < ptree > () = ptree("function",{{"",ptree(yystack_[3].value.as < std::string > ())},{"",yystack_[1].value.as < ptree > ()}}); }
#line 847 "parser.cc"
    break;

  case 23: // term: "function" "[" varid "," exp "]"
#line 116 "parser.y"
                                              { yylhs.value.as < ptree > () = ptree("function",{{"",ptree(yystack_[3].value.as < std::string > ())},{"",yystack_[1].value.as < ptree > ()}}); }
#line 853 "parser.cc"
    break;

  case 24: // ditems: ditem
#line 118 "parser.y"
                                  { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()}); }
#line 859 "parser.cc"
    break;

  case 25: // ditems: ditems "," ditem
#line 119 "parser.y"
                                  { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > (); yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()}); }
#line 865 "parser.cc"
    break;

  case 26: // ditem: exp ":" exp
#line 121 "parser.y"
                    { yylhs.value.as < ptree > () = ptree("Tuple",{{"",yystack_[2].value.as < ptree > ()},{"",yystack_[0].value.as < ptree > ()}}); }
#line 871 "parser.cc"
    break;

  case 27: // args: arg
#line 123 "parser.y"
                          { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back(yystack_[0].value.as < std::pair<std::string,ptree> > ()); }
#line 877 "parser.cc"
    break;

  case 28: // args: args "," arg
#line 124 "parser.y"
                          { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > (); yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back(yystack_[0].value.as < std::pair<std::string,ptree> > ()); }
#line 883 "parser.cc"
    break;

  case 29: // args: args ","
#line 125 "parser.y"
                          { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > (); yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({}); }
#line 889 "parser.cc"
    break;

  case 30: // arg: varid "=" exp
#line 127 "parser.y"
                   { yylhs.value.as < std::pair<std::string,ptree> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < ptree > ()}; }
#line 895 "parser.cc"
    break;

  case 31: // arg: varid "~" exp
#line 128 "parser.y"
                   { yylhs.value.as < std::pair<std::string,ptree> > () = {yystack_[2].value.as < std::string > (),add_sample(yystack_[0].value.as < ptree > ())}; }
#line 901 "parser.cc"
    break;

  case 32: // arg: exp
#line 129 "parser.y"
                   { yylhs.value.as < std::pair<std::string,ptree> > () = {"",yystack_[0].value.as < ptree > ()}; }
#line 907 "parser.cc"
    break;

  case 33: // tup_args: exp
#line 131 "parser.y"
                            { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()});}
#line 913 "parser.cc"
    break;

  case 34: // tup_args: tup_args "," exp
#line 132 "parser.y"
                            { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > (); yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()});}
#line 919 "parser.cc"
    break;

  case 35: // qvarid: varid
#line 137 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 925 "parser.cc"
    break;

  case 36: // qvarid: "QVARID"
#line 138 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 931 "parser.cc"
    break;

  case 37: // varid: "VARID"
#line 140 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 937 "parser.cc"
    break;

  case 38: // literal: "STRING"
#line 142 "parser.y"
                     {yylhs.value.as < ptree > () = ptree('"' + yystack_[0].value.as < std::string > () + '"');}
#line 943 "parser.cc"
    break;

  case 39: // literal: "INTEGER"
#line 143 "parser.y"
                     {yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < int > ());}
#line 949 "parser.cc"
    break;

  case 40: // literal: "FLOAT"
#line 144 "parser.y"
                     {yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < double > ());}
#line 955 "parser.cc"
    break;


#line 959 "parser.cc"

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


  const signed char parser::yypact_ninf_ = -32;

  const signed char parser::yytable_ninf_ = -1;

  const short
  parser::yypact_[] =
  {
     142,    10,   162,    62,   142,    82,   -15,   -32,   -32,   -32,
     -32,   -32,    12,   -32,    17,   -32,    21,    13,   -32,   -15,
     -15,   142,   -32,   -32,   -32,   -32,    -7,   -32,    28,    19,
      14,   -32,    33,    29,   -32,   -32,   -32,   162,   162,   102,
     122,   142,    30,    40,   -32,   -32,   142,   142,   142,   -32,
     142,   142,   -32,   142,   -32,   -32,   142,    -3,   142,     2,
      42,   142,   142,   -32,    42,   -32,    38,   -32,   -32,     1,
     -32,     3,   -32,   142,    15,    41,   -32,   -32,   -32,   -32,
     -32,   -32
  };

  const signed char
  parser::yydefact_[] =
  {
       0,     0,     0,     0,     0,     0,     0,    37,    36,    38,
      39,    40,     0,     2,     3,     6,     9,    35,    19,     0,
       0,     0,    18,    35,    16,    32,     0,    27,    35,    33,
       0,    21,     0,     0,    24,    10,     1,     0,     0,     0,
       0,     0,     0,     0,    33,    15,    29,     0,     0,     4,
       0,     0,    20,     0,     7,     8,     0,     0,     0,     0,
       0,     0,     0,    28,    30,    31,    34,    26,    25,     0,
      11,     0,    13,     0,     0,     0,    17,    12,    14,     5,
      23,    22
  };

  const signed char
  parser::yypgoto_[] =
  {
     -32,   -32,     0,   -32,    -1,   -32,    11,   -31,     8,   -32,
     -32,    20,   -32
  };

  const signed char
  parser::yydefgoto_[] =
  {
       0,    12,    25,    14,    15,    33,    34,    26,    27,    30,
      16,    17,    18
  };

  const signed char
  parser::yytable_[] =
  {
      13,    22,    45,     7,    29,    32,    70,    46,    57,    59,
      77,    46,    36,    72,    78,    46,    46,    46,    19,    41,
      20,    44,    23,    28,    80,    69,    35,    71,    50,    39,
      49,    40,    37,    38,    47,    48,    54,    55,    51,    42,
      43,    60,    52,    53,    61,    62,    73,    64,    65,    76,
      66,    67,    81,    32,    63,     0,     0,    23,    23,    28,
      28,    74,    75,     0,    68,     1,    28,     0,     0,     2,
       3,    24,     4,    79,     5,     0,    28,     0,    28,     6,
       7,     8,     9,    10,    11,     1,     0,     0,     0,     2,
       3,     0,     4,     0,     5,    31,     0,     0,     0,     6,
       7,     8,     9,    10,    11,     1,     0,     0,     0,     2,
       3,     0,     4,     0,     5,     0,    56,     0,     0,     6,
       7,     8,     9,    10,    11,     1,     0,     0,     0,     2,
       3,     0,     4,     0,     5,     0,    58,     0,     0,     6,
       7,     8,     9,    10,    11,     1,     0,     0,     0,     2,
       3,     0,     4,     0,     5,     0,     0,     0,     0,     6,
       7,     8,     9,    10,    11,     1,     0,     0,     0,     2,
       3,     0,    21,     0,     5,     0,     0,     0,     0,     6,
       7,     8,     9,    10,    11
  };

  const signed char
  parser::yycheck_[] =
  {
       0,     2,     9,    18,     4,     5,     9,    14,    39,    40,
       9,    14,     0,    11,    11,    14,    14,    14,     8,     6,
      10,    21,     2,     3,     9,    56,     6,    58,    14,     8,
      11,    10,    15,    16,     6,     7,    37,    38,     5,    19,
      20,    41,    13,    14,    14,     5,     4,    47,    48,    11,
      50,    51,    11,    53,    46,    -1,    -1,    37,    38,    39,
      40,    61,    62,    -1,    53,     3,    46,    -1,    -1,     7,
       8,     9,    10,    73,    12,    -1,    56,    -1,    58,    17,
      18,    19,    20,    21,    22,     3,    -1,    -1,    -1,     7,
       8,    -1,    10,    -1,    12,    13,    -1,    -1,    -1,    17,
      18,    19,    20,    21,    22,     3,    -1,    -1,    -1,     7,
       8,    -1,    10,    -1,    12,    -1,    14,    -1,    -1,    17,
      18,    19,    20,    21,    22,     3,    -1,    -1,    -1,     7,
       8,    -1,    10,    -1,    12,    -1,    14,    -1,    -1,    17,
      18,    19,    20,    21,    22,     3,    -1,    -1,    -1,     7,
       8,    -1,    10,    -1,    12,    -1,    -1,    -1,    -1,    17,
      18,    19,    20,    21,    22,     3,    -1,    -1,    -1,     7,
       8,    -1,    10,    -1,    12,    -1,    -1,    -1,    -1,    17,
      18,    19,    20,    21,    22
  };

  const signed char
  parser::yystos_[] =
  {
       0,     3,     7,     8,    10,    12,    17,    18,    19,    20,
      21,    22,    24,    25,    26,    27,    33,    34,    35,     8,
      10,    10,    27,    34,     9,    25,    30,    31,    34,    25,
      32,    13,    25,    28,    29,    34,     0,    15,    16,     8,
      10,     6,    34,    34,    25,     9,    14,     6,     7,    11,
      14,     5,    13,    14,    27,    27,    14,    30,    14,    30,
      25,    14,     5,    31,    25,    25,    25,    25,    29,    30,
       9,    30,    11,     4,    25,    25,    11,     9,    11,    25,
       9,    11
  };

  const signed char
  parser::yyr1_[] =
  {
       0,    23,    24,    25,    25,    25,    26,    26,    26,    27,
      27,    27,    27,    27,    27,    27,    27,    27,    27,    27,
      27,    27,    27,    27,    28,    28,    29,    30,    30,    30,
      31,    31,    31,    32,    32,    33,    33,    34,    35,    35,
      35
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     1,     1,     3,     5,     1,     3,     3,     1,
       2,     4,     5,     4,     5,     3,     2,     5,     2,     1,
       3,     2,     6,     6,     1,     3,     3,     1,     3,     2,
       3,     3,     1,     1,     3,     1,     1,     1,     1,     1,
       1
  };


#if ZZDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "\"function\"",
  "\";\"", "\":\"", "\"=\"", "\"~\"", "\"[\"", "\"]\"", "\"(\"", "\")\"",
  "\"{\"", "\"}\"", "\",\"", "\"+\"", "\"+>\"", "\"@\"", "\"VARID\"",
  "\"QVARID\"", "\"STRING\"", "\"INTEGER\"", "\"FLOAT\"", "$accept",
  "unit", "exp", "terms", "term", "ditems", "ditem", "args", "arg",
  "tup_args", "qvarid", "varid", "literal", YY_NULLPTR
  };
#endif


#if ZZDEBUG
  const unsigned char
  parser::yyrline_[] =
  {
       0,    90,    90,    93,    94,    95,    97,    98,    99,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   118,   119,   121,   123,   124,   125,
     127,   128,   129,   131,   132,   137,   138,   140,   142,   143,
     144
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
#line 1497 "parser.cc"

#line 152 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
zz::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}


ptree fold_terms(const std::vector<ptree>& terms)
{
    std::optional<ptree> result;
    for(auto& term: terms)
    {
        if (not result)
            result = term;
        else
            result = add_submodel(term, *result);
    }
    return *result;
}
