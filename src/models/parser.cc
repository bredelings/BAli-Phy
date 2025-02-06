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

ptree add_arg(const ptree& p1, const ptree& p2);

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

      case symbol_kind::S_defs: // defs
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.YY_MOVE_OR_COPY< ptree > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_def: // def
      case symbol_kind::S_arg: // arg
        value.YY_MOVE_OR_COPY< std::pair<std::string,ptree> > (YY_MOVE (that.value));
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

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.YY_MOVE_OR_COPY< std::vector<ptree> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.YY_MOVE_OR_COPY< std::vector<std::pair<std::string,ptree>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_varids: // varids
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_defs: // defs
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.move< ptree > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_def: // def
      case symbol_kind::S_arg: // arg
        value.move< std::pair<std::string,ptree> > (YY_MOVE (that.value));
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

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.move< std::vector<ptree> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<std::pair<std::string,ptree>> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_varids: // varids
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_defs: // defs
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.copy< ptree > (that.value);
        break;

      case symbol_kind::S_def: // def
      case symbol_kind::S_arg: // arg
        value.copy< std::pair<std::string,ptree> > (that.value);
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

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.copy< std::vector<ptree> > (that.value);
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.copy< std::vector<std::pair<std::string,ptree>> > (that.value);
        break;

      case symbol_kind::S_varids: // varids
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
      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (that.value);
        break;

      case symbol_kind::S_defs: // defs
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.move< ptree > (that.value);
        break;

      case symbol_kind::S_def: // def
      case symbol_kind::S_arg: // arg
        value.move< std::pair<std::string,ptree> > (that.value);
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

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.move< std::vector<ptree> > (that.value);
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<std::pair<std::string,ptree>> > (that.value);
        break;

      case symbol_kind::S_varids: // varids
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

      case symbol_kind::S_defs: // defs
      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        yylhs.value.emplace< ptree > ();
        break;

      case symbol_kind::S_def: // def
      case symbol_kind::S_arg: // arg
        yylhs.value.emplace< std::pair<std::string,ptree> > ();
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

      case symbol_kind::S_type_tup_args: // type_tup_args
        yylhs.value.emplace< std::vector<ptree> > ();
        break;

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_args: // args
      case symbol_kind::S_tup_args: // tup_args
        yylhs.value.emplace< std::vector<std::pair<std::string,ptree>> > ();
        break;

      case symbol_kind::S_varids: // varids
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
  case 2: // start: START_EXP exp
#line 127 "parser.y"
                     {drv.result = yystack_[0].value.as < ptree > ();}
#line 787 "parser.cc"
    break;

  case 3: // start: START_TYPE type
#line 128 "parser.y"
                       {drv.result = yystack_[0].value.as < ptree > ();}
#line 793 "parser.cc"
    break;

  case 4: // start: START_DEFS defs
#line 129 "parser.y"
                       {drv.result = yystack_[0].value.as < ptree > ();}
#line 799 "parser.cc"
    break;

  case 5: // def: varid "=" exp
#line 131 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,ptree> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < ptree > ()}; }
#line 805 "parser.cc"
    break;

  case 6: // def: fncall "=" exp
#line 132 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,ptree> > () = make_function_def(drv,yystack_[2].location,yystack_[2].value.as < ptree > (),yystack_[0].value.as < ptree > ()); }
#line 811 "parser.cc"
    break;

  case 7: // def: varid "~" exp
#line 133 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,ptree> > () = {yystack_[2].value.as < std::string > (),add_sample(yystack_[0].value.as < ptree > ())}; }
#line 817 "parser.cc"
    break;

  case 8: // defs: %empty
#line 135 "parser.y"
                   { yylhs.value.as < ptree > () = ptree("!Decls"); }
#line 823 "parser.cc"
    break;

  case 9: // defs: def
#line 136 "parser.y"
                   { yylhs.value.as < ptree > () = ptree("!Decls", {yystack_[0].value.as < std::pair<std::string,ptree> > ()}); }
#line 829 "parser.cc"
    break;

  case 10: // defs: defs ";" def
#line 137 "parser.y"
                   { yylhs.value.as < ptree > () = yystack_[2].value.as < ptree > (); yylhs.value.as < ptree > ().push_back(yystack_[0].value.as < std::pair<std::string,ptree> > ()); }
#line 835 "parser.cc"
    break;

  case 11: // defs: defs ";"
#line 138 "parser.y"
                   { yylhs.value.as < ptree > () = yystack_[1].value.as < ptree > (); }
#line 841 "parser.cc"
    break;

  case 12: // exp: term
#line 140 "parser.y"
                                  { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 847 "parser.cc"
    break;

  case 13: // exp: "{" defs ";" exp "}"
#line 141 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("!let",{{"decls",yystack_[3].value.as < ptree > ()},{"body",yystack_[1].value.as < ptree > ()}}); }
#line 853 "parser.cc"
    break;

  case 14: // exp: "{" defs ";" exp ";" "}"
#line 142 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("!let",{{"decls",yystack_[4].value.as < ptree > ()},{"body",yystack_[2].value.as < ptree > ()}}); }
#line 859 "parser.cc"
    break;

  case 15: // term: qvarid
#line 146 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < std::string > ()); }
#line 865 "parser.cc"
    break;

  case 16: // term: "@" varid
#line 147 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("@"+yystack_[0].value.as < std::string > ()); }
#line 871 "parser.cc"
    break;

  case 17: // term: fncall
#line 148 "parser.y"
                                  { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 877 "parser.cc"
    break;

  case 18: // term: "[" args "]"
#line 149 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 883 "parser.cc"
    break;

  case 19: // term: "[" "]"
#line 150 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",{}); }
#line 889 "parser.cc"
    break;

  case 20: // term: "(" tup_args "," exp ")"
#line 151 "parser.y"
                                  { yystack_[3].value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[1].value.as < ptree > ()}); yylhs.value.as < ptree > () = ptree("Tuple",yystack_[3].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 895 "parser.cc"
    break;

  case 21: // term: "~" term
#line 152 "parser.y"
                                  { yylhs.value.as < ptree > () = add_sample(yystack_[0].value.as < ptree > ()); }
#line 901 "parser.cc"
    break;

  case 22: // term: literal
#line 153 "parser.y"
                                  { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 907 "parser.cc"
    break;

  case 23: // term: "{" ditems "}"
#line 154 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 913 "parser.cc"
    break;

  case 24: // term: "{" "}"
#line 155 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("List",{}); }
#line 919 "parser.cc"
    break;

  case 25: // term: "|" varids ":" exp "|"
#line 156 "parser.y"
                                  { yylhs.value.as < ptree > () = make_function(yystack_[3].value.as < std::vector<std::string> > (), yystack_[1].value.as < ptree > ());}
#line 925 "parser.cc"
    break;

  case 26: // term: "(" exp ")"
#line 157 "parser.y"
                                  { yylhs.value.as < ptree > () = yystack_[1].value.as < ptree > (); }
#line 931 "parser.cc"
    break;

  case 27: // term: "-" term
#line 158 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("negate",{{"",ptree(yystack_[0].value.as < ptree > ())}}); }
#line 937 "parser.cc"
    break;

  case 28: // term: term "+" term
#line 159 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("+",{{"",ptree(yystack_[2].value.as < ptree > ())},{"",yystack_[0].value.as < ptree > ()}}); }
#line 943 "parser.cc"
    break;

  case 29: // term: term "-" term
#line 160 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("-",{{"",ptree(yystack_[2].value.as < ptree > ())},{"",yystack_[0].value.as < ptree > ()}}); }
#line 949 "parser.cc"
    break;

  case 30: // term: term "*" term
#line 161 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("*",{{"",ptree(yystack_[2].value.as < ptree > ())},{"",yystack_[0].value.as < ptree > ()}}); }
#line 955 "parser.cc"
    break;

  case 31: // term: term "/" term
#line 162 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("/",{{"",ptree(yystack_[2].value.as < ptree > ())},{"",yystack_[0].value.as < ptree > ()}}); }
#line 961 "parser.cc"
    break;

  case 32: // term: term "+>" fncall
#line 163 "parser.y"
                                  { yylhs.value.as < ptree > () = add_arg(yystack_[2].value.as < ptree > (),yystack_[0].value.as < ptree > ()); }
#line 967 "parser.cc"
    break;

  case 33: // term: term "+>" qvarid
#line 164 "parser.y"
                                  { yylhs.value.as < ptree > () = add_arg(yystack_[2].value.as < ptree > (),ptree(yystack_[0].value.as < std::string > ())); }
#line 973 "parser.cc"
    break;

  case 34: // term: "_"
#line 165 "parser.y"
                                  { yylhs.value.as < ptree > () = ptree("_"); }
#line 979 "parser.cc"
    break;

  case 35: // varids: varid
#line 168 "parser.y"
                        { yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 985 "parser.cc"
    break;

  case 36: // varids: varids varid
#line 169 "parser.y"
                        { yylhs.value.as < std::vector<std::string> > () = yystack_[1].value.as < std::vector<std::string> > (); yylhs.value.as < std::vector<std::string> > ().push_back(yystack_[0].value.as < std::string > ()); }
#line 991 "parser.cc"
    break;

  case 37: // fncall: qvarid "(" args ")"
#line 171 "parser.y"
                                    { yylhs.value.as < ptree > () = ptree(yystack_[3].value.as < std::string > (),yystack_[1].value.as < std::vector<std::pair<std::string,ptree>> > ()); }
#line 997 "parser.cc"
    break;

  case 38: // ditems: ditem
#line 173 "parser.y"
                                  { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()}); }
#line 1003 "parser.cc"
    break;

  case 39: // ditems: ditems "," ditem
#line 174 "parser.y"
                                  { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > (); yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()}); }
#line 1009 "parser.cc"
    break;

  case 40: // ditem: exp ":" exp
#line 176 "parser.y"
                    { yylhs.value.as < ptree > () = ptree("Tuple",{{"",yystack_[2].value.as < ptree > ()},{"",yystack_[0].value.as < ptree > ()}}); }
#line 1015 "parser.cc"
    break;

  case 41: // args: arg
#line 178 "parser.y"
                          { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back(yystack_[0].value.as < std::pair<std::string,ptree> > ()); }
#line 1021 "parser.cc"
    break;

  case 42: // args: args "," arg
#line 179 "parser.y"
                          { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > (); yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back(yystack_[0].value.as < std::pair<std::string,ptree> > ()); }
#line 1027 "parser.cc"
    break;

  case 43: // arg: varid "=" exp
#line 181 "parser.y"
                   { yylhs.value.as < std::pair<std::string,ptree> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < ptree > ()}; }
#line 1033 "parser.cc"
    break;

  case 44: // arg: varid "~" exp
#line 182 "parser.y"
                   { yylhs.value.as < std::pair<std::string,ptree> > () = {yystack_[2].value.as < std::string > (),add_sample(yystack_[0].value.as < ptree > ())}; }
#line 1039 "parser.cc"
    break;

  case 45: // arg: exp
#line 183 "parser.y"
                   { yylhs.value.as < std::pair<std::string,ptree> > () = {"",yystack_[0].value.as < ptree > ()}; }
#line 1045 "parser.cc"
    break;

  case 46: // tup_args: exp
#line 185 "parser.y"
                            { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()});}
#line 1051 "parser.cc"
    break;

  case 47: // tup_args: tup_args "," exp
#line 186 "parser.y"
                            { yylhs.value.as < std::vector<std::pair<std::string,ptree>> > () = yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > (); yystack_[2].value.as < std::vector<std::pair<std::string,ptree>> > ().push_back({"",yystack_[0].value.as < ptree > ()});}
#line 1057 "parser.cc"
    break;

  case 48: // qvarid: varid
#line 191 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1063 "parser.cc"
    break;

  case 49: // qvarid: "QVARID"
#line 192 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1069 "parser.cc"
    break;

  case 50: // qvarid: "(" "QVARSYM" ")"
#line 193 "parser.y"
                        { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 1075 "parser.cc"
    break;

  case 51: // varid: "VARID"
#line 195 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1081 "parser.cc"
    break;

  case 52: // varid: "(" "VARSYM" ")"
#line 196 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 1087 "parser.cc"
    break;

  case 53: // varid: "(" ":" ")"
#line 197 "parser.y"
                    { yylhs.value.as < std::string > () = ":"; }
#line 1093 "parser.cc"
    break;

  case 54: // varid: "(" "+" ")"
#line 198 "parser.y"
                    { yylhs.value.as < std::string > () = "+"; }
#line 1099 "parser.cc"
    break;

  case 55: // varid: "(" "-" ")"
#line 199 "parser.y"
                    { yylhs.value.as < std::string > () = "-"; }
#line 1105 "parser.cc"
    break;

  case 56: // varid: "(" "*" ")"
#line 200 "parser.y"
                    { yylhs.value.as < std::string > () = "*"; }
#line 1111 "parser.cc"
    break;

  case 57: // varid: "(" "/" ")"
#line 201 "parser.y"
                    { yylhs.value.as < std::string > () = "/"; }
#line 1117 "parser.cc"
    break;

  case 58: // literal: "STRING"
#line 203 "parser.y"
                     {yylhs.value.as < ptree > () = ptree('"' + yystack_[0].value.as < std::string > () + '"');}
#line 1123 "parser.cc"
    break;

  case 59: // literal: "INTEGER"
#line 204 "parser.y"
                     {yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < int > ());}
#line 1129 "parser.cc"
    break;

  case 60: // literal: "FLOAT"
#line 205 "parser.y"
                     {yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < double > ());}
#line 1135 "parser.cc"
    break;

  case 61: // type: btype
#line 209 "parser.y"
                                        { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 1141 "parser.cc"
    break;

  case 62: // type: btype "->" type
#line 210 "parser.y"
                                        { yylhs.value.as < ptree > () = make_type_app("Function",{yystack_[2].value.as < ptree > (),yystack_[0].value.as < ptree > ()});  }
#line 1147 "parser.cc"
    break;

  case 63: // btype: atype
#line 212 "parser.y"
                                        { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 1153 "parser.cc"
    break;

  case 64: // btype: atype "<" type_tup_args ">"
#line 213 "parser.y"
                                        { yylhs.value.as < ptree > () = make_type_app(yystack_[3].value.as < ptree > (), yystack_[1].value.as < std::vector<ptree> > ()); }
#line 1159 "parser.cc"
    break;

  case 65: // atype: varid
#line 215 "parser.y"
                                        { yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < std::string > ()); }
#line 1165 "parser.cc"
    break;

  case 66: // atype: "(" type ")"
#line 216 "parser.y"
                                        { yylhs.value.as < ptree > () = yystack_[1].value.as < ptree > (); }
#line 1171 "parser.cc"
    break;

  case 67: // atype: "(" type_tup_args "," type ")"
#line 217 "parser.y"
                                        { yystack_[3].value.as < std::vector<ptree> > ().push_back(yystack_[1].value.as < ptree > ()); yylhs.value.as < ptree > () = make_type_app(ptree("Tuple"),yystack_[3].value.as < std::vector<ptree> > ()); }
#line 1177 "parser.cc"
    break;

  case 68: // type_tup_args: type
#line 219 "parser.y"
                                        { yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 1183 "parser.cc"
    break;

  case 69: // type_tup_args: type_tup_args "," type
#line 220 "parser.y"
                                        { yylhs.value.as < std::vector<ptree> > () = yystack_[2].value.as < std::vector<ptree> > (); yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 1189 "parser.cc"
    break;


#line 1193 "parser.cc"

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


  const signed char parser::yypact_ninf_ = -62;

  const signed char parser::yytable_ninf_ = -1;

  const short
  parser::yypact_[] =
  {
      93,   182,    24,    27,    14,    52,    26,   104,   130,    52,
     208,   208,   -62,   -62,   -62,   -62,   -62,   -62,   -62,    82,
     -62,     6,   -62,   -62,     4,   -62,   -62,   -10,    35,   222,
     -62,    71,    81,     6,    31,   -62,   226,    15,   -62,   -62,
     -62,    -4,   -62,    59,   100,   101,   156,   102,   107,   114,
     115,   126,   124,   -62,   113,   136,    81,    55,   -62,   -62,
     130,   -62,    62,   208,   208,   208,   208,    27,   182,   133,
     138,   127,    24,    24,    27,   182,   182,   182,   182,   -62,
     -62,   182,   182,   182,   -62,   -62,   -62,   -62,   -62,   -62,
     -62,   -62,   182,   182,   182,   -62,   182,    62,    62,   -62,
     -62,   -62,     6,    69,   -62,    24,   -62,   -62,    70,   -62,
     -62,   -62,   -62,   142,   -62,   -62,   -62,   139,    13,   -62,
     -62,   -62,   140,   -62,    24,   -62,   -62,   144,   -62,   -62,
     -62,   -62
  };

  const signed char
  parser::yydefact_[] =
  {
       0,     0,     0,     8,     0,     0,     0,     0,     8,     0,
       0,     0,    34,    51,    49,    58,    59,    60,     2,    12,
      17,    15,    48,    22,     0,    65,     3,    61,    63,     0,
       9,     4,     0,     0,    48,     1,     0,     0,    35,    19,
      45,     0,    41,    48,     0,     0,     0,     0,     0,     0,
       0,    46,     0,    24,     0,     0,    17,     0,    38,    16,
       0,    21,    27,     0,     0,     0,     0,     0,     0,     0,
      68,     0,     0,     0,    11,     0,     0,     0,     0,    36,
      18,     0,     0,     0,    53,    54,    55,    56,    57,    52,
      50,    26,     0,    11,     0,    23,     0,    28,    29,    30,
      31,    32,    33,     0,    66,     0,    62,    68,     0,    10,
       6,     5,     7,     0,    42,    43,    44,    47,     0,    40,
      39,    37,    69,    64,     0,    25,    20,     0,    13,    67,
      69,    14
  };

  const short
  parser::yypgoto_[] =
  {
     -62,   -62,   -61,   149,    17,    -1,   -62,     8,   -62,    64,
     106,    87,   -62,     2,    -2,   -62,   -22,   -62,   -62,    96
  };

  const signed char
  parser::yydefgoto_[] =
  {
       0,     4,    30,    31,    40,    19,    37,    20,    57,    58,
      41,    42,    52,    21,    22,    23,    26,    27,    28,    71
  };

  const unsigned char
  parser::yytable_[] =
  {
      25,    34,    70,    38,    43,    33,    34,    59,    80,    61,
      62,    32,    44,   109,    35,    81,    56,    72,    18,    24,
     127,    68,    25,    78,    51,    55,    45,    69,    47,    48,
      36,   128,   109,    13,    49,    79,     5,     6,    39,    24,
      76,     7,    29,     8,    13,    62,     9,    10,    73,    11,
     106,   107,    77,    13,    12,    13,    13,    14,    14,    15,
      16,    17,    97,    98,    99,   100,    43,    36,    82,   102,
      25,    25,    34,    95,    96,   101,    33,    55,    74,    43,
      83,    13,    32,   122,   123,   121,    65,    66,    81,   124,
      75,    34,   110,   111,   112,   113,     1,     2,     3,   115,
     116,    56,   130,    25,    63,    64,    65,    66,    67,   117,
     118,   119,    44,    55,     5,     6,    84,    85,    87,     7,
      93,     8,    25,    88,     9,    10,    45,    46,    47,    48,
      89,    90,    12,    13,    49,    14,    50,    15,    16,    17,
       5,     6,    91,    92,    94,     7,   105,     8,    53,    86,
       9,    10,   125,    11,   104,   126,   129,    54,    12,    13,
     120,    14,   131,    15,    16,    17,     5,     6,   114,   108,
       0,     7,    86,    60,   103,     0,     9,    10,     0,    11,
       0,     0,     0,     0,    12,    13,     0,    14,     0,    15,
      16,    17,     5,     6,     0,     0,     0,     7,     0,     8,
       0,     0,     9,    10,     0,    11,     0,     0,     0,     0,
      12,    13,     0,    14,     0,    15,    16,    17,     5,     6,
       0,     0,     0,     7,     0,    60,     0,     0,     9,    10,
      44,    11,     0,     0,    44,     0,    12,    13,     0,    14,
       0,    15,    16,    17,    45,    69,    47,    48,    45,    69,
      47,    48,    49,     0,    50,     0,    49
  };

  const signed char
  parser::yycheck_[] =
  {
       2,     3,    24,     5,     6,     3,     8,     9,    12,    10,
      11,     3,     8,    74,     0,    19,     8,    27,     1,    15,
       7,    15,    24,     8,     7,     8,    22,    23,    24,    25,
      15,    18,    93,    29,    30,    37,    10,    11,    12,    15,
       9,    15,    15,    17,    29,    46,    20,    21,    13,    23,
      72,    73,    21,    29,    28,    29,    29,    31,    31,    33,
      34,    35,    63,    64,    65,    66,    68,    15,     9,    67,
      72,    73,    74,    18,    19,    67,    74,    60,     7,    81,
      21,    29,    74,   105,    14,    16,    24,    25,    19,    19,
       9,    93,    75,    76,    77,    78,     3,     4,     5,    82,
      83,    93,   124,   105,    22,    23,    24,    25,    26,    92,
      93,    94,     8,    96,    10,    11,    16,    16,    16,    15,
       7,    17,   124,    16,    20,    21,    22,    23,    24,    25,
      16,    16,    28,    29,    30,    31,    32,    33,    34,    35,
      10,    11,    16,    19,     8,    15,    19,    17,    18,    16,
      20,    21,    10,    23,    16,    16,    16,     8,    28,    29,
      96,    31,    18,    33,    34,    35,    10,    11,    81,    73,
      -1,    15,    16,    17,    68,    -1,    20,    21,    -1,    23,
      -1,    -1,    -1,    -1,    28,    29,    -1,    31,    -1,    33,
      34,    35,    10,    11,    -1,    -1,    -1,    15,    -1,    17,
      -1,    -1,    20,    21,    -1,    23,    -1,    -1,    -1,    -1,
      28,    29,    -1,    31,    -1,    33,    34,    35,    10,    11,
      -1,    -1,    -1,    15,    -1,    17,    -1,    -1,    20,    21,
       8,    23,    -1,    -1,     8,    -1,    28,    29,    -1,    31,
      -1,    33,    34,    35,    22,    23,    24,    25,    22,    23,
      24,    25,    30,    -1,    32,    -1,    30
  };

  const signed char
  parser::yystos_[] =
  {
       0,     3,     4,     5,    37,    10,    11,    15,    17,    20,
      21,    23,    28,    29,    31,    33,    34,    35,    40,    41,
      43,    49,    50,    51,    15,    50,    52,    53,    54,    15,
      38,    39,    43,    49,    50,     0,    15,    42,    50,    12,
      40,    46,    47,    50,     8,    22,    23,    24,    25,    30,
      32,    40,    48,    18,    39,    40,    43,    44,    45,    50,
      17,    41,    41,    22,    23,    24,    25,    26,    15,    23,
      52,    55,    27,    13,     7,     9,     9,    21,     8,    50,
      12,    19,     9,    21,    16,    16,    16,    16,    16,    16,
      16,    16,    19,     7,     8,    18,    19,    41,    41,    41,
      41,    43,    49,    46,    16,    19,    52,    52,    55,    38,
      40,    40,    40,    40,    47,    40,    40,    40,    40,    40,
      45,    16,    52,    14,    19,    10,    16,     7,    18,    16,
      52,    18
  };

  const signed char
  parser::yyr1_[] =
  {
       0,    36,    37,    37,    37,    38,    38,    38,    39,    39,
      39,    39,    40,    40,    40,    41,    41,    41,    41,    41,
      41,    41,    41,    41,    41,    41,    41,    41,    41,    41,
      41,    41,    41,    41,    41,    42,    42,    43,    44,    44,
      45,    46,    46,    47,    47,    47,    48,    48,    49,    49,
      49,    50,    50,    50,    50,    50,    50,    50,    51,    51,
      51,    52,    52,    53,    53,    54,    54,    54,    55,    55
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     2,     2,     2,     3,     3,     3,     0,     1,
       3,     2,     1,     5,     6,     1,     2,     1,     3,     2,
       5,     2,     1,     3,     2,     5,     3,     2,     3,     3,
       3,     3,     3,     3,     1,     1,     2,     4,     1,     3,
       3,     1,     3,     3,     3,     1,     1,     3,     1,     1,
       3,     1,     3,     3,     3,     3,     3,     3,     1,     1,
       1,     1,     3,     1,     4,     1,     3,     5,     1,     3
  };


#if ZZDEBUG || 1
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "\"end of file\"", "error", "\"invalid token\"", "START_EXP",
  "START_TYPE", "START_DEFS", "\"function\"", "\";\"", "\":\"", "\"=\"",
  "\"|\"", "\"[\"", "\"]\"", "\"<\"", "\">\"", "\"(\"", "\")\"", "\"{\"",
  "\"}\"", "\",\"", "\"@\"", "\"~\"", "\"+\"", "\"-\"", "\"*\"", "\"/\"",
  "\"+>\"", "\"->\"", "\"_\"", "\"VARID\"", "\"VARSYM\"", "\"QVARID\"",
  "\"QVARSYM\"", "\"STRING\"", "\"INTEGER\"", "\"FLOAT\"", "$accept",
  "start", "def", "defs", "exp", "term", "varids", "fncall", "ditems",
  "ditem", "args", "arg", "tup_args", "qvarid", "varid", "literal", "type",
  "btype", "atype", "type_tup_args", YY_NULLPTR
  };
#endif


#if ZZDEBUG
  const unsigned char
  parser::yyrline_[] =
  {
       0,   127,   127,   128,   129,   131,   132,   133,   135,   136,
     137,   138,   140,   141,   142,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   168,   169,   171,   173,   174,
     176,   178,   179,   181,   182,   183,   185,   186,   191,   192,
     193,   195,   196,   197,   198,   199,   200,   201,   203,   204,
     205,   209,   210,   212,   213,   215,   216,   217,   219,   220
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
#line 1769 "parser.cc"

#line 228 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
zz::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}

ptree make_function(const std::vector<std::string>& vars, const ptree& body)
{
    ptree f = body;
    for(auto& var: vars | views::reverse)
	f = ptree("function",{{"",ptree(var)},{"",f}});
    return f;
}

ptree make_type_app(ptree type, const std::vector<ptree>& args)
{
    for(auto& arg: args)
	type = ptree("@APP",{{"",type},{"",arg}});
    return type;
}

std::pair<std::string,ptree> make_function_def(zz_driver& drv, const yy::location& l, const ptree& fncall, const ptree& body)
{
    assert(fncall.has_value<string>());

    // 1. Get the function name
    auto fname = fncall.get_value<string>();
    if (fname.find('.') != string::npos)
	drv.push_error_message(l, "Function name cannot contain '.'");

    // 2. Get the argument names
    vector<string> vars;
    for(auto& [name,value]: fncall)
    {
	if (not name.empty())
	    drv.push_error_message(l, "Named arguments not allowed in function definitions");

	if (not value.is_a<string>())
	    drv.push_error_message(l, "Arguments in function definition must be variables");
	else
	    vars.push_back(value.get_value<string>());
    }
    
    return {fname, make_function(vars, body)};
}
