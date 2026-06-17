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
#line 42 "parser.y"

# include "driver.hh"
# include "parse.H"

CM::UntypedExpr add_arg(CM::UntypedExpr p1, CM::UntypedExpr p2);
CM::UntypedExpr make_binary_call(const std::string& name, const CM::UntypedExpr& lhs, const CM::UntypedExpr& rhs);
CM::UntypedExpr make_call(const std::string& name, const std::vector<CM::Arg<CM::NoAnn>>& args);
CM::UntypedExpr make_list(const std::vector<CM::Arg<CM::NoAnn>>& args);
CM::UntypedExpr make_list(const std::vector<CM::UntypedExpr>& elements);
CM::UntypedExpr make_sample(const CM::UntypedExpr& dist);
CM::UntypedExpr make_model_tuple(const std::vector<CM::UntypedExpr>& elements);

#line 61 "parser.cc"


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
#line 154 "parser.cc"

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

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.YY_MOVE_OR_COPY< CM::UntypedExpr > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_pattern: // pattern
        value.YY_MOVE_OR_COPY< CM::UntypedPattern > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.YY_MOVE_OR_COPY< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.YY_MOVE_OR_COPY< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.YY_MOVE_OR_COPY< ptree > (YY_MOVE (that.value));
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

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.YY_MOVE_OR_COPY< std::vector<CM::UntypedExpr> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.YY_MOVE_OR_COPY< std::vector<CM::UntypedPattern> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.YY_MOVE_OR_COPY< std::vector<ptree> > (YY_MOVE (that.value));
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

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.move< CM::UntypedExpr > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_pattern: // pattern
        value.move< CM::UntypedPattern > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.move< ptree > (YY_MOVE (that.value));
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

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<CM::UntypedExpr> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.move< std::vector<CM::UntypedPattern> > (YY_MOVE (that.value));
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.move< std::vector<ptree> > (YY_MOVE (that.value));
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

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.copy< CM::UntypedExpr > (that.value);
        break;

      case symbol_kind::S_pattern: // pattern
        value.copy< CM::UntypedPattern > (that.value);
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.copy< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.copy< int > (that.value);
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.copy< ptree > (that.value);
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

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.copy< std::vector<CM::UntypedExpr> > (that.value);
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.copy< std::vector<CM::UntypedPattern> > (that.value);
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.copy< std::vector<ptree> > (that.value);
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

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        value.move< CM::UntypedExpr > (that.value);
        break;

      case symbol_kind::S_pattern: // pattern
        value.move< CM::UntypedPattern > (that.value);
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        value.move< double > (that.value);
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        value.move< int > (that.value);
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        value.move< ptree > (that.value);
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

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        value.move< std::vector<CM::UntypedExpr> > (that.value);
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        value.move< std::vector<CM::UntypedPattern> > (that.value);
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        value.move< std::vector<ptree> > (that.value);
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

      case symbol_kind::S_exp: // exp
      case symbol_kind::S_term: // term
      case symbol_kind::S_fncall: // fncall
      case symbol_kind::S_ditem: // ditem
      case symbol_kind::S_literal: // literal
        yylhs.value.emplace< CM::UntypedExpr > ();
        break;

      case symbol_kind::S_pattern: // pattern
        yylhs.value.emplace< CM::UntypedPattern > ();
        break;

      case symbol_kind::S_FLOAT: // "FLOAT"
        yylhs.value.emplace< double > ();
        break;

      case symbol_kind::S_INTEGER: // "INTEGER"
        yylhs.value.emplace< int > ();
        break;

      case symbol_kind::S_type: // type
      case symbol_kind::S_btype: // btype
      case symbol_kind::S_atype: // atype
        yylhs.value.emplace< ptree > ();
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

      case symbol_kind::S_ditems: // ditems
      case symbol_kind::S_tup_args: // tup_args
        yylhs.value.emplace< std::vector<CM::UntypedExpr> > ();
        break;

      case symbol_kind::S_patterns: // patterns
      case symbol_kind::S_pattern_tup_args: // pattern_tup_args
        yylhs.value.emplace< std::vector<CM::UntypedPattern> > ();
        break;

      case symbol_kind::S_type_tup_args: // type_tup_args
        yylhs.value.emplace< std::vector<ptree> > ();
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
#line 147 "parser.y"
                     {drv.expression_result = yystack_[0].value.as < CM::UntypedExpr > ();}
#line 878 "parser.cc"
    break;

  case 3: // start: START_TYPE type
#line 148 "parser.y"
                       {drv.type_result = yystack_[0].value.as < ptree > ();}
#line 884 "parser.cc"
    break;

  case 4: // start: START_DEFS defs
#line 149 "parser.y"
                       {drv.defs_result = yystack_[0].value.as < CM::Decls<CM::NoAnn> > ();}
#line 890 "parser.cc"
    break;

  case 5: // def: varid "=" exp
#line 151 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,CM::UntypedExpr> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < CM::UntypedExpr > ()}; }
#line 896 "parser.cc"
    break;

  case 6: // def: fncall "=" exp
#line 152 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,CM::UntypedExpr> > () = make_function_def(drv,yystack_[2].location,yystack_[2].value.as < CM::UntypedExpr > (),yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 902 "parser.cc"
    break;

  case 7: // def: varid "~" exp
#line 153 "parser.y"
                                         { yylhs.value.as < std::pair<std::string,CM::UntypedExpr> > () = {yystack_[2].value.as < std::string > (),make_sample(yystack_[0].value.as < CM::UntypedExpr > ())}; }
#line 908 "parser.cc"
    break;

  case 8: // defs: %empty
#line 155 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = {}; }
#line 914 "parser.cc"
    break;

  case 9: // defs: def
#line 156 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = {yystack_[0].value.as < std::pair<std::string,CM::UntypedExpr> > ()}; }
#line 920 "parser.cc"
    break;

  case 10: // defs: defs ";" def
#line 157 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = yystack_[2].value.as < CM::Decls<CM::NoAnn> > (); yylhs.value.as < CM::Decls<CM::NoAnn> > ().push_back(yystack_[0].value.as < std::pair<std::string,CM::UntypedExpr> > ()); }
#line 926 "parser.cc"
    break;

  case 11: // defs: defs ";"
#line 158 "parser.y"
                   { yylhs.value.as < CM::Decls<CM::NoAnn> > () = yystack_[1].value.as < CM::Decls<CM::NoAnn> > (); }
#line 932 "parser.cc"
    break;

  case 12: // exp: term
#line 160 "parser.y"
                                   { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 938 "parser.cc"
    break;

  case 13: // exp: exp "where" "{" defs "}"
#line 161 "parser.y"
                                   { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Let<CM::NoAnn>{yystack_[1].value.as < CM::Decls<CM::NoAnn> > (), yystack_[4].value.as < CM::UntypedExpr > ()}}; }
#line 944 "parser.cc"
    break;

  case 14: // term: qvarid
#line 165 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Var{yystack_[0].value.as < std::string > ()}}; }
#line 950 "parser.cc"
    break;

  case 15: // term: "@" varid
#line 166 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::ArgRef{yystack_[0].value.as < std::string > ()}}; }
#line 956 "parser.cc"
    break;

  case 16: // term: fncall
#line 167 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 962 "parser.cc"
    break;

  case 17: // term: "[" args "]"
#line 168 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_list(yystack_[1].value.as < std::vector<CM::Arg<CM::NoAnn>> > ()); }
#line 968 "parser.cc"
    break;

  case 18: // term: "[" "]"
#line 169 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::List<CM::NoAnn>{}}; }
#line 974 "parser.cc"
    break;

  case 19: // term: "(" tup_args "," exp ")"
#line 170 "parser.y"
                                  { yystack_[3].value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[1].value.as < CM::UntypedExpr > ()); yylhs.value.as < CM::UntypedExpr > () = make_model_tuple(yystack_[3].value.as < std::vector<CM::UntypedExpr> > ()); }
#line 980 "parser.cc"
    break;

  case 20: // term: "~" term
#line 171 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_sample(yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 986 "parser.cc"
    break;

  case 21: // term: literal
#line 172 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = yystack_[0].value.as < CM::UntypedExpr > (); }
#line 992 "parser.cc"
    break;

  case 22: // term: "{" ditems "}"
#line 173 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_list(yystack_[1].value.as < std::vector<CM::UntypedExpr> > ()); }
#line 998 "parser.cc"
    break;

  case 23: // term: "{" "}"
#line 174 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::List<CM::NoAnn>{}}; }
#line 1004 "parser.cc"
    break;

  case 24: // term: "|" patterns ":" exp "|"
#line 175 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_function(yystack_[3].value.as < std::vector<CM::UntypedPattern> > (), yystack_[1].value.as < CM::UntypedExpr > ());}
#line 1010 "parser.cc"
    break;

  case 25: // term: "(" exp ")"
#line 176 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = yystack_[1].value.as < CM::UntypedExpr > (); }
#line 1016 "parser.cc"
    break;

  case 26: // term: "-" term
#line 177 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_call("negate", {{ "", yystack_[0].value.as < CM::UntypedExpr > (), false, false, std::nullopt }}); }
#line 1022 "parser.cc"
    break;

  case 27: // term: term "+" term
#line 178 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("+", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1028 "parser.cc"
    break;

  case 28: // term: term "-" term
#line 179 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("-", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1034 "parser.cc"
    break;

  case 29: // term: term "*" term
#line 180 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("*", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1040 "parser.cc"
    break;

  case 30: // term: term "/" term
#line 181 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("/", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1046 "parser.cc"
    break;

  case 31: // term: term "%" term
#line 182 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("%", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1052 "parser.cc"
    break;

  case 32: // term: term "==" term
#line 183 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("==", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1058 "parser.cc"
    break;

  case 33: // term: term "!=" term
#line 184 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("!=", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1064 "parser.cc"
    break;

  case 34: // term: term "<" term
#line 185 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("<", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1070 "parser.cc"
    break;

  case 35: // term: term ">" term
#line 186 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call(">", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1076 "parser.cc"
    break;

  case 36: // term: term "<=" term
#line 187 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("<=", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1082 "parser.cc"
    break;

  case 37: // term: term ">=" term
#line 188 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call(">=", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1088 "parser.cc"
    break;

  case 38: // term: term "&&" term
#line 189 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("&&", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1094 "parser.cc"
    break;

  case 39: // term: term "||" term
#line 190 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = make_binary_call("||", yystack_[2].value.as < CM::UntypedExpr > (), yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1100 "parser.cc"
    break;

  case 40: // term: term "+>" fncall
#line 191 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = add_arg(yystack_[2].value.as < CM::UntypedExpr > (),yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1106 "parser.cc"
    break;

  case 41: // term: term "+>" qvarid
#line 192 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = add_arg(yystack_[2].value.as < CM::UntypedExpr > (),CM::UntypedExpr{CM::NoAnn{}, CM::Var{yystack_[0].value.as < std::string > ()}}); }
#line 1112 "parser.cc"
    break;

  case 42: // term: "_"
#line 193 "parser.y"
                                  { yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::Placeholder{}}; }
#line 1118 "parser.cc"
    break;

  case 43: // patterns: pattern
#line 196 "parser.y"
                            { yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ()); }
#line 1124 "parser.cc"
    break;

  case 44: // patterns: patterns pattern
#line 197 "parser.y"
                             { yylhs.value.as < std::vector<CM::UntypedPattern> > () = yystack_[1].value.as < std::vector<CM::UntypedPattern> > (); yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ()); }
#line 1130 "parser.cc"
    break;

  case 45: // pattern: varid
#line 199 "parser.y"
                                                    { yylhs.value.as < CM::UntypedPattern > () = CM::UntypedPattern{CM::NoAnn{}, CM::VarPattern{yystack_[0].value.as < std::string > ()}}; }
#line 1136 "parser.cc"
    break;

  case 46: // pattern: "(" pattern_tup_args "," pattern ")"
#line 200 "parser.y"
                                                    { yystack_[3].value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[1].value.as < CM::UntypedPattern > ()); yylhs.value.as < CM::UntypedPattern > () = CM::UntypedPattern{CM::NoAnn{}, CM::TuplePattern<CM::NoAnn>{yystack_[3].value.as < std::vector<CM::UntypedPattern> > ()}}; }
#line 1142 "parser.cc"
    break;

  case 47: // pattern_tup_args: pattern
#line 202 "parser.y"
                                                    { yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ());}
#line 1148 "parser.cc"
    break;

  case 48: // pattern_tup_args: pattern_tup_args "," pattern
#line 203 "parser.y"
                                                    { yylhs.value.as < std::vector<CM::UntypedPattern> > () = yystack_[2].value.as < std::vector<CM::UntypedPattern> > (); yylhs.value.as < std::vector<CM::UntypedPattern> > ().push_back(yystack_[0].value.as < CM::UntypedPattern > ());}
#line 1154 "parser.cc"
    break;

  case 49: // fncall: qvarid "(" args ")"
#line 205 "parser.y"
                                    { yylhs.value.as < CM::UntypedExpr > () = make_call(yystack_[3].value.as < std::string > (),yystack_[1].value.as < std::vector<CM::Arg<CM::NoAnn>> > ()); }
#line 1160 "parser.cc"
    break;

  case 50: // ditems: ditem
#line 207 "parser.y"
                                  { yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1166 "parser.cc"
    break;

  case 51: // ditems: ditems "," ditem
#line 208 "parser.y"
                                  { yylhs.value.as < std::vector<CM::UntypedExpr> > () = yystack_[2].value.as < std::vector<CM::UntypedExpr> > (); yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ()); }
#line 1172 "parser.cc"
    break;

  case 52: // ditem: exp ":" exp
#line 210 "parser.y"
                    { yylhs.value.as < CM::UntypedExpr > () = make_model_tuple({yystack_[2].value.as < CM::UntypedExpr > (),yystack_[0].value.as < CM::UntypedExpr > ()}); }
#line 1178 "parser.cc"
    break;

  case 53: // args: arg
#line 212 "parser.y"
                          { yylhs.value.as < std::vector<CM::Arg<CM::NoAnn>> > ().push_back(yystack_[0].value.as < CM::Arg<CM::NoAnn> > ()); }
#line 1184 "parser.cc"
    break;

  case 54: // args: args "," arg
#line 213 "parser.y"
                          { yylhs.value.as < std::vector<CM::Arg<CM::NoAnn>> > () = yystack_[2].value.as < std::vector<CM::Arg<CM::NoAnn>> > (); yylhs.value.as < std::vector<CM::Arg<CM::NoAnn>> > ().push_back(yystack_[0].value.as < CM::Arg<CM::NoAnn> > ()); }
#line 1190 "parser.cc"
    break;

  case 55: // arg: varid "=" exp
#line 215 "parser.y"
                   { yylhs.value.as < CM::Arg<CM::NoAnn> > () = {yystack_[2].value.as < std::string > (),yystack_[0].value.as < CM::UntypedExpr > (),false,false,std::nullopt}; }
#line 1196 "parser.cc"
    break;

  case 56: // arg: varid "~" exp
#line 216 "parser.y"
                   { yylhs.value.as < CM::Arg<CM::NoAnn> > () = {yystack_[2].value.as < std::string > (),make_sample(yystack_[0].value.as < CM::UntypedExpr > ()),false,false,std::nullopt}; }
#line 1202 "parser.cc"
    break;

  case 57: // arg: exp
#line 217 "parser.y"
                   { yylhs.value.as < CM::Arg<CM::NoAnn> > () = {"",yystack_[0].value.as < CM::UntypedExpr > (),false,false,std::nullopt}; }
#line 1208 "parser.cc"
    break;

  case 58: // tup_args: exp
#line 219 "parser.y"
                            { yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ());}
#line 1214 "parser.cc"
    break;

  case 59: // tup_args: tup_args "," exp
#line 220 "parser.y"
                            { yylhs.value.as < std::vector<CM::UntypedExpr> > () = yystack_[2].value.as < std::vector<CM::UntypedExpr> > (); yylhs.value.as < std::vector<CM::UntypedExpr> > ().push_back(yystack_[0].value.as < CM::UntypedExpr > ());}
#line 1220 "parser.cc"
    break;

  case 60: // qvarid: varid
#line 225 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1226 "parser.cc"
    break;

  case 61: // qvarid: "QVARID"
#line 226 "parser.y"
               { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1232 "parser.cc"
    break;

  case 62: // qvarid: "(" "QVARSYM" ")"
#line 227 "parser.y"
                        { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 1238 "parser.cc"
    break;

  case 63: // varid: "VARID"
#line 229 "parser.y"
                    { yylhs.value.as < std::string > () = yystack_[0].value.as < std::string > (); }
#line 1244 "parser.cc"
    break;

  case 64: // varid: "(" "VARSYM" ")"
#line 230 "parser.y"
                       { yylhs.value.as < std::string > () = yystack_[1].value.as < std::string > (); }
#line 1250 "parser.cc"
    break;

  case 65: // varid: "(" ":" ")"
#line 231 "parser.y"
                    { yylhs.value.as < std::string > () = ":"; }
#line 1256 "parser.cc"
    break;

  case 66: // varid: "(" "+" ")"
#line 232 "parser.y"
                    { yylhs.value.as < std::string > () = "+"; }
#line 1262 "parser.cc"
    break;

  case 67: // varid: "(" "-" ")"
#line 233 "parser.y"
                    { yylhs.value.as < std::string > () = "-"; }
#line 1268 "parser.cc"
    break;

  case 68: // varid: "(" "*" ")"
#line 234 "parser.y"
                    { yylhs.value.as < std::string > () = "*"; }
#line 1274 "parser.cc"
    break;

  case 69: // varid: "(" "/" ")"
#line 235 "parser.y"
                    { yylhs.value.as < std::string > () = "/"; }
#line 1280 "parser.cc"
    break;

  case 70: // literal: "STRING"
#line 237 "parser.y"
                     {yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::StringLiteral{yystack_[0].value.as < std::string > ()}};}
#line 1286 "parser.cc"
    break;

  case 71: // literal: "INTEGER"
#line 238 "parser.y"
                     {yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::IntLiteral{yystack_[0].value.as < int > ()}};}
#line 1292 "parser.cc"
    break;

  case 72: // literal: "FLOAT"
#line 239 "parser.y"
                     {yylhs.value.as < CM::UntypedExpr > () = CM::UntypedExpr{CM::NoAnn{}, CM::DoubleLiteral{yystack_[0].value.as < double > ()}};}
#line 1298 "parser.cc"
    break;

  case 73: // type: btype
#line 243 "parser.y"
                                        { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 1304 "parser.cc"
    break;

  case 74: // type: btype "->" type
#line 244 "parser.y"
                                        { yylhs.value.as < ptree > () = make_type_app("Function",{yystack_[2].value.as < ptree > (),yystack_[0].value.as < ptree > ()});  }
#line 1310 "parser.cc"
    break;

  case 75: // btype: atype
#line 246 "parser.y"
                                        { yylhs.value.as < ptree > () = yystack_[0].value.as < ptree > (); }
#line 1316 "parser.cc"
    break;

  case 76: // btype: atype "<" type_tup_args ">"
#line 247 "parser.y"
                                        { yylhs.value.as < ptree > () = make_type_app(yystack_[3].value.as < ptree > (), yystack_[1].value.as < std::vector<ptree> > ()); }
#line 1322 "parser.cc"
    break;

  case 77: // atype: varid
#line 249 "parser.y"
                                        { yylhs.value.as < ptree > () = ptree(yystack_[0].value.as < std::string > ()); }
#line 1328 "parser.cc"
    break;

  case 78: // atype: "(" type ")"
#line 250 "parser.y"
                                        { yylhs.value.as < ptree > () = yystack_[1].value.as < ptree > (); }
#line 1334 "parser.cc"
    break;

  case 79: // atype: "(" type_tup_args "," type ")"
#line 251 "parser.y"
                                        { yystack_[3].value.as < std::vector<ptree> > ().push_back(yystack_[1].value.as < ptree > ()); yylhs.value.as < ptree > () = make_type_app(ptree("Tuple"),yystack_[3].value.as < std::vector<ptree> > ()); }
#line 1340 "parser.cc"
    break;

  case 80: // type_tup_args: type
#line 253 "parser.y"
                                        { yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 1346 "parser.cc"
    break;

  case 81: // type_tup_args: type_tup_args "," type
#line 254 "parser.y"
                                        { yylhs.value.as < std::vector<ptree> > () = yystack_[2].value.as < std::vector<ptree> > (); yylhs.value.as < std::vector<ptree> > ().push_back(yystack_[0].value.as < ptree > ());}
#line 1352 "parser.cc"
    break;


#line 1356 "parser.cc"

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


  const signed char parser::yypact_ninf_ = -28;

  const signed char parser::yytable_ninf_ = -1;

  const short
  parser::yypact_[] =
  {
     140,   211,    38,    19,     6,    48,   154,     8,   166,    82,
     211,   211,   -28,   -28,   -28,   -28,   -28,   -28,    27,   234,
     -28,    49,   -28,   -28,    50,   -28,   -28,   -21,    47,    17,
     -28,    76,    85,    49,    33,   -28,   104,    28,   -28,   -28,
     -28,    27,     0,   -28,   110,    94,   107,   199,   119,   125,
     135,   136,    63,   121,   -28,   108,    72,   -28,    45,   -28,
     -28,   124,    87,   211,   211,   211,   211,   211,   211,   211,
     211,   211,   211,   211,   211,   211,    19,   211,   156,   157,
     151,    38,    38,    19,   211,   211,   211,   -28,   161,   211,
     -28,   -28,   211,   211,   211,   -28,   -28,   -28,   -28,   -28,
     -28,   -28,   -28,   211,   211,   -28,   211,    19,   124,   124,
     -28,   -28,   -28,   262,   248,   276,   276,   276,   276,   276,
     276,   -28,    49,   120,   -28,    38,   -28,   -28,   103,   -28,
      27,    27,    27,    48,   126,   -28,    27,    27,   101,    27,
     -28,     4,   -28,   160,    38,   -28,   169,   -28,   -28,   -28,
     -28,   -28,   -28
  };

  const signed char
  parser::yydefact_[] =
  {
       0,     0,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,    42,    63,    61,    70,    71,    72,     2,    12,
      16,    14,    60,    21,     0,    77,     3,    73,    75,     0,
       9,     4,     0,     0,    60,     1,     0,     0,    43,    45,
      18,    57,     0,    53,    60,     0,     0,     0,     0,     0,
       0,     0,    58,     0,    23,     0,     0,    50,     0,    15,
      20,    26,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
       0,     0,     0,    11,     0,     0,     0,    47,     0,     0,
      44,    17,     0,     0,     0,    65,    66,    67,    68,    69,
      64,    62,    25,     0,     0,    22,     0,     8,    27,    28,
      29,    30,    31,    38,    39,    35,    37,    34,    36,    32,
      33,    40,    41,     0,    78,     0,    74,    80,     0,    10,
       6,     5,     7,     0,     0,    54,    55,    56,    59,    52,
      51,     0,    49,    81,     0,    76,    48,    24,    19,    13,
      79,    81,    46
  };

  const signed char
  parser::yypgoto_[] =
  {
     -28,   -28,    97,    79,     7,    88,   -28,   -27,   -28,     2,
     -28,    91,   111,    99,   -28,    21,    -2,   -28,   -22,   -28,
     -28,   116
  };

  const signed char
  parser::yydefgoto_[] =
  {
       0,     4,    30,    31,    41,    19,    37,    38,    88,    20,
      56,    57,    42,    43,    53,    21,    22,    23,    26,    27,
      28,    80
  };

  const short
  parser::yytable_[] =
  {
      25,    34,    79,    39,    44,    32,    35,    59,    18,    87,
      90,    83,    91,    81,    52,    55,    45,    92,     5,     6,
     149,     7,    25,     8,    33,    45,     9,    10,    46,    47,
      48,    49,    29,    62,    39,    39,    89,    46,    78,    48,
      49,    36,    85,    12,    13,    50,    14,    51,    15,    16,
      17,    24,    86,    45,    50,    13,    51,    14,    45,   126,
     127,    36,    77,    24,    13,    46,    78,    48,    49,    62,
      46,    78,    48,    49,    13,    44,    82,   102,   121,    25,
      25,    34,    50,    83,    13,    32,    13,    50,   105,   106,
      44,   130,   131,   132,    84,    58,   134,   122,    60,    61,
     136,   137,   107,   143,    33,    34,   146,    62,    95,    32,
     138,   139,    45,    55,    62,   148,   104,    36,    13,    93,
     144,    96,   151,    25,    46,    78,    48,    49,    33,    94,
     145,    39,    62,    98,   142,    61,   147,    92,   103,    99,
      13,    50,    25,     1,     2,     3,    65,    66,    67,   100,
     101,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,     5,     6,    40,     7,   125,     8,
      97,   124,     9,    10,   150,    11,     5,     6,   133,     7,
     129,     8,    54,   152,     9,    10,   141,    11,   123,    12,
      13,   135,    14,     0,    15,    16,    17,   140,   128,     0,
       0,    12,    13,     0,    14,     0,    15,    16,    17,     5,
       6,     0,     7,    97,     8,     0,     0,     9,    10,     0,
      11,     5,     6,     0,     7,     0,     8,     0,     0,     9,
      10,     0,    11,     0,    12,    13,     0,    14,     0,    15,
      16,    17,     0,     0,     0,     0,    12,    13,     0,    14,
       0,    15,    16,    17,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    63,    64,
      65,    66,    67,    68,     0,    70,    71,    72,    73,    74,
      75,    76,    63,    64,    65,    66,    67,     0,     0,    70,
      71,    72,    73,    74,    75,    76,    63,    64,    65,    66,
      67,     0,     0,    -1,    -1,    -1,    -1,    -1,    -1,    76
  };

  const short
  parser::yycheck_[] =
  {
       2,     3,    24,     5,     6,     3,     0,     9,     1,    36,
      37,     7,    12,    34,     7,     8,     8,    17,    10,    11,
      16,    13,    24,    15,     3,     8,    18,    19,    20,    21,
      22,    23,    13,     6,    36,    37,     8,    20,    21,    22,
      23,    13,     9,    35,    36,    37,    38,    39,    40,    41,
      42,    13,    19,     8,    37,    36,    39,    38,     8,    81,
      82,    13,    13,    13,    36,    20,    21,    22,    23,     6,
      20,    21,    22,    23,    36,    77,    29,    14,    76,    81,
      82,    83,    37,     7,    36,    83,    36,    37,    16,    17,
      92,    84,    85,    86,     9,    13,    89,    76,    10,    11,
      93,    94,    15,   125,    83,   107,   133,     6,    14,   107,
     103,   104,     8,   106,     6,    14,     8,    13,    36,     9,
      17,    14,   144,   125,    20,    21,    22,    23,   107,    19,
      27,   133,     6,    14,    14,    47,    10,    17,    17,    14,
      36,    37,   144,     3,     4,     5,    22,    23,    24,    14,
      14,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    10,    11,    12,    13,    17,    15,
      14,    14,    18,    19,    14,    21,    10,    11,    17,    13,
      83,    15,    16,    14,    18,    19,   107,    21,    77,    35,
      36,    92,    38,    -1,    40,    41,    42,   106,    82,    -1,
      -1,    35,    36,    -1,    38,    -1,    40,    41,    42,    10,
      11,    -1,    13,    14,    15,    -1,    -1,    18,    19,    -1,
      21,    10,    11,    -1,    13,    -1,    15,    -1,    -1,    18,
      19,    -1,    21,    -1,    35,    36,    -1,    38,    -1,    40,
      41,    42,    -1,    -1,    -1,    -1,    35,    36,    -1,    38,
      -1,    40,    41,    42,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    20,    21,
      22,    23,    24,    25,    -1,    27,    28,    29,    30,    31,
      32,    33,    20,    21,    22,    23,    24,    -1,    -1,    27,
      28,    29,    30,    31,    32,    33,    20,    21,    22,    23,
      24,    -1,    -1,    27,    28,    29,    30,    31,    32,    33
  };

  const signed char
  parser::yystos_[] =
  {
       0,     3,     4,     5,    44,    10,    11,    13,    15,    18,
      19,    21,    35,    36,    38,    40,    41,    42,    47,    48,
      52,    58,    59,    60,    13,    59,    61,    62,    63,    13,
      45,    46,    52,    58,    59,     0,    13,    49,    50,    59,
      12,    47,    55,    56,    59,     8,    20,    21,    22,    23,
      37,    39,    47,    57,    16,    47,    53,    54,    13,    59,
      48,    48,     6,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    13,    21,    61,
      64,    34,    29,     7,     9,     9,    19,    50,    51,     8,
      50,    12,    17,     9,    19,    14,    14,    14,    14,    14,
      14,    14,    14,    17,     8,    16,    17,    15,    48,    48,
      48,    48,    48,    48,    48,    48,    48,    48,    48,    48,
      48,    52,    58,    55,    14,    17,    61,    61,    64,    45,
      47,    47,    47,    17,    47,    56,    47,    47,    47,    47,
      54,    46,    14,    61,    17,    27,    50,    10,    14,    16,
      14,    61,    14
  };

  const signed char
  parser::yyr1_[] =
  {
       0,    43,    44,    44,    44,    45,    45,    45,    46,    46,
      46,    46,    47,    47,    48,    48,    48,    48,    48,    48,
      48,    48,    48,    48,    48,    48,    48,    48,    48,    48,
      48,    48,    48,    48,    48,    48,    48,    48,    48,    48,
      48,    48,    48,    49,    49,    50,    50,    51,    51,    52,
      53,    53,    54,    55,    55,    56,    56,    56,    57,    57,
      58,    58,    58,    59,    59,    59,    59,    59,    59,    59,
      60,    60,    60,    61,    61,    62,    62,    63,    63,    63,
      64,    64
  };

  const signed char
  parser::yyr2_[] =
  {
       0,     2,     2,     2,     2,     3,     3,     3,     0,     1,
       3,     2,     1,     5,     1,     2,     1,     3,     2,     5,
       2,     1,     3,     2,     5,     3,     2,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     1,     1,     2,     1,     5,     1,     3,     4,
       1,     3,     3,     1,     3,     3,     3,     1,     1,     3,
       1,     1,     3,     1,     3,     3,     3,     3,     3,     3,
       1,     1,     1,     1,     3,     1,     4,     1,     3,     5,
       1,     3
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
  "\"@\"", "\"~\"", "\"+\"", "\"-\"", "\"*\"", "\"/\"", "\"%\"", "\"&&\"",
  "\"||\"", "\">\"", "\">=\"", "\"<\"", "\"<=\"", "\"==\"", "\"!=\"",
  "\"+>\"", "\"->\"", "\"_\"", "\"VARID\"", "\"VARSYM\"", "\"QVARID\"",
  "\"QVARSYM\"", "\"STRING\"", "\"INTEGER\"", "\"FLOAT\"", "$accept",
  "start", "def", "defs", "exp", "term", "patterns", "pattern",
  "pattern_tup_args", "fncall", "ditems", "ditem", "args", "arg",
  "tup_args", "qvarid", "varid", "literal", "type", "btype", "atype",
  "type_tup_args", YY_NULLPTR
  };
#endif


#if ZZDEBUG
  const unsigned char
  parser::yyrline_[] =
  {
       0,   147,   147,   148,   149,   151,   152,   153,   155,   156,
     157,   158,   160,   161,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   196,   197,   199,   200,   202,   203,   205,
     207,   208,   210,   212,   213,   215,   216,   217,   219,   220,
     225,   226,   227,   229,   230,   231,   232,   233,   234,   235,
     237,   238,   239,   243,   244,   246,   247,   249,   250,   251,
     253,   254
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
#line 1958 "parser.cc"

#line 262 "parser.y"


using std::optional;
using std::string;
using std::vector;
using std::pair;

void
zz::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}

// Builds one positional argument edge for parser-created calls.
CM::Arg<CM::NoAnn> positional_arg(const CM::UntypedExpr& expr)
{
    return {"", expr, false, false, std::nullopt};
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

// Builds one binary operator call with positional arguments.
CM::UntypedExpr make_binary_call(const string& name, const CM::UntypedExpr& lhs, const CM::UntypedExpr& rhs)
{
    return make_call(name, {positional_arg(lhs), positional_arg(rhs)});
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

ptree make_type_app(ptree type, const vector<ptree>& args)
{
    for(auto& arg: args)
	type = ptree("@APP",{{"",type},{"",arg}});
    return type;
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

// Replaces immediate placeholders in the callee argument list with one stacked
// argument, matching the old ptree `+>` behavior.
int add_arg_placeholder(CM::Call<CM::NoAnn>& call, const CM::UntypedExpr& arg)
{
    int n_placeholders = 0;
    for(auto& call_arg: call.args)
    {
        if (call_arg.value and call_arg.value->is<CM::Placeholder>())
        {
            n_placeholders++;
            call_arg.value = arg;
        }
    }
    return n_placeholders;
}

// Adds a stacked argument to a parser-created call, preserving placeholder
// replacement before falling back to prepending a positional argument.
CM::UntypedExpr add_arg(CM::UntypedExpr arg, CM::UntypedExpr callee)
{
    if (auto var = callee.to<CM::Var>())
        callee = make_call(var->name, {});

    auto call = callee.to<CM::Call<CM::NoAnn>>();
    if (not call)
        throw myexception()<<"Right side of +> must be a function call or function name.";

    int n_placeholders = add_arg_placeholder(*call, arg);
    if (n_placeholders > 1)
	throw myexception()<<"Placeholder '_' may only occur once.";

    if (n_placeholders == 0)
	call->args.insert(call->args.begin(), positional_arg(arg));

    return callee;
}
