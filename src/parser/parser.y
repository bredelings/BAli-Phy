%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.1"

%defines
%define api.token.constructor
%define api.value.type variant
%define parse.assert

%code requires {
  # include <string>
  # include <iostream>
  # include "computation/expression/expression_ref.H"
  # include "computation/expression/var.H"
  class driver;
}

// The parsing context.
%param { driver& drv }

%locations

%define parse.trace
%define parse.error verbose

%code {
# include "driver.hh"
}

%define api.token.prefix {TOK_}
%token
  END  0  "end of file"
  ASSIGN  ":="
  MINUS   "-"
  PLUS    "+"
  STAR    "*"
  SLASH   "/"
  LPAREN  "("
  RPAREN  ")"
;

%token <std::string> IDENTIFIER "identifier"
%token <int> NUMBER "number"
%type  <expression_ref> exp

%printer { yyoutput << $$; } <*>;

%%
%start unit;
unit: assignments exp  { std::cout<< $2 << std::endl; };

assignments:
  %empty                 {}
| assignments assignment {};

assignment:
"identifier" ":=" exp { std::cout<< $1 <<" = " << $3 <<std::endl; };

exp:
  exp "+" exp   { $$ = expression_ref{var("+"),$1,$3}; }
| exp "-" exp   { $$ = expression_ref{var("-"),$1,$3}; }
| exp "*" exp   { $$ = expression_ref{var("*"),$1,$3}; }
| exp "/" exp   { $$ = expression_ref{var("/"),$1,$3}; }
| "(" exp ")"   { std::swap ($$, $2); }
| "identifier"  { $$ = var($1); }
| "number"      { $$ = $1; };
%%

void
yy::parser::error (const location_type& l, const std::string& m)
{
  std::cerr << l << ": " << m << '\n';
}
