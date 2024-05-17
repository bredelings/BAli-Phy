%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.4"

%defines
%define api.prefix  {zz}
%define api.namespace  {zz}
%code requires {#include "computation/parser/location.hh"}
%define api.location.type {yy::location}
%define api.token.constructor
%define api.value.type variant
// This could offer a speedup, but doesn't work with `if ($7) e.push_back($7)`.
// %define api.value.automove
%define parse.assert

%code requires {
  # include <string>
  # include <iostream>
  # include <vector>
  # include <tuple>
  # include "util/ptree.H"

  class zz_driver;

}

// The parsing context.
%param { zz_driver& drv }

%locations

%define parse.trace
%define parse.error verbose

%code {
# include "driver.hh"
# include "parse.H"

ptree add_arg(const ptree& p1, const ptree& p2);
}

%define api.token.prefix {TOK_}
%token
  END  0  "end of file"
  START_EXP 1
  START_TYPE 2

  FUNCTION      "function"
  SEMI          ";"
  COLON         ":"
  EQUAL         "="
  OBRACK        "["
  CBRACK        "]"
  OANGLE        "<"
  CANGLE        ">"
  OPAREN        "("
  CPAREN        ")"
  OCURLY        "{"
  CCURLY        "}"
  COMMA         ","
  AT            "@"

  TILDE         "~"
  PLUS          "+"
  MINUS         "-"
  TIMES         "*"
  DIVIDE        "/"

  STACK         "+>"
  ARROW         "->"
  PLACEHOLDER   "_"
;

%token <std::string> VARID    "VARID"
%token <std::string> VARSYM   "VARSYM"
%token <std::string> QVARID   "QVARID"

%token <std::string>   STRING   "STRING"
%token <int>           INTEGER  "INTEGER"
%token <double>        FLOAT    "FLOAT"

%type <ptree>       exp
%type <ptree>       term
%type <ptree>       fncall
%type <std::vector<std::pair<std::string,ptree>>> args
%type <std::pair<std::string,ptree>> arg
%type <std::vector<std::pair<std::string,ptree>>> ditems
%type <ptree> ditem
%type <std::vector<std::pair<std::string,ptree>>> tup_args

%type <ptree>       type
%type <std::vector<std::pair<std::string,ptree>>> type_tup_args

%type <std::string> qvarid
%type <std::string> varid

%type <ptree> literal

%expect 0

 /* Having vector<> as a type seems to be causing trouble with the printer */
 /* %printer { yyoutput << $$; } <*>; */

%left "+>"
%left "+" "-"
%left "*" "/"
%left "~"
%right "->"

%%
%start start;
start: START_EXP exp {drv.result = $2;}
|      START_TYPE type {drv.result = $2;}


exp: term                                     { $$ = $1; }
|    varid "=" exp ";" exp                    { $$ = ptree("let",{{$1,$3},{"",$5}}); }
|    varid "~" exp ";" exp                    { $$ = ptree("let",{{$1,add_sample($3)},{"",$5}}); }


// See parse_no_submodel( )
term: qvarid                      { $$ = ptree($1); }
|     "@" varid                   { $$ = ptree("@"+$2); }
|     fncall                      { $$ = $1; }
|     "[" args "]"                { $$ = ptree("List",$2); }
|     "[" "]"                     { $$ = ptree("List",{}); }
|     "(" tup_args "," exp ")"    { $2.push_back({"",$4}); $$ = ptree("Tuple",$2); }
|     "~" term                    { $$ = add_sample($2); }
|     literal                     { $$ = $1; }
|     "{" ditems "}"              { $$ = ptree("List",$2); }
|     "{" "}"                     { $$ = ptree("List",{}); }
|    "function" "(" varid ":" exp ")"         { $$ = ptree("function",{{"",ptree($3)},{"",$5}}); }
|    "(" exp ")"                              { $$ = $2; }
|     "-" term                    { $$ = ptree("negate",{{"",ptree($2)}}); }
|     term "+" term               { $$ = ptree("+",{{"",ptree($1)},{"",$3}}); }
|     term "-" term               { $$ = ptree("-",{{"",ptree($1)},{"",$3}}); }
|     term "*" term               { $$ = ptree("*",{{"",ptree($1)},{"",$3}}); }
|     term "/" term               { $$ = ptree("/",{{"",ptree($1)},{"",$3}}); }
|     term "+>" fncall            { $$ = add_arg($1,$3); }
|     term "+>" qvarid            { $$ = add_arg($1,ptree($3)); }
|     "_"                         { $$ = ptree("_"); }


fncall: qvarid "(" args ")"         { $$ = ptree($1,$3); }


ditems: ditem                     { $$.push_back({"",$1}); }
|       ditems "," ditem          { $$ = $1; $$.push_back({"",$3}); }

ditem: exp ":" exp  { $$ = ptree("Tuple",{{"",$1},{"",$3}}); }

args: arg                 { $$.push_back($1); }
|     args "," arg        { $$ = $1; $$.push_back($3); }

arg: varid "=" exp { $$ = {$1,$3}; }
|    varid "~" exp { $$ = {$1,add_sample($3)}; }
|    exp           { $$ = {"",$1}; }

tup_args: exp               { $$.push_back({"",$1});}
|         tup_args "," exp  { $$ = $1; $1.push_back({"",$3});}


/* ------------- Literal ----------------------------------------- */

qvarid: varid  { $$ = $1; }
|       QVARID { $$ = $1; }

varid: VARID        { $$ = $1; }
|       "(" VARSYM ")" { $$ = $2; }
|       "(" ":" ")" { $$ = ":"; }
|       "(" "+" ")" { $$ = "+"; }
|       "(" "-" ")" { $$ = "-"; }
|       "(" "*" ")" { $$ = "*"; }
|       "(" "/" ")" { $$ = "/"; }

literal: STRING      {$$ = ptree('"' + $1 + '"');}
|        INTEGER     {$$ = ptree($1);}
|        FLOAT       {$$ = ptree($1);}

/* -------------------------------------------------------------- */

type: varid                           { $$ = ptree($1); }
|     varid "<" type_tup_args ">"     { $$ = ptree($1, $3); }
|     "(" type ")"                    { $$ = $2; }
|     "(" type_tup_args "," type ")"  { $2.push_back({"",$4}); $$ = ptree("Tuple",$2); }
|     type "->" type                  { $$ = ptree("Function",{{"",$1},{"",$3}});  }

type_tup_args: type               { $$.push_back({"",$1});}
|              type_tup_args "," type  { $$ = $1; $$.push_back({"",$3});}


       /* Without the yyerrok, the yyerror seems not to be called at the end of the file, 
          so that the drv.pop_error_message() causes a SEGFAULT. */
/* error { yyerrok; } */


%%

using std::optional;
using std::string;
using std::vector;
using std::pair;

void
zz::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message(l,m);
}

