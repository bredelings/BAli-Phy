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

ptree fold_terms(const std::vector<ptree>& terms);
}

%define api.token.prefix {TOK_}
%token
  END  0  "end of file"

  SEMI          ";"
  EQUAL         "="
  TILDE         "~"
  OBRACK        "["
  CBRACK        "]"
  OPAREN        "("
  CPAREN        ")"
  COMMA         ","
  PLUS          "+"
  AT            "@"
;

%token <std::string> VARID    "VARID"
%token <std::string> QVARID   "QVARID"

%token <std::string>   STRING   "STRING"
%token <int>           INTEGER  "INTEGER"
%token <double>        FLOAT    "FLOAT"

%type <ptree>       exp
%type <ptree>       term
%type <std::vector<ptree>>       terms
%type <std::vector<std::pair<std::string,ptree>>> args
%type <std::pair<std::string,ptree>> arg
%type <std::vector<std::pair<std::string,ptree>>> tup_args

%type <std::string> qvarid
%type <std::string> varid

%type <ptree> literal

%expect 0

 /* Having vector<> as a type seems to be causing trouble with the printer */
 /* %printer { yyoutput << $$; } <*>; */

%%
%start unit;
unit: exp {drv.result = $1;}


exp: terms                                { $$ = fold_terms($1); }
|           "(" exp ")"                   { $$ = $2; }
|           varid "=" exp ";" exp  { $$ = ptree("let",{{$1,$3},{"",$5}}); }

terms: term                 { $$.push_back($1);}
|      terms "+" term       { $$ = $1; $$.push_back($3);}

// See parse_no_submodel( )
term: qvarid                      { $$ = ptree($1); }
|     "@" varid                   { $$ = ptree("@"+$2); }
|     qvarid "[" args "]"         { $$ = ptree($1,$3); }
|     qvarid "(" args ")"         { $$ = ptree($1,$3); }
|     "[" args "]"                { $$ = ptree("List",$2); }
|     "[" "]"                     { $$ = ptree("List",{}); }
|     "(" tup_args "," exp ")"    { $2.push_back({"",$4}); $$ = ptree("Tuple",$2); }
|     "~" term                    { $$ = add_sample($2); }
|     literal                     { $$ = $1; }

args: arg                 { $$.push_back($1); }
|     args "," arg        { $$ = $1; $$.push_back($3); }
|     args ","            { $$ = $1; $$.push_back({}); }

arg: varid "=" exp { $$ = {$1,$3}; }
|    varid "~" exp { $$ = {$1,add_sample($3)}; }
|    exp           { $$ = {"",$1}; }

tup_args: exp               { $$.push_back({"",$1});}
|         tup_args "," exp  { $$ = $1; $1.push_back({"",$3});}


/* ------------- Literal ----------------------------------------- */

qvarid: varid  { $$ = $1; }
|       QVARID { $$ = $1; }

varid: VARID        { $$ = $1; }

literal: STRING      {$$ = ptree('"' + $1 + '"');}
|        INTEGER     {$$ = ptree($1);}
|        FLOAT       {$$ = ptree($1);}


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
