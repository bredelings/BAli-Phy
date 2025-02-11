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
  # include "range/v3/all.hpp"

  namespace views = ranges::views;

  class zz_driver;

  ptree make_function(const std::vector<std::string>& vars, const ptree& body);
  ptree make_type_app(ptree type, const std::vector<ptree>& args);
  std::pair<std::string,ptree> make_function_def(zz_driver&, const yy::location&, const ptree& fncall, const ptree& body);

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
  START_DEFS 3

  WHERE         "where"
  SEMI          ";"
  COLON         ":"
  EQUAL         "="
  BAR           "|"
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
%token <std::string> QVARSYM  "QVARSYM"

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
%type <std::pair<std::string,ptree>>              def
%type <ptree> defs

%type <ptree>       type
%type <ptree>       atype
%type <ptree>       btype
%type <std::vector<ptree>> type_tup_args

%type <std::string> qvarid
%type <std::string> varid

%type <std::vector<std::string>> varids

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
|      START_DEFS defs {drv.result = $2;}

def: varid                      "=" exp  { $$ = {$1,$3}; }
|    fncall                     "=" exp  { $$ = make_function_def(drv,@1,$1,$3); }
|    varid                      "~" exp  { $$ = {$1,add_sample($3)}; }

defs: %empty       { $$ = ptree("!Decls"); }
|     def          { $$ = ptree("!Decls", {$1}); }
|     defs ";" def { $$ = $1; $$.push_back($3); }
|     defs ";"     { $$ = $1; }

exp: term                          { $$ = $1; }
|    exp "where" "{" defs "}"      { $$ = ptree("!let",{{"decls",$4},{"body",$1}}); }


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
|    "|" varids ":" exp "|"       { $$ = make_function($2, $4);}
|    "(" exp ")"                  { $$ = $2; }
|     "-" term                    { $$ = ptree("negate",{{"",ptree($2)}}); }
|     term "+" term               { $$ = ptree("+",{{"",ptree($1)},{"",$3}}); }
|     term "-" term               { $$ = ptree("-",{{"",ptree($1)},{"",$3}}); }
|     term "*" term               { $$ = ptree("*",{{"",ptree($1)},{"",$3}}); }
|     term "/" term               { $$ = ptree("/",{{"",ptree($1)},{"",$3}}); }
|     term "+>" fncall            { $$ = add_arg($1,$3); }
|     term "+>" qvarid            { $$ = add_arg($1,ptree($3)); }
|     "_"                         { $$ = ptree("_"); }


varids: varid           { $$.push_back($1); }
|       varids varid    { $$ = $1; $$.push_back($2); }

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
|       "(" QVARSYM ")" { $$ = $2; }

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

type: btype                             { $$ = $1; }
|     btype "->" type                   { $$ = make_type_app("Function",{$1,$3});  }

btype: atype                            { $$ = $1; }
|      atype "<" type_tup_args ">"      { $$ = make_type_app($1, $3); }

atype: varid                            { $$ = ptree($1); }
|      "(" type ")"                     { $$ = $2; }
|      "(" type_tup_args "," type ")"   { $2.push_back($4); $$ = make_type_app(ptree("Tuple"),$2); }

type_tup_args: type                     { $$.push_back($1);}
|              type_tup_args "," type   { $$ = $1; $$.push_back($3);}


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
