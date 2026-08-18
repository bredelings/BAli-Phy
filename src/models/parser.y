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
  # include "models/model-expr.H"
  # include "models/model-type.H"
  # include "range/v3/all.hpp"

  namespace views = ranges::views;

  class zz_driver;

  CM::UntypedExpr make_function(const std::vector<CM::UntypedPattern>& patterns, const CM::UntypedExpr& body);
  std::pair<std::string,CM::UntypedExpr> make_function_def(zz_driver&, const yy::location&, const CM::UntypedExpr& fncall, const CM::UntypedExpr& body);

}

// The parsing context.
%param { zz_driver& drv }

%locations

%define parse.trace
%define parse.error verbose

%code {
# include "driver.hh"
# include "parse.H"
# include "util/myexception.H"

CM::UntypedExpr make_call(const std::string& name, const std::vector<CM::Arg<CM::NoAnn>>& args);
CM::UntypedExpr make_list(const std::vector<CM::Arg<CM::NoAnn>>& args);
CM::UntypedExpr make_list(const std::vector<CM::UntypedExpr>& elements);
CM::UntypedExpr make_sample(const CM::UntypedExpr& dist);
CM::UntypedExpr make_model_tuple(const std::vector<CM::UntypedExpr>& elements);
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
  OPAREN        "("
  CPAREN        ")"
  OCURLY        "{"
  CCURLY        "}"
  COMMA         ","
  AT            "@"
  BACKQUOTE     "`"

  TILDE         "~"
  /* Minus stays distinct because it can introduce a prefix expression. */
  MINUS         "-"

  GT            ">"
  LT            "<"

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

%type <CM::UntypedExpr> exp
%type <CM::UntypedExpr> infix_exp
%type <CM::UntypedExpr> prefix_exp
%type <CM::UntypedExpr> atom
%type <std::vector<std::pair<Located<std::string>,CM::UntypedExpr>>> infix_terms
%type <Located<std::string>> infix_operator
%type <CM::UntypedExpr> fncall
%type <std::vector<CM::Arg<CM::NoAnn>>> args
%type <CM::Arg<CM::NoAnn>> arg
%type <std::vector<CM::UntypedExpr>> ditems
%type <CM::UntypedExpr> ditem
%type <std::pair<std::string,CM::UntypedExpr>> def
%type <CM::Decls<CM::NoAnn>> defs
%type <std::vector<CM::UntypedExpr>> tup_args

%type <CM::Type>       type
%type <CM::Type>       atype
%type <CM::Type>       btype
%type <std::vector<CM::Type>> type_tup_args

%type <std::string> qvarid
%type <std::string> varid

%type <CM::UntypedPattern> pattern
%type <std::vector<CM::UntypedPattern>> patterns
%type <std::vector<CM::UntypedPattern>> pattern_tup_args

%type <CM::UntypedExpr> literal

%expect 0

 /* Having vector<> as a type seems to be causing trouble with the printer */
 /* %printer { yyoutput << $$; } <*>; */

%%
%start start;
start: START_EXP exp {drv.expression_result = $2;}
|      START_TYPE type {drv.type_result = $2;}
|      START_DEFS defs {drv.defs_result = $2;}

def: varid                      "=" exp  { $$ = {$1,$3}; }
|    fncall                     "=" exp  { $$ = make_function_def(drv,@1,$1,$3); }
|    varid                      "~" exp  { $$ = {$1,make_sample($3)}; }

defs: %empty       { $$ = {}; }
|     def          { $$ = {$1}; }
|     defs ";" def { $$ = $1; $$.push_back($3); }
|     defs ";"     { $$ = $1; }

exp: infix_exp                     { $$ = $1; }
|    exp "where" "{" defs "}"      { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Let<CM::NoAnn>{$4, $1}}; }

infix_exp: prefix_exp              { $$ = $1; }
|          prefix_exp infix_terms  { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Infix<CM::NoAnn>{$1, $2}}; }

infix_terms: infix_operator prefix_exp
             { $$ = {{$1, $2}}; }
|            infix_terms infix_operator prefix_exp
             { $$ = $1; $$.push_back({$2, $3}); }

infix_operator: VARSYM  { $$ = Located<std::string>{@1, $1}; }
|               QVARSYM { $$ = Located<std::string>{@1, $1}; }
|               "`" VARID "`"  { $$ = Located<std::string>{@2, $2}; }
|               "`" QVARID "`" { $$ = Located<std::string>{@2, $2}; }
|               "-"     { $$ = Located<std::string>{@1, "-"}; }
|               "+>"    { $$ = Located<std::string>{@1, "+>"}; }

prefix_exp: atom                 { $$ = $1; }
|           "~" prefix_exp       { $$ = make_sample($2); }
|           "-" prefix_exp       { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::PrefixNeg<CM::NoAnn>{{@1, "-"}, $2}}; }

// See parse_no_submodel( )
atom: qvarid                      { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Var{$1}}; }
|     "@" varid                   { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::ArgRef{$2}}; }
|     fncall                      { $$ = $1; }
|     "[" args "]"                { $$ = make_list($2); }
|     "[" "]"                     { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::List<CM::NoAnn>{}}; }
|     "(" tup_args "," exp ")"    { $2.push_back($4); $$ = make_model_tuple($2); }
|     literal                     { $$ = $1; }
|     "{" ditems "}"              { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Dictionary<CM::NoAnn>{$2}}; }
|     "{" "}"                     { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Dictionary<CM::NoAnn>{}}; }
|     "|" patterns ":" exp "|"    { $$ = make_function($2, $4);}
|     "(" exp ")"                 { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Infix<CM::NoAnn>{$2, {}}}; }
|     "_"                         { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Placeholder{}}; }


patterns: pattern           { $$.push_back($1); }
|         patterns pattern   { $$ = $1; $$.push_back($2); }

pattern: varid                                      { $$ = CM::UntypedPattern{CM::NoAnn{}, CM::VarPattern{$1}}; }
|        "(" pattern_tup_args "," pattern ")"       { $2.push_back($4); $$ = CM::UntypedPattern{CM::NoAnn{}, CM::TuplePattern<CM::NoAnn>{$2}}; }

pattern_tup_args: pattern                           { $$.push_back($1);}
|                pattern_tup_args "," pattern       { $$ = $1; $$.push_back($3);}

fncall: qvarid "(" args ")"         { $$ = make_call($1,$3); }

ditems: ditem                     { $$.push_back($1); }
|       ditems "," ditem          { $$ = $1; $$.push_back($3); }

ditem: exp ":" exp  { $$ = make_model_tuple({$1,$3}); }

args: arg                 { $$.push_back($1); }
|     args "," arg        { $$ = $1; $$.push_back($3); }

arg: varid "=" exp { $$ = {$1,$3,false,false,std::nullopt}; }
|    varid "~" exp { $$ = {$1,make_sample($3),false,false,std::nullopt}; }
|    exp           { $$ = {"",$1,false,false,std::nullopt}; }

tup_args: exp               { $$.push_back($1);}
|         tup_args "," exp  { $$ = $1; $$.push_back($3);}


/* ------------- Literal ----------------------------------------- */

qvarid: varid  { $$ = $1; }
|       QVARID { $$ = $1; }
|       "(" QVARSYM ")" { $$ = $2; }

varid: VARID        { $$ = $1; }
|       "(" VARSYM ")" { $$ = $2; }
|       "(" ":" ")" { $$ = ":"; }
|       "(" "-" ")" { $$ = "-"; }

literal: STRING      {$$ = CM::UntypedExpr{CM::NoAnn{}, CM::StringLiteral{$1}};}
|        INTEGER     {$$ = CM::UntypedExpr{CM::NoAnn{}, CM::IntLiteral{$1}};}
|        FLOAT       {$$ = CM::UntypedExpr{CM::NoAnn{}, CM::DoubleLiteral{$1}};}

/* -------------------------------------------------------------- */

type: btype                             { $$ = $1; }
|     btype "->" type                   { $$ = CM::type_apps("Function",{$1,$3});  }

btype: atype                            { $$ = $1; }
|      atype "<" type_tup_args ">"      { $$ = CM::type_apps($1, $3); }

atype: varid                            { $$ = CM::type_atom($1); }
|      "(" type ")"                     { $$ = $2; }
|      "(" type_tup_args "," type ")"   { $2.push_back($4); $$ = CM::type_apps(CM::type_con("Tuple"),$2); }

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
