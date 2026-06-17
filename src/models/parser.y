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
  # include "util/ptree.H"
  # include "range/v3/all.hpp"

  namespace views = ranges::views;

  class zz_driver;

  CM::UntypedExpr make_function(const std::vector<CM::UntypedPattern>& patterns, const CM::UntypedExpr& body);
  ptree make_type_app(ptree type, const std::vector<ptree>& args);
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

CM::UntypedExpr add_arg(CM::UntypedExpr p1, CM::UntypedExpr p2);
CM::UntypedExpr make_binary_call(const std::string& name, const CM::UntypedExpr& lhs, const CM::UntypedExpr& rhs);
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

  TILDE         "~"
  PLUS          "+"
  MINUS         "-"
  TIMES         "*"
  DIVIDE        "/"
  MOD           "%"

  AND           "&&"
  OR            "||"
  GT            ">"
  GE            ">="
  LT            "<"
  LE            "<="
  EQ            "=="
  NE            "!="

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
%type <CM::UntypedExpr> term
%type <CM::UntypedExpr> fncall
%type <std::vector<CM::Arg<CM::NoAnn>>> args
%type <CM::Arg<CM::NoAnn>> arg
%type <std::vector<CM::UntypedExpr>> ditems
%type <CM::UntypedExpr> ditem
%type <std::pair<std::string,CM::UntypedExpr>> def
%type <CM::Decls<CM::NoAnn>> defs
%type <std::vector<CM::UntypedExpr>> tup_args

%type <ptree>       type
%type <ptree>       atype
%type <ptree>       btype
%type <std::vector<ptree>> type_tup_args

%type <std::string> qvarid
%type <std::string> varid

%type <CM::UntypedPattern> pattern
%type <std::vector<CM::UntypedPattern>> patterns
%type <std::vector<CM::UntypedPattern>> pattern_tup_args

%type <CM::UntypedExpr> literal

%expect 0

 /* Having vector<> as a type seems to be causing trouble with the printer */
 /* %printer { yyoutput << $$; } <*>; */

%left "||"
%left "&&"
%nonassoc "==" "!=" "<" ">" "<=" ">="
%left "+>"
%left "+" "-"
%left "*" "/" "%"
%left "~"
%right "->"

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

exp: term                          { $$ = $1; }
|    exp "where" "{" defs "}"      { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Let<CM::NoAnn>{$4, CM::Box<CM::UntypedExpr>($1)}}; }


// See parse_no_submodel( )
term: qvarid                      { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::Var{$1}}; }
|     "@" varid                   { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::ArgRef{$2}}; }
|     fncall                      { $$ = $1; }
|     "[" args "]"                { $$ = make_list($2); }
|     "[" "]"                     { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::List<CM::NoAnn>{}}; }
|     "(" tup_args "," exp ")"    { $2.push_back($4); $$ = make_model_tuple($2); }
|     "~" term                    { $$ = make_sample($2); }
|     literal                     { $$ = $1; }
|     "{" ditems "}"              { $$ = make_list($2); }
|     "{" "}"                     { $$ = CM::UntypedExpr{CM::NoAnn{}, CM::List<CM::NoAnn>{}}; }
|    "|" patterns ":" exp "|"     { $$ = make_function($2, $4);}
|    "(" exp ")"                  { $$ = $2; }
|     "-" term                    { $$ = make_call("negate", {{ "", CM::Box<CM::UntypedExpr>($2), false, false, std::nullopt }}); }
|     term "+" term               { $$ = make_binary_call("+", $1, $3); }
|     term "-" term               { $$ = make_binary_call("-", $1, $3); }
|     term "*" term               { $$ = make_binary_call("*", $1, $3); }
|     term "/" term               { $$ = make_binary_call("/", $1, $3); }
|     term "%" term               { $$ = make_binary_call("%", $1, $3); }
|     term "==" term              { $$ = make_binary_call("==", $1, $3); }
|     term "!=" term              { $$ = make_binary_call("!=", $1, $3); }
|     term "<" term               { $$ = make_binary_call("<", $1, $3); }
|     term ">" term               { $$ = make_binary_call(">", $1, $3); }
|     term "<=" term              { $$ = make_binary_call("<=", $1, $3); }
|     term ">=" term              { $$ = make_binary_call(">=", $1, $3); }
|     term "&&" term              { $$ = make_binary_call("&&", $1, $3); }
|     term "||" term              { $$ = make_binary_call("||", $1, $3); }
|     term "+>" fncall            { $$ = add_arg($1,$3); }
|     term "+>" qvarid            { $$ = add_arg($1,CM::UntypedExpr{CM::NoAnn{}, CM::Var{$3}}); }
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

arg: varid "=" exp { $$ = {$1,CM::Box<CM::UntypedExpr>($3),false,false,std::nullopt}; }
|    varid "~" exp { $$ = {$1,CM::Box<CM::UntypedExpr>(make_sample($3)),false,false,std::nullopt}; }
|    exp           { $$ = {"",CM::Box<CM::UntypedExpr>($1),false,false,std::nullopt}; }

tup_args: exp               { $$.push_back($1);}
|         tup_args "," exp  { $$ = $1; $$.push_back($3);}


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

literal: STRING      {$$ = CM::UntypedExpr{CM::NoAnn{}, CM::StringLiteral{$1}};}
|        INTEGER     {$$ = CM::UntypedExpr{CM::NoAnn{}, CM::IntLiteral{$1}};}
|        FLOAT       {$$ = CM::UntypedExpr{CM::NoAnn{}, CM::DoubleLiteral{$1}};}

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

// Builds one positional argument edge for parser-created calls.
CM::Arg<CM::NoAnn> positional_arg(const CM::UntypedExpr& expr)
{
    return {"", CM::Box<CM::UntypedExpr>(expr), false, false, std::nullopt};
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
        auto& state = args[0].value->get();
        if (auto var = std::get_if<CM::Var>(&state.node))
            return {CM::NoAnn{}, CM::GetState{var->name}};
        if (auto str = std::get_if<CM::StringLiteral>(&state.node))
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
        list.elements.push_back(arg.value->get());
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
    return {CM::NoAnn{}, CM::Sample<CM::NoAnn>{CM::Box<CM::UntypedExpr>(dist)}};
}

// Builds nested unary lambda nodes for the parser's lambda syntax.
CM::UntypedExpr make_function(const vector<CM::UntypedPattern>& patterns, const CM::UntypedExpr& body)
{
    auto f = body;
    for(auto& pattern: patterns | views::reverse)
    {
        f = {
            CM::NoAnn{},
            CM::Lambda<CM::NoAnn>{
                CM::Box<CM::UntypedPattern>(std::move(pattern)),
                CM::Box<CM::UntypedExpr>(std::move(f))
            }
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
    auto call = std::get_if<CM::Call<CM::NoAnn>>(&fncall.node);
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
	else if (auto var = std::get_if<CM::Var>(&arg.value->get().node))
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
        if (call_arg.value and std::holds_alternative<CM::Placeholder>(call_arg.value->get().node))
        {
            n_placeholders++;
            call_arg.value = CM::Box<CM::UntypedExpr>(arg);
        }
    }
    return n_placeholders;
}

// Adds a stacked argument to a parser-created call, preserving placeholder
// replacement before falling back to prepending a positional argument.
CM::UntypedExpr add_arg(CM::UntypedExpr arg, CM::UntypedExpr callee)
{
    if (auto var = std::get_if<CM::Var>(&callee.node))
        callee = make_call(var->name, {});

    auto call = std::get_if<CM::Call<CM::NoAnn>>(&callee.node);
    if (not call)
        throw myexception()<<"Right side of +> must be a function call or function name.";

    int n_placeholders = add_arg_placeholder(*call, arg);
    if (n_placeholders > 1)
	throw myexception()<<"Placeholder '_' may only occur once.";

    if (n_placeholders == 0)
	call->args.insert(call->args.begin(), positional_arg(arg));

    return callee;
}
