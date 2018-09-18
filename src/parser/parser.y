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
  # include "located.H"
  class driver;

  #define LOCATED(obj) {drv.location, obj}
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
  UNDERSCORE    "_"
  AS            "as"
  CASE          "case"
  DATA          "data"
  DEFAULT       "default"
  DERIVING      "deriving"
  DO            "do"
  ELSE          "else"
  HIDING        "hiding"
  IF            "if"
  IMPORT        "import"
  IN            "in"
  INFIX         "infix"
  INFIXL        "infixl"
  INFIXR        "infixr"
  INSTANCE      "instance"
  LET           "let"
  MODULE        "module"
  NEWTYPE       "newtype"
  OF            "of"
  QUALIFIED     "qualified"
  THEN          "then"
  TYPE          "type"
  WHERE         "where"

 /* BAli-Phy extension keyword */
  BUILTIN       "builtin"

 /* GHC extension keywords */
  FORALL        "forall"
  FOREIGN       "foreign"
  EXPORT        "export"
  LABEL         "label"
  DYNAMIC       "dynamic"
  SAFE          "safe"
  INTERRUPTIBLE "interruptible"
  UNSAFE        "unsafe"
  MDO           "mdo"
  FAMILY        "family"
  ROLE          "role"
  STDCALL       "stdcall"
  CCALL         "ccall"
  CAPI          "capi"
  PRIM          "prim"
  JAVASCRIPT    "javascript"
  PROC          "proc"
  REC           "rec"
  GROUP         "group"
  BY            "by"
  USING         "using"
  PATTERN       "pattern"
  STATIC        "static"
  STOCK         "stock"
  ANYCLASS      "anyclass"
  VIA           "via"
  UNIT          "unit"
  SIGNATURE     "signature"
  DEPENDENCY    "dependency"

  INLINE_PRAG             "{-# INLINE"
  SPECIALIZE_PRAG         "{-# SPECIALIZE"
  SPECIALIZE_INLINE_PRAG  "{-# SPECIALIZE_INLINE"
  SOURCE_PRAG             "{-# SOURCE"
  RULES_PRAG              "{-# RULES"
  CORE_PRAG               "{-# CORE"
  SCC_PRAG                "{-# SCC"
  GENERATED_PRAG          "{-# GENERATED"
  DEPRECATED_PRAG         "{-# DEPRECATED"
  WARNING_PRAG            "{-# WARNING"
  UNPACK_PRAG             "{-# UNPACK"
  NOUNPACK_PRAG           "{-# NOUNPACK"
  ANN_PRAG                "{-# ANN"
  MINIMAL_PRAG            "{-# MINIMAL"
  CTYPE_PRAG              "{-# CTYPE"
  OVERLAPPING_PRAG        "{-# OVERLAPPING"
  OVERLAPPABLE_PRAG       "{-# OVERLAPPABLE"
  OVERLAPS_PRAG           "{-# OVERLAPS"
  INCOHERENT_PRAG         "{-# INCOHERENT"
  COMPLETE_PRAG           "{-# COMPLETE"
  CLOSE_PRAG              "#-}"

  DOTDOT        ".."
  COLON         ":"
  DCOLON        "::"
  EQUAL         "="
  LAM           "\\"
  LCASE         "lcase"
  VBAR          "|"
  LARROW        "<-"
  RARROW        "->"
  AT            "@"
  TILDE         "~"
  DARROW        "=>"
  MINUS         "-"
  BANG          "!"
  STAR          "*"
  lARROWTAIL    "-<"
  rARROWTAIL    ">-"
  LARROWTAIL    "-<<"
  RARROWTAIL    ">>-"
  DOT           "."
  TYPEAPP       "TYPEAPP"

  OCURLY        "{"
  CCURLY        "}"
  VOCURLY       "vocurly"
  VCCURLY       "vccurly"
  OBRACK        "["
  CBRACK        "]"
  OPABRACK      "[:"
  CPABRACK      ":]"
  OPAREN        "("
  CPAREN        ")"
  OUBXPAREN     "(#"
  CUBXPAREN     "#)"
  OPARENBAR     "(|"
  CPARENBAR     "|)"
  SEMI          ";"
  COMMA         ","
  BACKQUOTE     "`"
  SIMPLEQUOTE   "'"
;

%token <std::string> VARID    "VARID"
%token <std::string> CONID    "CONID"
%token <std::string> VARSYM   "VARSYM"
%token <std::string> CONSYM   "CONSYM"
%token <std::string> QVARID   "QVARID"
%token <std::string> QCONID   "QCONID"
%token <std::string> QVARSYM  "QVARSYM"
%token <std::string> QCONSYM  "QCONSYM"

%token <char>          CHAR     "CHAR"
%token <std::string>   STRING   "STRING"
%token <int>           INTEGER  "INTEGER"
%token <double>        RATIONAL "RATIONAL"

%token <char>          PRIMCHAR    "PRIMCHAR"
%token <std::string>   PRIMSTRING  "PRIMSTRING"
%token <int>           PRIMINTEGER "PRIMINTEGER"
%token <int>           PRINTWORD   "PRIMWORD"
%token <float>         PRIMFLOAT   "PRIMFLOAT"
%token <double>        PRIMDOUBLE  "PRIMDOUBLE"

 /* DOCNEXT DOCPREV DOCNAMED DOCSECTION: skipped tokens.*/

 /* Template Haskell: skipped tokens.*/

%type  <expression_ref> exp

%type <Located<std::string>> var
%type <Located<std::string>> qvar
%type <Located<std::string>> qvarid
%type <Located<std::string>> varid
%type <Located<std::string>> qvarsym
%type <Located<std::string>> qvarsym_no_minus
%type <Located<std::string>> qvarsym1
%type <Located<std::string>> varsym
%type <Located<std::string>> varsym_no_minus
%type <Located<std::string>> special_id
%type <Located<std::string>> special_sym

%type <Located<std::string>> qconid
%type <Located<std::string>> conid
%type <Located<std::string>> qconsym
%type <Located<std::string>> consym

%type  <Located<expression_ref>> literal
%type  <Located<std::string>> modid
%type  <int> commas

%printer { yyoutput << $$; } <*>;

%%
%start unit;
unit: assignments exp  { std::cout<< $2 << std::endl; };

assignments:
  %empty                 {}
| assignments assignment {};

assignment:
 varid "=" exp { std::cout<< $1 <<" = " << $3 <<std::endl; };
| qvarid "=" exp { std::cout<< $1 <<" = " << $3 <<std::endl; };

exp:
 exp varsym exp   { $$ = expression_ref{var($2),$1,$3}; }
| "(" exp ")"   { std::swap ($$, $2); }
| qvar   { $$ = var($1); }
| literal      { $$ = $1; };


/* ------------- Variables --------------------------------------- */
var: varid { $$ = $1; }
| "(" VARSYM ")" {$$ = LOCATED($2); }

qvar: qvarid { $$ = $1; }
| "(" VARSYM ")" {$$ = LOCATED($2); }
| "(" qvarsym1 ")" {$$ = $2; }

qvarid: varid { $$ = $1; }
| QVARID { $$ = LOCATED($1); }

varid: VARID        { $$ = LOCATED($1); }
| special_id        { $$ = $1; }
| "unsafe"          { $$ = LOCATED("unsafe"); }
| "safe"            { $$ = LOCATED("safe"); }
| "interruptible"   { $$ = LOCATED("interruptible"); }
| "forall"          { $$ = LOCATED("forall"); }
| "family"          { $$ = LOCATED("family"); }
| "role"            { $$ = LOCATED("role"); }

qvarsym: varsym     { $$ = $1; }
| qvarsym1          { $$ = $1; }

qvarsym_no_minus: varsym_no_minus {$$ = $1;}
| qvarsym1 {$$ = $1;}

qvarsym1: QVARSYM   { $$ = LOCATED($1);}

varsym: varsym_no_minus  { $$ = $1; }
|        "-"             { $$ = LOCATED("-"); }

varsym_no_minus: VARSYM {$$ = LOCATED($1);}
| special_sym {$$ = $1;}

special_id:  "as"         { $$ = LOCATED("as"); }
|            "qualified"  { $$ = LOCATED("qualified"); }
|            "hiding"     { $$ = LOCATED("hiding"); }
|            "export"     { $$ = LOCATED("export"); }
|            "label"      { $$ = LOCATED("label"); }
|            "dynamic"    { $$ = LOCATED("dynamic"); }
|            "stdcall"    { $$ = LOCATED("stdcall"); }
|            "ccall"      { $$ = LOCATED("ccall"); }
|            "capi"       { $$ = LOCATED("capi"); }
|            "prim"       { $$ = LOCATED("prim"); }
|            "javascript" { $$ = LOCATED("javascript"); }
|            "group"      { $$ = LOCATED("group"); }
|            "stock"      { $$ = LOCATED("stock"); }
|            "anyclass"   { $$ = LOCATED("anyclass"); }
|            "via"        { $$ = LOCATED("via"); }
|            "unit"       { $$ = LOCATED("unit"); }
|            "dependency" { $$ = LOCATED("dependency"); }
|            "signature"  { $$ = LOCATED("signature"); }

special_sym: "!" { $$ = LOCATED("!"); }
| "." { $$ = LOCATED("."); }
| "*" { $$ = LOCATED("*"); }

/* ------------- Data constructors ------------------------------- */

qconid: conid { $$ = $1; }
| QCONID {$$ = LOCATED($1); }

conid: CONID {$$ = LOCATED($1); }

qconsym: consym {$$ = $1}
| QCONSYM {$$ = LOCATED($1); }

consym: CONSYM { $$ = LOCATED($1); }
| ":" { $$ = LOCATED(":"); }

/* ------------- Literal ----------------------------------------- */

literal: CHAR {$$ = LOCATED($1);}
| STRING {$$ = LOCATED(String($1));}
| INTEGER {$$ = LOCATED($1);}
| RATIONAL {$$ = LOCATED($1);}


/* ------------- Layout ------------------------------------------ */

close: VCCURLY |
error

/* ------------- Miscellaneous (mostly renamings) ---------------- */

modid: CONID {$$ = LOCATED($1);}
| QCONID {$$ = LOCATED($1);}

commas: commas "," {$$ = $1 + 1;}
    | "," {$$ = 1;}


%%

void
yy::parser::error (const location_type& l, const std::string& m)
{
  std::cerr << l << ": " << m << '\n';
}
