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


%type  <Located<expression_ref>> exp

%type <Located<std::string>> conop
%type <Located<std::string>> qconop

%type <Located<std::string>> op
%type <Located<std::string>> varop
%type <Located<std::string>> qop
%type <Located<std::string>> qopm
%type <Located<std::string>> hole_op
%type <Located<std::string>> qvarop
%type <Located<std::string>> qvaropm

%type <Located<std::string>> tyvar
%type <Located<std::string>> tyvarop
%type <Located<std::string>> tyvarid

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
exp op exp   { $$ = {@$,var($2)+$1+$3}; }
| "(" exp ")"   { $$ = {@$,$2}; }
| qvar   { $$ = {@$,var($1)}; }
| literal      { $$ = {@$,$1}; };


conop: consym { $$ = $1; }
|      "`" conid "`" { $$ = $2; }

qconop: qconsym { $$ = $1; }
|      "`" qconid "`" { $$ = $2; }

/* ------------- Type Constructors ------------------------------- */


/* ------------- Operators --------------------------------------- */

op : varop { $$ = $1; }
|    conop { $$ = $1; }

varop: varsym   { $$ = $1; }
| "`" varid "`" { $$ = $2; }

qop:  qvarop    { $$ = $1; }
|     qconop    { $$ = $1; }
|     hole_op   { $$ = $1; }

qopm: qvaropm   { $$ = $1; }
|     qconop    { $$ = $1; }
|     hole_op   { $$ = $1; }

hole_op: "`" "_" "`"  { $$ = {@$, "_"}; }

qvarop: qvarsym  { $$ = $1; }
|       "`" qvarid "`" { $$ = $2; }

qvaropm: qvarsym_no_minus  { $$ =$1; }
| "`" qvarid "`" { $$ = $2; }

/* ------------- Type Variables ---------------------------------- */

tyvar: tyvarid            { $$ = $1; }

tyvarop:  "`" tyvarid "`" { $$ = $2; }

tyvarid: VARID            { $$ = {@$, $1}; }
| special_id              { $$ = $1; }
| "unsafe"                { $$ = {@$, "unsafe"}; }
| "safe"                  { $$ = {@$, "safe"}; }
| "interruptible"         { $$ = {@$, "interruptible"}; }

/* ------------- Variables --------------------------------------- */
var: varid { $$ = $1; }
| "(" VARSYM ")" {$$ = {@$, $2}; }

qvar: qvarid { $$ = $1; }
| "(" VARSYM ")" {$$ = {@$, $2}; }
| "(" qvarsym1 ")" {$$ = $2; }

qvarid: varid { $$ = $1; }
| QVARID { $$ = {@$, $1}; }

varid: VARID        { $$ = {@$, $1}; }
| special_id        { $$ = $1; }
| "unsafe"          { $$ = {@$, "unsafe"}; }
| "safe"            { $$ = {@$, "safe"}; }
| "interruptible"   { $$ = {@$, "interruptible"}; }
| "forall"          { $$ = {@$, "forall"}; }
| "family"          { $$ = {@$, "family"}; }
| "role"            { $$ = {@$, "role"}; }

qvarsym: varsym     { $$ = $1; }
| qvarsym1          { $$ = $1; }

qvarsym_no_minus: varsym_no_minus {$$ = $1;}
|                 qvarsym1 {$$ = $1;}

qvarsym1: QVARSYM        { $$ = {@$, $1}; }

varsym: varsym_no_minus  { $$ = $1; }
|        "-"             { $$ = {@$, "-"}; }

varsym_no_minus: VARSYM      {$$ = {@$, $1}; }
|                special_sym {$$ = $1; }

special_id:  "as"         { $$ = {@$, "as"}; }
|            "qualified"  { $$ = {@$, "qualified"}; }
|            "hiding"     { $$ = {@$, "hiding"}; }
|            "export"     { $$ = {@$, "export"}; }
|            "label"      { $$ = {@$, "label"}; }
|            "dynamic"    { $$ = {@$, "dynamic"}; }
|            "stdcall"    { $$ = {@$, "stdcall"}; }
|            "ccall"      { $$ = {@$, "ccall"}; }
|            "capi"       { $$ = {@$, "capi"}; }
|            "prim"       { $$ = {@$, "prim"}; }
|            "javascript" { $$ = {@$, "javascript"}; }
|            "group"      { $$ = {@$, "group"}; }
|            "stock"      { $$ = {@$, "stock"}; }
|            "anyclass"   { $$ = {@$, "anyclass"}; }
|            "via"        { $$ = {@$, "via"}; }
|            "unit"       { $$ = {@$, "unit"}; }
|            "dependency" { $$ = {@$, "dependency"}; }
|            "signature"  { $$ = {@$, "signature"}; }

special_sym: "!" { $$ = {@$, "!"}; }
|            "." { $$ = {@$, "."}; }
|            "*" { $$ = {@$, "*"}; }

/* ------------- Data constructors ------------------------------- */

qconid:  conid   { $$ = $1; }
|        QCONID  { $$ = {@$, $1}; }

conid:   CONID   { $$ = {@$, $1}; }

qconsym: consym  { $$ = $1; }
|        QCONSYM { $$ = {@$, $1}; }

consym:  CONSYM  { $$ = {@$, $1}; }
|        ":"     { $$ = {@$, ":"}; }

/* ------------- Literal ----------------------------------------- */

literal: CHAR     {$$ = {@$, $1};}
|        STRING   {$$ = {@$,String($1)};}
|        INTEGER  {$$ = {@$, $1};}
|        RATIONAL {$$ = {@$, $1};}


/* ------------- Layout ------------------------------------------ */

close: VCCURLY |
error

/* ------------- Miscellaneous (mostly renamings) ---------------- */

modid: CONID {$$ = {@$, $1};}
| QCONID {$$ = {@$, $1};}

commas: commas "," {$$ = $1 + 1;}
    | "," {$$ = 1;}


%%

void
yy::parser::error (const location_type& l, const std::string& m)
{
  std::cerr << l << ": " << m << '\n';
}
