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

%token <std::string> IPDUPVARID "IPDUPVARID" /* extension: implicit param ?x */
%token <std::string> LABELVARID "LABELVARID" /* Overladed label: #x */

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


%type <Located<expression_ref>> exp

 /* %type <Located<std::string>> qcon_nowiredlist */
%type <Located<std::string>> qcon
%type <Located<std::string>> gen_qcon
%type <Located<std::string>> con
%type <Located<std::string>> sysdcon_no_list
%type <Located<std::string>> sysdcon
%type <Located<std::string>> conop
%type <Located<std::string>> qconop

%type <Located<std::string>> gtycon
%type <Located<std::string>> ntgtycon
%type <Located<std::string>> oqtycon
%type <Located<std::string>> oqtycon_no_varcon
%type <Located<std::string>> qtyconop
%type <Located<std::string>> qtycondoc
%type <Located<std::string>> qtycon
%type <Located<std::string>> tycon
%type <Located<std::string>> qtyconsym
%type <Located<std::string>> tyconsym

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
%type  <int> bars0
%type  <int> bars

%printer { yyoutput << $$; } <*>;

%%
%start unit;
unit: module

/* ------------- Identifiers ------------------------------------- /
identifier: qvar
|           qcon
|           qvarop
|           qconop
|           "(" "->" ")"
|           "(" "~" ")"
*/

/* ------------- Backpack stuff ---------------------------------- */

/* ------------- Module header ----------------------------------- */

/* signature: backpack stuff */

module: "module" modid maybemodwarning maybeexports "where" body
| body2

missing_module_keyword: %empty

/* BACKPACK: implicit_top: %empty */

maybemodwarning: "{-# DEPRECATED" strings "#-}"
|                "{-# WARNING" strings "#-}"
|                %empty

body: "{" top "}"
|     VOCURLY top close

body2: "{" top "}"
|     missing_module_keyword top close


top: semis top1

top1: importdecls_semi topdecls_semi
|     importdecls_semi topdecls
|     importdecls

/* ------------- Module declaration and imports only ------------- */

/* Skip backpack stuff */

/* ------------- The Export List --------------------------------- */

maybeexports: "(" exportlist ")"
|             %empty

exportlist: exportlist1

exportlist1: export "," exportlist1
|            export

export: qcname_ext export_subspec
|       "module" modid
|       "pattern" qcon

export_subspec: %empty
|              "(" qcnames ")"

qcnames: %empty
|        qcnames1

qcnames1 : qcnames1 "," qcname_ext_w_wildcard
|          qcname_ext_w_wildcard

qcname_ext_w_wildcard: qcname_ext
| ".."

qcname_ext: qcname
|           "type" oqtycon

qcname: qvar
|       oqtycon_no_varcon

/* ------------- Import Declarations ----------------------------- */

semis1: semis1 ";"
|       ";"

semis: semis ";"
|      %empty

importdecls: importdecls_semi importdecls

importdecls_semi: importdecls_semi importdecl semis1
|                 %empty

importdecl: "import" maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec

maybe_src: "{-# SOURCE" "#-}"
|          %empty

maybe_safe: "safe"
|           %empty

maybe_pkg: STRING
|          %empty

optqualified: "qualified"
|             %empty

maybeas:  "as" modid
|         %empty

maybeimpspec: impspec
|             %empty

impspec: "(" exportlist ")"
|        "hiding" "(" exportlist ")"


/* ------------- Fixity Declarations ----------------------------- */

prec: %empty
|     INTEGER

infix: "infix"
|      "infixl"
|      "infixr"

ops:   ops "," op
|      op

/* ------------- Top-Level Declarations -------------------------- */

topdecls: topdecls_semi topdecl

topdecls_semi: topdecls_semi topdecl semis1
|              %empty

topdecl: cl_decl
|        ty_decl
|        inst_decl
/*|        stand_alone_deriving
  |        role_annot*/
|        "default" "(" comma_types0 ")"
/*
|        "foreign" fdecl
|        "{-# DEPRECATED" deprecations "#-}"
|        "{-# WARNING" warnings "#-}"
|        "{-# RULES" rules "#-}"
|        annotation*/
|        decl_no_th
|        infixexp_top
|        "builtin" var INTEGER STRING STRING
|        "builtin" var INTEGER STRING
|        "builtin" varop INTEGER STRING STRING
|        "builtin" varop INTEGER STRING

cl_decl: "class" tycl_hdr fds where_cls

ty_decl: "type" type "=" ctypedoc
/* |        "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family */
|        data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
|        data_or_newtype capi_ctype tycl_hdr opt_kind_sig
/* |        "data" "family" type opt_datafam_kind_sig */

inst_decl: "instance" overlap_pragma inst_type where_inst
/* |          "type" "instance" ty_fam_inst_eqn */
|          data_or_newtype "instance" capi_ctype tycl_hdr constrs
|          data_or_newtype "instance" capi_ctype opt_kind_sig

overlap_pragma: "{-# OVERLAPPABLE" "#-}"
|               "{-# OVERLAPPING" "#-}"
|               "{-# OVERLAPS" "#-}"
|               "{-# INCOHERENT" "#-}"
|               %empty
   
deriv_strategy_no_via: "stock"
|                      "anyclass"
|                      "newtype"

deriv_strategy_via: "via" type

/*
deriv_standalone_strategy: "stock"
|                          "anyclass"
|                          "newtype"
|                          %empty
*/

/* Injective type families 

opt_injective_info: %empty
|                   "|" injectivity_cond

injectivity_cond: tyvarid "->" inj_varids

inj_varids: inj_varids tyvarid
|           tyvarid
*/
/* Closed type families 

where_type_family: %empty
|                  "where" ty_fam_inst_eqn_list

ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"
|                     VOCURLY ty_fam_inst_eqns close
|                     "{" ".." "}"
|                     VOCURLY ".." close

ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn
|                 ty_fam_inst_eqn ";"
|                 ty_fam_inst_eqn
|                 %empty

ty_fam_inst_eqn: type "=" ctype

at_decl_cls: "data" opt_family type opt_datafam_kind_sig
|            "type" type opt_at_kind_inj_sig
*/

data_or_newtype: "data"
|                "newtype"

opt_kind_sig: %empty
|             "::" kind

/*opt_datafam_kind_sig: %empty
|                     "::" kind

 opt_tyfam_kind:sigm: %empty */

/* opt_tyfam_at_kind_inj_sig: */

tycl_hdr: context "=>" type
|         type

capi_ctype: "{-# CTYPE" STRING STRING "#-}"
|           "{-# CTYPE" STRING "#-}"
|           %empty

/* ------------- Stand-alone deriving ---------------------------- */

/* ------------- Role annotations -------------------------------- */
/*
role_annot: "type" "role" oqtycon maybe_roles

maybe_roles: %empty
|            roles

roles:       role
|            roles role

role:        VARID
|            "_"
*/
pattern_synonym_decl: "pattern" pattern_synonym_lhs "=" pat
|                     "pattern" pattern_synonym_lhs "<-" pat
|                     "pattern" pattern_synonym_lhs "<-" pat where_decls

pattern_synonym_lhs: con vars0
|                    varid conop varid
|                    con "{" cvars1 "}"

vars0: %empty
|      varid vars0

cvars1: varid vars0

where_decls: "where" "{" decls "}"
|            "where" VOCURLY decls close


pattern_synonym_sig: "pattern" con_list "::" sigtypedoc

/* ------------- Nested declarations ----------------------------- */

decl_cls: /*at_decl_cls | */ decl
|         "default" infixexp "::" sigtypedoc

decls_cls: decls_cls ";" decl_cls
|          decls_cls ";"
|          decl_cls
|          %empty

decllist_cls: "{" decls_cls "}"
|             VOCURLY decls_cls close

where_cls: "where" decllist_cls
|          %empty

decl_inst: /* at_decl_inst | */ decl

decls_inst: decls_inst ";" decl_inst
|           decls_inst ";"
|           decl_inst
|           %empty

decllist_inst: "{" decls_inst "}"
|              VOCURLY decls_inst close

where_inst: "where" decllist_inst
|           %empty

decls: decls ";" decl
|      decls ";"
|      decl
|      %empty

decllist: "{" decls "}"
|         VOCURLY decls close

binds: decllist
|     "{" dbinds "}"
|     VOCURLY dbinds close

wherebinds: "where" binds
|           %empty



/* ------------- Transformation Rules ---------------------------- */

/* ------------- Warnings and deprecations ----------------------- */

strings: STRING
| "[" stringlist "]"

stringlist: stringlist "," STRING
|           STRING
|           %empty

/* ------------- Annotations ------------------------------------- */

/* ------------- Foreign import and export declarations ---------- */

/* ------------- Type signatures --------------------------------- */

opt_sig: %empty
| "::" sigtype

opt_tyconsig: %empty
| "::" gtycon

sigtype: ctype

sigtypedoc: ctypedoc

sig_vars: sig_vars "," var
|         var

sigtypes1: sigtype
|          sigtype "," sigtypes1

/* ------------- Types ------------------------------------------- */

strict_mark: strictness
|            unpackedness
|            unpackedness strictness

strictness: "!"
|           "~"

unpackedness: "{-# UNPACK" "#-"
|             "{-# NOUNPACK" "#-"

ctype: "forall" tv_bndrs "." ctype
|      context "=>" ctype
|      ipvar "::" type
|      type

ctypedoc: ctype

/*
ctypedoc:  "forall" tv_bnrds "." ctypedoc
|      context "=>" ctypedoc
|      ipvar "::" type
|      typedoc
*/

context: btype

context_no_ops: btype_no_ops

type: btype
|     btype "->" ctype

typedoc: type
/* typedoc: .... */

btype: tyapps

btype_no_ops: atype_docs
|             btype_no_ops atype_docs

tyapps: tyapp
|       tyapps tyapp

tyapp: atype
|      qtyconop
|      tyvarop
/* Template Haskell
|      SIMPLEQUOTE qconop
|      SIMPLEQUOTE varop
*/

atype_docs: atype /* FIX */

atype: ntgtycon
|      tyvar
|      "*"
|      strict_mark atype
|      "{" fielddecls "}"
|      "(" ")"
|      "(" ctype "," comma_types1 ")"
|      "(#" "#)"
|      "(#" comma_types1 "#)"
|      "(#" bar_types2   "#)"
|      "[" ctype "]"
|      "(" ctype ")"
|      "(" ctype "::" kind ")"
/* Template Haskell */

inst_type: sigtype

deriv_types: typedoc
|            typedoc "," deriv_types

comma_types0: comma_types1
|             %empty

comma_types1: ctype
|             ctype "," comma_types1

bar_types2: ctype "|" ctype
|           ctype "|" bar_types2

tv_bndrs:   tv_bndr tv_bndrs
|           %empty

tv_bndr:    tyvar
|           "(" tyvar "::" kind ")"

fds:        %empty
|           "|" fds1

fds1:       fds1 "," fd
|           fd

fd:         varids0 "->" varids0

varids0:    %empty
|           varids0 tyvar

/* ------------- Kinds ------------------------------------------- */

kind: ctype



/* ------------- Datatype declarations --------------------------- */

constrs: "=" constrs1

constrs1: constrs1 "|" constr
|         constr

constr: forall context_no_ops "=>" constr_stuff
|       forall constr_stuff

forall: "forall" tv_bndrs "."
|       %empty

constr_stuff: btype_no_ops
|             btype_no_ops conop btype_no_ops

fielddecls: %empty
|           fielddecls1

fielddecls1: fielddecl "," fielddecls1
|            fielddecl

fielddecl: sig_vars "::" ctype

maybe_derivings: %empty
|                derivings

derivings:       derivings deriving
|                deriving

deriving: "deriving" deriv_clause_types
|         "deriving" deriv_strategy_no_via deriv_clause_types
|         "deriving" deriv_clause_types deriv_strategy_via

deriv_clause_types: qtycondoc
|                   "(" ")"
|                   "(" deriv_types ")"


/* ------------- Value definitions ------------------------------- */

decl_no_th: sigdecl
| "!" aexp rhs
| infixexp_top opt_sig rhs
| pattern_synonym_decl
/* | docdel */

decl: decl_no_th
/*  | splice_exp */

rhs: "=" exp wherebinds
|    gdrhs wherebinds

gdrhs: gdrhs gdrh
|      gdrh

gdrh: "|" guardquals "=" exp

sigdecl: infixexp_top "::" sigtypedoc
|        var "," sig_vars "::" sigtypedoc
|        infix prec ops
|        pattern_synonym_sig
|        "{-# COMPLETE" con_list opt_tyconsig "#-}"
|        "{-# INLINE" activation qvar "#-}"
|        "{-# SCC" qvar "#-}"
|        "{-# SCC" qvar STRING "#-}"
|        "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}"
|        "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}"
|        "{-# SPECIALISE" "instance" inst_type "#-}"

activation: %empty
|           explicit_activation

explicit_activation: "[" INTEGER "]"
|                    "[" "~" INTEGER "]"

/* ------------- Expressions ------------------------------------- */

exp: infixexp "::" sigtype { $$ = {}; }
|    infixexp              { $$ = {}; }

infixexp: exp10
|         infixexp qop exp10

infixexp_top: exp10_top
|             infixexp_top qop exp10_top

exp10_top: "-" fexp
|          "{-# CORE" STRING "#-}"
|          fexp

exp10: exp10_top
|      scc_annot exp


optSemi: ";"
|        %empty

scc_annot: "{-# SCC" STRING "#-}"
|          "{-# SCC" VARID "#-}"

/* hpc_annot */

fexp: fexp aexp
|     fexp TYPEAPP atype
|     "static" aexp
|     aexp

aexp: qvar "@" aexp
|     "~" aexp
|     "\\" apat apats "->" exp
|     "let" binds "in" exp
|     "\\" "case" altslist
|     "if" exp optSemi "then" exp optSemi "else" exp
|     "if" ifgdpats
|     "case" exp "of" altslist
|     "do" stmtlist
|     "mdo" stmtlist
|     "proc" aexp "->" exp
|     aexp1

aexp1: aexp1 "{" fbinds "}"
|      aexp2

aexp2: qvar
|      qcon
|      literal
|      "(" texp ")"
|      "(" tup_exprs ")"
|      "(#" texp "#)"
|      "(#" tup_exprs "#)"
|      "[" list "]"
|      "_"
/* Skip Template Haskell Extensions */

/* ------------- Tuple expressions ------------------------------- */

texp: exp
|     infixexp qop
|     qopm infixexp
|     exp "->" texp

tup_exprs: texp commas_tup_tail
|          texp bars
|          commas tup_tail
|          bars texp bars0

commas_tup_tail: commas tup_tail

tup_tail: texp commas_tup_tail
|         texp
|         %empty

/* ------------- List expressions -------------------------------- */

list: texp
|     lexps
|     texp ".."
|     texp "," exp ".."
|     texp ".." exp
|     texp "," exp ".." exp
|     texp "|" flattenedpquals

lexps: lexps "," texp
|      texp "," texp


/* ------------- List Comprehensions ----------------------------- */

flattenedpquals: pquals

pquals: squals "|" pquals
|       squals

squals: squals "," transformqual
|       squals "," qual
|       transformqual
|       qual

transformqual: "then" exp
|              "then" exp "by" exp
|              "then" "group" "using" exp
|              "then" "group" "by" exp "using" exp

/* ------------- Guards ------------------------------------------ */
guardquals: guardquals1

guardquals1: guardquals1 "," qual
|            qual

/* ------------- Case alternatives ------------------------------- */
altslist: "{" alts "}"
|         VOCURLY alts close
|         "{" "}"
|         VOCURLY close

alts: alts1
|     ";" alts

alts1: alts1 ";" alt
|      alts1 ";"
|      alt

alt:   pat alt_rhs

alt_rhs: ralt wherebinds

ralt: "->" exp
|     gdpats

gdpats: gdpats gdpat
|       gdpat

ifgdpats : "{" gdpats "}"
|          gdpats close

gdpat: "|" guardquals "->" exp

pat: exp
|   "!" aexp

bindpat: exp
|   "!" aexp

apat: aexp
|    "!" aexp

apats: apat apats
|     %empty

/* ------------- Statement sequences ----------------------------- */
stmtlist: "{" stmts "}"
|         VOCURLY stmts close

stmts: stmts ";" stmt
|      stmts ";"
|      stmt
|      %empty

/*maybe_stmt:   stmt
|             %empty */

stmt: qual
|     "rec" stmtlist

qual: bindpat "<-" exp
|     exp
|     "let" binds


/* ------------- Record Field Update/Construction ---------------- */

fbinds: fbinds1
|       %empty

fbinds1: fbind "," fbinds1
|        fbind
|        ".."

fbind: qvar "=" texp
|      qvar

/* ------------- Implicit Parameter Bindings --------------------- */

dbinds: dbinds ";" dbind
|       dbinds ";"
|       dbind
|       %empty

dbind:  ipvar "=" exp

ipvar: IPDUPVARID

/* ------------- Warnings and deprecations ----------------------- */

/* ------------- Data Constructors ------------------------------- */

/* For Template Haskell
qcon_nowiredlist:  gen_qcon         { $$ = $1; }
|                  sysdcon_no_list  { $$ = $1; }
*/

qcon: gen_qcon { $$ = $1; }
|     sysdcon  { $$ = $1; }

gen_qcon: qconid      { $$ = $1; }
|     "(" qconsym ")" { $$ = $2; }

con: conid          { $$ = $1; }
|    "(" consym ")" { $$ = $2; }
|    sysdcon        { $$ = $1; }

con_list: con
|         con "," con_list

sysdcon_no_list:  "(" ")"   { $$ = {@$, "()"}; }
|                 "(" commas   ")" { $$ = {@$, "("+std::string($2,',')+")"}; }
|                 "(#" "#)" { $$ = {@$, "(##)"}; }
|                 "(#" commas "#)" { $$ = {@$, "(#"+std::string($2,',')+"#)"}; }

sysdcon: sysdcon_no_list { $$ = $1; }
|        "[" "]"         { $$ = {@$, "[]"}; }

conop: consym { $$ = $1; }
|      "`" conid "`" { $$ = $2; }

qconop: qconsym { $$ = $1; }
|      "`" qconid "`" { $$ = $2; }

/* ------------- Type Constructors ------------------------------- */
gtycon:   ntgtycon   { $$ = $1; }
|         "(" ")"   { $$ = {@$, "()"}; }
|         "(#" "#)" { $$ = {@$, "(##)"}; }

ntgtycon: oqtycon          { $$ = $1; }
|        "(" commas   ")" { $$ = {@$, "("+std::string($2,',')+")"}; }
|        "(#" commas "#)" { $$ = {@$, "(#"+std::string($2,',')+"#)"}; }
|        "(" "->" ")"     { $$ = {@$, "->"}; }
|        "[" "]"          { $$ = {@$, "[]"}; }

oqtycon: qtycon            { $$ = $1; }
|        "(" qtyconsym ")" { $$ = $2; }
|        "(" "~" ")"       { $$ = {@$, "~"}; }

oqtycon_no_varcon: qtycon  { $$ = $1; }
|        "(" QCONSYM ")"   { $$ = {@$, $2}; }
|        "(" CONSYM  ")"   { $$ = {@$, $2}; }
|        "(" ":"  ")"      { $$ = {@$, ":"}; }
|        "(" "~"  ")"      { $$ = {@$, "~"}; }


qtyconop: qtyconsym      {$$ = $1; }
|         "`" qtycon "`" { $$ = $2; }

qtycondoc: qtycon {$$ = $1;}

qtycon:  QCONID { $$ = {@$,$1}; }
|        tycon  { $$ = $1; }

/* qtycondoc */

tycon:     CONID    { $$ = {@$,$1}; }

qtyconsym: QCONSYM  { $$ = {@$,$1}; }
|          QVARSYM  { $$ = {@$,$1}; }
|          tyconsym { $$ = $1; }

tyconsym: CONSYM { $$ = {@$, $1}; }
|         VARSYM { $$ = {@$, $1}; }
|         ":"    { $$ = {@$, ":"}; }
|         "-"    { $$ = {@$, "-"}; }


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
| "(" varsym ")" {$$ = $2; }

qvar: qvarid { $$ = $1; }
| "(" varsym ")" {$$ = $2; }
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
       /* Without the yyerrok, the yyerror seems not to be called at the end of the file, 
          so that the drv.pop_error_message() causes a SEGFAULT. */
error { yyerrok; drv.pop_error_message(); drv.pop_context();}

/* ------------- Miscellaneous (mostly renamings) ---------------- */

modid: CONID {$$ = {@$, $1};}
| QCONID {$$ = {@$, $1};}

commas: commas "," {$$ = $1 + 1;}
|       ","        {$$ = 1;}

bars0: bars        {$$ = $1 + 1;}
|     %empty       {$$ = 0;}

bars: bars "|"     {$$ = $1 + 1;}
|     "|"          {$$ = 1;}

%%

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}
