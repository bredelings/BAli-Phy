%skeleton "lalr1.cc" /* -*- C++ -*- */
%require "3.4"

%defines
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
  # include "computation/expression/expression_ref.H"
  # include "computation/expression/var.H"
  # include "computation/operations.H"
  # include "computation/expression/list.H"
  # include "computation/expression/tuple.H"
  # include "computation/haskell/haskell.H"
  # include "computation/typecheck/types.H"
  # include "computation/typecheck/kind.H"

  class driver;

  std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>> make_body(const std::vector<Hs::ImpDecl>& imports, const std::optional<Hs::Decls>& topdecls);

  Hs::Kind type_to_kind(const Hs::Type& kind);
  Hs::Constructor make_constructor(const std::vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::Type& typeish);
  Hs::InstanceDecl make_instance_decl(const Located<Hs::Type>& type, const std::optional<Located<Hs::Binds>>& decls);
  Hs::TypeSynonymDecl make_type_synonym(const Located<Hs::Type>& lhs_type, const Located<Hs::Type>& rhs_type);
  Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context& context,
                                             const Hs::Type& header, const std::vector<Hs::Constructor>& constrs);
  Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::Type& header, const std::optional<Located<Hs::Binds>>& decls);
  Hs::Context make_context(const Hs::Type& context);

  expression_ref make_infixexp(const std::vector<expression_ref>& args);
  expression_ref make_minus(const expression_ref& exp);
  expression_ref make_fexp(const std::vector<expression_ref>& args);

  expression_ref yy_make_string(const std::string&);
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
  CLASS         "class"
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
  BPCALL        "bpcall"
  CAPI          "capi"
  PRIM          "prim"
  JAVASCRIPT    "javascript"
  PROC          "proc"
  REC           "rec"
  GROUP         "group"
  BY            "by"
  USING         "using"
 /*  PATTERN       "pattern" */
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


%type <Hs::Module> module
 /* %type <void> missing_module_keyword */
 /* %type <expression_ref> maybemodwarning */
%type <std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>>> body2
%type <std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>>> body
%type <std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>>> top
%type <std::pair<std::vector<Hs::ImpDecl>, std::optional<Hs::Decls>>> top1

%type <std::optional<std::vector<Hs::Export>>> maybeexports
%type <std::vector<Hs::Export>> exportlist
%type <std::vector<Hs::Export>> exportlist1
%type <Hs::Export> export
%type <std::optional<Hs::ExportSubSpec>> export_subspec
%type <std::vector<Located<std::string>>> qcnames
%type <std::vector<Located<std::string>>> qcnames1
%type <Located<std::string>> qcname

%type <std::vector<Hs::ImpDecl>> importdecls
%type <std::vector<Hs::ImpDecl>> importdecls_semi
%type <Hs::ImpDecl> importdecl
 // %type <bool> maybe_src
 // %type <bool> maybe_safe
 // %type <std::optional<std::string>> maybe_pkg
%type <bool> optqualified
%type <std::optional<std::string>> maybeas
%type <std::optional<Hs::ImpSpec>> maybeimpspec
%type <Hs::ImpSpec> impspec

%type <std::optional<int>> prec
%type <Hs::Fixity> infix
%type <std::vector<std::string>> ops


%type <Hs::Decls> topdecls
%type <Hs::Decls> topdecls_semi
%type <expression_ref> topdecl
%type <expression_ref> cl_decl
%type <expression_ref> ty_decl
%type <expression_ref> inst_decl
 /*
%type <void> overlap_pragma
%type <void> deriv_strategy_no_via
%type <void> deriv_strategy_via
 */
%type <Hs::DataOrNewtype> data_or_newtype
 /* %type <void> opt_kind_sig */
%type <std::pair<Hs::Context,Hs::Type>> tycl_hdr
/* %type <void> capi_ctype 


%type <void> pattern_synonym_decl
%type <void> pattern_synonym_lhs
%type <void> vars0
%type <void> cvars1
%type <void> where_decls
%type <void> pattern_synonym_sig

%type <void> decl_cls
%type <void> decls_cls
%type <void> declslist_cls
%type <void> where_cls

%type <void> decl_inst
%type <void> decls_inst
%type <void> decllist_inst
%type <void> where_inst
*/

%type <std::vector<expression_ref>> decls
%type <Hs::Decls> decllist
%type <Located<Hs::Binds>> binds
%type <std::optional<Located<Hs::Binds>>> wherebinds
 /*

%type <void> strings
%type <void> stringlist
%type <expression_ref> opt_sig
 */
%type <Hs::Type> opt_tyconsig
%type <Hs::Type> sigtype
%type <Hs::Type> sigtypedoc
%type <std::vector<Hs::Var>> sig_vars
%type <std::vector<Hs::Type>> sigtypes1

%type <Hs::StrictLazy> strict_mark
%type <Hs::StrictLazy> strictness
%type <Hs::Type> ctype
%type <Hs::Type> ctypedoc
%type <Hs::Context> context
%type <Hs::Context> context_no_ops
%type <Hs::Type> type
%type <Hs::Type> typedoc
%type <Hs::Type> btype
%type <std::vector<Hs::Type>> btype_no_ops
%type <std::vector<Hs::Type>> tyapps
%type <Hs::Type> tyapp
%type <Hs::Type> atype_docs
%type <Hs::Type> atype
%type <Hs::Type> inst_type
 /*
%type <void> deriv_types
 */
%type <std::vector<Hs::Type>> comma_types0
%type <std::vector<Hs::Type>> comma_types1
 /*
%type <void> bar_types2
 */
%type <std::vector<Hs::TypeVar>> tv_bndrs
%type <Hs::TypeVar> tv_bndr
 /*
%type <void> fds
%type <void> fds1
%type <void> fd
%type <void> varids0
 */
 //%type <Hs::Kind> kind
%type <expression_ref> kind

%type <std::vector<Hs::Constructor>> constrs
%type <std::vector<Hs::Constructor>> constrs1
%type <Hs::Constructor> constr
%type <std::vector<Hs::TypeVar>> forall
%type <Hs::Type> constr_stuff
%type <std::vector<Hs::FieldDecl>> fielddecls
%type <std::vector<Hs::FieldDecl>> fielddecls1
%type <Hs::FieldDecl> fielddecl
 /*
%type <void> maybe_derivings
%type <void> derivings
%type <void> deriv_clause_types
 */

%type <expression_ref> decl_no_th
%type <expression_ref> decl
%type <Hs::MultiGuardedRHS> rhs
%type <std::vector<Hs::GuardedRHS>> gdrhs
%type <Hs::GuardedRHS> gdrh
%type <expression_ref> sigdecl
 /*
%type <void> activation
%type <void> explicit_activation
 */

%type <expression_ref> exp
%type <std::vector<expression_ref>> infixexp
%type <std::vector<expression_ref>> infixexp_top
%type <expression_ref> exp10_top
%type <expression_ref> exp10

%type <std::vector<expression_ref>> fexp
%type <expression_ref> aexp
%type <expression_ref> aexp1
%type <expression_ref> aexp2
%type <expression_ref> texp
%type <std::vector<expression_ref>> tup_exprs
 /*
%type <void> tup_tail
 */
%type <expression_ref> list
%type <std::vector<expression_ref>> lexps

 /* 
%type <std::vector<expression_ref>> flattenedpquals 
%type <std::vector<expression_ref>> pquals
 */
%type <std::vector<expression_ref>> squals
 /* %type <expression_ref> transformqual */

%type <std::vector<expression_ref>> guardquals
%type <std::vector<expression_ref>> guardquals1

%type <Hs::Alts> altslist
%type <std::vector<Located<Hs::Alt>>> alts
%type <std::vector<Located<Hs::Alt>>> alts1
%type <Located<Hs::Alt>> alt
%type <Hs::MultiGuardedRHS> alt_rhs
%type <std::vector<Hs::GuardedRHS>> gdpats
 /* %type <expression_ref> ifgdpats */
%type <Hs::GuardedRHS> gdpat
%type <expression_ref> pat
%type <expression_ref> bindpat
%type <expression_ref> apat
%type <std::vector<expression_ref>> apats1

%type <Hs::Stmts> stmtlist
%type <std::vector<expression_ref>> stmts
%type <expression_ref> stmt

%type <expression_ref> qual
 /*
%type <void> fbinds
%type <void> fbinds1
%type <void> fbind

%type <void> dbinds
%type <void> dbind
%type <std::string> ipvar 
%type <std::string> overloaded_label
*/

 /* %type <std::string> qcon_nowiredlist */
%type <std::string> qcon
%type <std::string> gen_qcon
%type <std::string> con
%type <std::string> sysdcon_no_list
%type <std::string> sysdcon
%type <std::string> conop
%type <std::string> qconop

%type <std::string> gtycon
%type <std::string> ntgtycon
%type <std::string> oqtycon
%type <std::string> oqtycon_no_varcon
%type <std::string> qtyconop
%type <std::string> qtycondoc
%type <std::string> qtycon
%type <std::string> tycon
%type <std::string> qtyconsym
%type <std::string> tyconsym

%type <std::string> op
%type <std::string> varop
%type <expression_ref> qop
%type <expression_ref> qopm
 //%type <std::string> hole_op
%type <std::string> qvarop
%type <std::string> qvaropm

%type <std::string> tyvar
%type <std::string> tyvarop
%type <std::string> tyvarid

%type <std::string> var
%type <std::string> qvar
%type <std::string> qvarid
%type <std::string> varid
%type <std::string> qvarsym
%type <std::string> qvarsym_no_minus
%type <std::string> qvarsym1
%type <std::string> varsym
%type <std::string> varsym_no_minus
%type <std::string> special_id
%type <std::string> special_sym

%type <std::string> qconid
%type <std::string> conid
%type <std::string> qconsym
%type <std::string> consym

%type  <expression_ref> literal
%type  <std::string> modid
%type  <int> commas
/*
%type  <int> bars0
%type  <int> bars
*/

%expect 140

 /* Having vector<> as a type seems to be causing trouble with the printer */
 /* %printer { yyoutput << $$; } <*>; */

%%
%start unit;
unit: module {drv.result = $1;}

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

module: "module" modid maybemodwarning maybeexports "where" body {$$ = Hs::Module{$2,$4,$6.first, $6.second};}
| body2                                                          {$$ = Hs::Module{"Main",{},$1.first, $1.second};}

missing_module_keyword: %empty                                   {drv.push_module_context();}

/* BACKPACK: implicit_top: %empty */

maybemodwarning: "{-# DEPRECATED" strings "#-}"
|                "{-# WARNING" strings "#-}"
|                %empty

body: "{" top "}"       {$$ = $2;}
|     VOCURLY top close {$$ = $2;}

body2: "{" top "}"                         {$$ = $2;}
|     missing_module_keyword top close     {$$ = $2;}


top: semis top1                            {$$ = $2;}

top1: importdecls_semi topdecls_semi       {$$ = make_body($1,$2);}
|     importdecls_semi topdecls            {$$ = make_body($1,$2);}
|     importdecls                          {$$ = make_body($1,{});}

/* ------------- Module declaration and imports only ------------- */

/* Skip backpack stuff */

/* ------------- The Export List --------------------------------- */

maybeexports: "(" exportlist ")"      {$$ = $2;}
|             %empty                  {}

exportlist: exportlist1               {$$ = $1;}

exportlist1: exportlist1 "," export   {$$ = $1; $$.push_back($3);}
|            export                   {$$.push_back($1);}

export: qcname export_subspec         {$$ = Hs::ExportSymbol{$1, $2}; }
|       "module" modid                {$$ = Hs::ExportModule{{@2,$2}}; }

export_subspec: %empty                {}
|              "(" qcnames ")"        { $$ = Hs::ExportSubSpecSome{$2}; }
|              "(" ".." ")"           { $$ = Hs::ExportSubSpecAll(); }

qcnames: %empty    {}
|        qcnames1  {$$ = $1;}

qcnames1 : qcnames1 "," qcname        {$$ = $1; $$.push_back($3);}
|          qcname                     {$$.push_back($1);}

qcname: qvar                          { $$ = {@1,$1}; }
|       oqtycon_no_varcon             { $$ = {@1,$1}; }

/* ------------- Import Declarations ----------------------------- */

semis1: semis1 ";"
|       ";"

semis: semis ";"
|      %empty

importdecls: importdecls_semi importdecl { $$ = $1, $$.push_back($2); }

importdecls_semi: importdecls_semi importdecl semis1 { $$ = $1; $$.push_back($2); }
|                 %empty { }

importdecl: "import" /*maybe_src*/ /*maybe_safe*/ optqualified /*maybe_pkg*/ modid maybeas maybeimpspec {
    $$ = Hs::ImpDecl($2,$3,$4,$5);
}
/*
maybe_src: "{-# SOURCE" "#-}"  { $$ = true; }
|          %empty              { $$ = false; }

maybe_safe: "safe"             { $$ = true; }
|           %empty             { $$ = false; }

maybe_pkg: STRING              { $$ = $1; }
|          %empty              { }
*/
optqualified: "qualified"      { $$ = true; }
|             %empty           { $$ = false; }

maybeas:  "as" modid           { $$ = $2; }
|         %empty               { }

maybeimpspec: impspec          { $$ = $1; }
|             %empty           { }

/* Since we can't have `module name` in an IMPORT list, maybe we should a different type here...*/

impspec: "(" exportlist ")"           { $$ = Hs::ImpSpec{false, $2}; }
|        "hiding" "(" exportlist ")"  { $$ = Hs::ImpSpec{true,  $3}; }


/* ------------- Fixity Declarations ----------------------------- */

prec: %empty       { }
|     INTEGER      { $$ = $1; }

infix: "infix"     { $$ = Hs::Fixity::infix; }
|      "infixl"    { $$ = Hs::Fixity::infixl; }
|      "infixr"    { $$ = Hs::Fixity::infixr; }

ops:   ops "," op  { $$ = $1; $$.push_back($3); }
|      op          { $$ = {$1}; }

/* ------------- Top-Level Declarations -------------------------- */

topdecls: topdecls_semi topdecl  { $$ = $1; $$.push_back($2); }

topdecls_semi: topdecls_semi topdecl semis1 { $$ = $1; $$.push_back($2); }
|              %empty                       { }

topdecl: cl_decl                               {$$ = $1;}
|        ty_decl                               {$$ = $1;}
|        inst_decl                             {$$ = $1;}
/*|        stand_alone_deriving
  |        role_annot*/
|        "default" "(" comma_types0 ")"        {$$ = Hs::DefaultDecl($3); }
|        "foreign" "import" "bpcall" STRING var "::" sigtypedoc  {$$ = Hs::ForeignDecl($4, $5, $7);}
/*
|        "foreign" fdecl
|        "{-# DEPRECATED" deprecations "#-}"
|        "{-# WARNING" warnings "#-}"
|        "{-# RULES" rules "#-}"
|        annotation*/
|        decl_no_th                            {$$ = $1;}
/* What is this for? How is this a decl ? */
|        infixexp_top                          {$$ = make_infixexp($1);}

cl_decl: "class" tycl_hdr /*fds*/ wherebinds   {$$ = make_class_decl($2.first,$2.second,$3);}

ty_decl: "type" type "=" ctypedoc                                          {$$ = make_type_synonym({@2,$2},{@4,$4});}
|        data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings       {$$ = make_data_or_newtype($1,$3.first,$3.second,$4);}
|        data_or_newtype capi_ctype tycl_hdr opt_kind_sig                  {$$ = make_data_or_newtype($1,$3.first,$3.second,{});}
/* |        "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family */
/* |        "data" "family" type opt_datafam_kind_sig */

// inst_type -> sigtype -> ctype --maybe--> context => type
inst_decl: "instance" overlap_pragma inst_type wherebinds                  {$$ = make_instance_decl({@3,$3},$4);}
/* |          "type" "instance" ty_fam_inst_eqn */
/* |          data_or_newtype "instance" capi_ctype tycl_hdr constrs
   |          data_or_newtype "instance" capi_ctype opt_kind_sig */

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

data_or_newtype: "data"    {$$=Hs::DataOrNewtype::data;}
|                "newtype" {$$=Hs::DataOrNewtype::newtype;}

opt_kind_sig: %empty
|             "::" kind

/*opt_datafam_kind_sig: %empty
|                     "::" kind

 opt_tyfam_kind:sigm: %empty */

/* opt_tyfam_at_kind_inj_sig: */

tycl_hdr: context "=>" type  {$$ = {$1,$3};}
|         type               {$$ = {{},$1};}

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
*/

/* ------------- Nested declarations ----------------------------- */

/* Remove specializtion of binds for classes and instances */

decls: decls ";" decl   {$$ = $1; $$.push_back($3);}
|      decls ";"        {$$ = $1;}
|      decl             {$$.push_back($1);}
|      %empty           {}

decllist: "{" decls "}"          {$$ = $2;}  // location here should include { }?
|         VOCURLY decls close    {$$ = $2;}

binds: decllist                  {$$ = {@1,{$1}};}

wherebinds: "where" binds        {$$ = $2;}                   // location here should include "where"?
|           %empty               {}



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

/*
opt_sig: %empty  {}
| "::" sigtype   {$$ = $2;}
*/

opt_tyconsig: %empty {}
| "::" gtycon        {$$ = Hs::TypeCon({@2,$2});}

sigtype: ctype   {$$ = $1;}

sigtypedoc: ctypedoc {$$ = $1;}

sig_vars: sig_vars "," var {$$ = $1; $$.push_back(Hs::Var({@3,$3}));}
|         var              {$$.push_back(Hs::Var({@1,$1}));}

sigtypes1: sigtype               {$$.push_back($1);}
|          sigtypes1 "," sigtype {$$ = $1; $$.push_back($3);}

/* ------------- Types ------------------------------------------- */

strict_mark: strictness                     {$$ = $1;}
/*
|            unpackedness                   {}
|            unpackedness strictness        {$$ = $2;}
*/

strictness: "!" {$$ = Hs::StrictLazy::strict;}
|           "~" {$$ = Hs::StrictLazy::lazy;}

/*
unpackedness: "{-# UNPACK" "#-}"
|             "{-# NOUNPACK" "#-}"
*/

ctype: "forall" tv_bndrs "." ctype {$$ = Hs::ForallType($2, $4);}
|      context "=>" ctype          {$$ = Hs::ConstrainedType($1,$3);}
/* |      ipvar "::" type             {} */
|      type                        {$$ = $1;}

ctypedoc: ctype                    {$$ = $1;}

/*
ctypedoc:  "forall" tv_bnrds "." ctypedoc
|      context "=>" ctypedoc
|      ipvar "::" type
|      typedoc
*/

context: btype                     {$$ = make_context($1);}

context_no_ops: btype_no_ops       {$$ = make_context(Hs::make_tyapps($1));}

type: btype                        {$$ = $1;}
|     btype "->" ctype             {$$ = Hs::make_tyapps({Hs::TypeCon({@2,"->"}),$1,$3});}

typedoc: type                      {$$ = $1;}
/* typedoc: .... */

btype: tyapps                      {$$ = Hs::make_tyapps($1);}

btype_no_ops: atype_docs               {$$.push_back($1);}
|             btype_no_ops atype_docs  {$$ = $1; $$.push_back($2);}

tyapps: tyapp                      {$$.push_back($1);}
|       tyapps tyapp               {$$ = $1; $$.push_back($2);}

tyapp: atype                       {$$ = $1;}
|      qtyconop                    {$$ = Hs::TypeCon({@1,$1});}
|      tyvarop                     {$$ = Hs::TypeVar({@1,$1});}
/* Template Haskell
|      SIMPLEQUOTE qconop
|      SIMPLEQUOTE varop
*/

atype_docs: atype /* FIX */        {$$ = $1;}

/* '*' is either a binary type operator (m * n) or the * kind, unless the StarIsType
 * extension is enabled.
 * Do I need a separate rule for it if I'm not using StarIsType?
 */

atype: ntgtycon                        {$$ = Hs::TypeCon({@1,$1});}
|      tyvar                           {$$ = Hs::TypeVar({@1,$1});}
|      "*"                             {$$ = Hs::TypeCon({@1,"*"});}
|      strict_mark atype               {$$ = Hs::StrictLazyType($1,$2);}
|      "{" fielddecls "}"              {$$ = Hs::FieldDecls($2);}
|      "(" ")"                         {$$ = Hs::TypeCon({@1,"()"});}
|      "(" comma_types1 "," ctype")"   {auto ts = $2;ts.push_back($4);$$ = Hs::TupleType(ts);}
/*
|      "(#" "#)"                       {}
|      "(#" comma_types1 "#)"          {}
|      "(#" bar_types2   "#)"          {}
*/
|      "[" ctype "]"                   {$$ = Hs::ListType{$2}; }
|      "(" ctype ")"                   {$$ = $2;}
/* |      "(" ctype "::" kind ")"         {$$ = Hs::TypeOfKind($2,$4); } */
/* Template Haskell */

inst_type: sigtype                     {$$ = $1;}

deriv_types: typedoc
|            typedoc "," deriv_types

comma_types0: comma_types1             {$$ = $1;}
|             %empty                   { /* default construction OK */ }

comma_types1: ctype                    {$$.push_back($1);}
|             comma_types1 "," ctype   {$$ = $1; $$.push_back($3);}

/*
bar_types2: ctype "|" ctype
|           ctype "|" bar_types2
*/

tv_bndrs:   tv_bndrs tv_bndr   {$$ = $1; $$.push_back($2);}
|           %empty             { /* default construction OK */}

/* If we put the kind into the type var (maybe as an optional) we could unify these two */
tv_bndr:    tyvar                   {$$ = Hs::TypeVar({@1,$1});}
|           "(" tyvar "::" kind ")" {$$ = Hs::TypeVar({@2,$2},$4);}


/* fds are functional dependencies = FunDeps 
fds:        %empty
|           "|" fds1

fds1:       fds1 "," fd
|           fd

fd:         varids0 "->" varids0

varids0:    %empty
|           varids0 tyvar
*/

/* ------------- Kinds ------------------------------------------- */

kind: ctype  {$$ = type_to_kind($1);}



/* ------------- Datatype declarations --------------------------- */

constrs: "=" constrs1           {$$ = $2;}

constrs1: constrs1 "|" constr   {$$ = $1; $$.push_back($3);}
|         constr                {$$.push_back($1);}

constr: forall context_no_ops "=>" constr_stuff {$$ = make_constructor($1,$2, $4);}
|       forall constr_stuff                     {$$ = make_constructor($1,{}, $2);}

forall: "forall" tv_bndrs "."   {$$ = $2;}
|       %empty                  {}

constr_stuff: btype_no_ops                      {$$ = Hs::make_tyapps($1);}
|             btype_no_ops conop btype_no_ops   {$$ = Hs::make_tyapps({Hs::TypeCon({@2,$2}),Hs::make_tyapps($1),Hs::make_tyapps($3)});}

fielddecls: %empty              {}
|           fielddecls1         {$$ = $1;}

fielddecls1: fielddecls1 "," fielddecl  {$$ = $1; $$.push_back($3);}
|            fielddecl                  {$$.push_back($1);}

fielddecl: sig_vars "::" ctype          {$$ = Hs::FieldDecl($1,$3);}

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

decl_no_th: sigdecl           {$$ = $1;}
/* I guess this is a strict let. Code as DeclStrict, rather than StrictPattern, since docs say this is part of the binding, not part of the patter */
| "!" aexp rhs                {$$ = Hs::StrictValueDecl{{@2,$2},$3}; }
| infixexp_top rhs            {$$ = Hs::ValueDecl({@1,make_infixexp($1)},$2);}

decl: decl_no_th              {$$ = $1;}
/*  | splice_exp */

// rhs is like altrhs but with = instead of ->
rhs: "=" exp wherebinds       {$$ = Hs::SimpleRHS({@2,$2},$3);}
|    gdrhs wherebinds         {$$ = Hs::MultiGuardedRHS{$1,$2};}

gdrhs: gdrhs gdrh             {$$ = $1; $$.push_back($2);}
|      gdrh                   {$$.push_back($1);}


// gdrh is like gdpat, but with = instead of ->
gdrh: "|" guardquals "=" exp  {$$ = Hs::GuardedRHS{$2,$4};}

/* sigdecl : infixexp_top "::" sigtypedoc        { }  | ...

   Previously the var :: type case was caught here, and
   the line below had var , sig_vars instead of just sig_vars..

   GHC did this to allow expressions like f :: Int -> Int = ...
   See note [Joint value/type declarations]. */

sigdecl: sig_vars "::" sigtypedoc { $$ = Hs::SignatureDecl{$1,$3}; }
|        infix prec ops  { $$ = Hs::FixityDecl{$1,$2,$3}; }
/* |        pattern_synonym_sig {}  */
|        "{-# COMPLETE" con_list opt_tyconsig "#-}" {}
|        "{-# INLINE" activation qvar "#-}" {}
|        "{-# SCC" qvar "#-}" {}
|        "{-# SCC" qvar STRING "#-}" {}
|        "{-# SPECIALISE" activation qvar "::" sigtypes1 "#-}" {}
|        "{-# SPECIALISE_INLINE" activation qvar "::" sigtypes1 "#-}" {}
|        "{-# SPECIALISE" "instance" inst_type "#-}" {}


activation: %empty
|           explicit_activation

explicit_activation: "[" INTEGER "]"
|                    "[" "~" INTEGER "]"

/* ------------- Expressions ------------------------------------- */

exp: infixexp "::" sigtype { $$ = Hs::TypedExp(make_infixexp($1),$3); }
|    infixexp              { $$ = make_infixexp($1); }

infixexp: exp10                 {$$.push_back($1);}
|         infixexp qop exp10    {$$ = $1; $$.push_back($2); $$.push_back($3);}

infixexp_top: exp10_top         {$$.push_back($1);}
|             infixexp_top qop exp10_top  {$$ = $1; $$.push_back($2); $$.push_back($3);}

exp10_top: "-" fexp                {$$ = make_minus(make_fexp($2));}
|          "{-# CORE" STRING "#-}" {}
|          fexp                    {$$ = make_fexp($1);}

exp10: exp10_top                 {$$ = $1;}
|      scc_annot exp             {}


optSemi: ";"
|        %empty

scc_annot: "{-# SCC" STRING "#-}"
|          "{-# SCC" VARID "#-}"

/* hpc_annot */

fexp: fexp aexp                  {$$ = $1; $$.push_back($2);}
|     fexp TYPEAPP atype         {}
|     "static" aexp              {}
|     aexp                       {$$.push_back($1);}

aexp: qvar "@" aexp              {$$ = Hs::AsPattern(Hs::Var({@1,$1}),$3);}
|     "~" aexp                   {$$ = Hs::LazyPattern($2);}
|     "\\" apats1 "->" exp       {$$ = Hs::LambdaExp($2,$4);}
|     "let" binds "in" exp       {$$ = Hs::LetExp($2,{@4,$4});}
/* |     "\\" "case" altslist       {} LambdaCase extension not currently handled */
|     "if" exp optSemi "then" exp optSemi "else" exp   {$$ = Hs::IfExp({@2,$2},{@5,$5},{@8,$8});}
/* |     "if" ifgdpats              {} MultiWayIf extension not currently handled */
|     "case" exp "of" altslist   {$$ = Hs::CaseExp($2,$4);}
|     "do" stmtlist              {$$ = Hs::Do($2);}
|     "mdo" stmtlist             {$$ = Hs::MDo($2);}
/* |     "proc" aexp "->" exp       {} -XArrows not currently handled */
|     aexp1                      {$$ = $1;}

aexp1: aexp1 "{" fbinds "}"   {}
|      aexp2                  {$$ = $1;}

aexp2: qvar                   {$$ = Hs::Var({@1,$1});}
|      qcon                   {$$ = Hs::Con({@1,$1});}
|      literal                {$$ = $1;}
|      "(" texp ")"           {$$ = $2;}
|      "(" tup_exprs ")"      {$$ = Hs::Tuple($2);}
/* 
|      "(#" texp "#)"         {}
|      "(#" tup_exprs "#)"    {}
*/
|      "[" list "]"           {$$ = $2;}
|      "_"                    {$$ = Hs::WildcardPattern();}
/* Skip Template Haskell Extensions */

/* ------------- Tuple expressions ------------------------------- */

texp: exp             {$$ = $1;}
|     infixexp qop    {$$ = Hs::LeftSection ( make_infixexp($1), $2 ); }
|     qopm infixexp   {$$ = Hs::RightSection( $1, make_infixexp($2) ); }
/* view patterns 
|     exp "->" texp
*/

tup_exprs: tup_exprs "," texp    {$$ = $1; $$.push_back($3);}
|          texp "," texp         {$$.push_back($1); $$.push_back($3);}

/*
See unboxed sums for where the bars are coming from.

tup_exprs: texp commas_tup_tail    {
|          texp bars
|          commas tup_tail
|          bars texp bars0

commas_tup_tail: commas tup_tail

tup_tail: texp commas_tup_tail
|         texp
|         %empty
*/
/* ------------- List expressions -------------------------------- */

list: texp                       { $$ = Hs::List{{$1}}; }
|     lexps                      { $$ = Hs::List{$1}; }
|     texp ".."                  { $$ = Hs::ListFrom($1); }
|     texp "," exp ".."          { $$ = Hs::ListFromThen($1,$3); }
|     texp ".." exp              { $$ = Hs::ListFromTo($1,$3); }
|     texp "," exp ".." exp      { $$ = Hs::ListFromThenTo($1, $3, $5); }
|     texp "|" squals            { $$ = Hs::ListComprehension($1, $3); }

lexps: lexps "," texp            { $$ = $1; $$.push_back($3);}
|      texp "," texp             { $$.push_back($1); $$.push_back($3);}


/* ------------- List Comprehensions ----------------------------- */

/*
flattenedpquals: pquals                   {$$ = $1;}

pquals: squals "|" pquals                 {$$.push_back(make_squals($1));$$.insert($$.end(),$3.begin(),$3.end());}
|       squals                            {$$.push_back(make_squals($1));}
*/

squals: /* squals "," transformqual          {$$ = $1; $$.push_back($3);} */
        squals "," qual                   {$$ = $1; $$.push_back($3);}
/* |       transformqual                     {$$.push_back($1);} */
|       qual                              {$$.push_back($1);}

/*
transformqual: "then" exp                           {}
|              "then" exp "by" exp                  {}
|              "then" "group" "using" exp           {}
|              "then" "group" "by" exp "using" exp  {}
*/

/* ------------- Guards ------------------------------------------ */
guardquals: guardquals1            {$$ = $1;}

guardquals1: guardquals1 "," qual  {$$ = $1;$$.push_back($3);}
|            qual                  {$$.push_back($1);}

/* ------------- Case alternatives ------------------------------- */
altslist: "{" alts "}"           {$$ = Hs::Alts{$2};}
|         VOCURLY alts close     {$$ = Hs::Alts{$2};}
|         "{" "}"                {}
|         VOCURLY close          {}

alts: alts1                      {$$ = $1;}
|     ";" alts                   {$$ = $2;}

alts1: alts1 ";" alt             {$$ = $1; $$.push_back($3);}
|      alts1 ";"                 {$$ = $1;}
|      alt                       {$$.push_back($1);}

alt:   pat alt_rhs               {$$ = Located<Hs::Alt>{@1+@2,{$1,$2}};}

alt_rhs: "->" exp wherebinds     {$$ = Hs::SimpleRHS({@2,$2},$3);}
|        gdpats   wherebinds     {$$ = Hs::MultiGuardedRHS($1,$2);}

gdpats: gdpats gdpat             {$$ = $1; $$.push_back($2);}
|       gdpat                    {$$.push_back($1);}

/*
Used in MultiWayIf extension:

ifgdpats : "{" gdpats "}"        {}
|          gdpats close          {}
*/

gdpat: "|" guardquals "->" exp   {$$=Hs::GuardedRHS{$2,$4};}

pat: exp      {$$ = $1;}
|   "!" aexp  {$$ = Hs::StrictPattern($2);}

bindpat: exp  {$$ = $1;}
|   "!" aexp  {$$ = Hs::StrictPattern($2);}

apat: aexp    {$$ = $1;}
|    "!" aexp {$$ = Hs::StrictPattern($2);}

apats1: apats1 apat {$$ = $1; $$.push_back($2);}
|       apat        {$$.push_back($1);}

/* ------------- Statement sequences ----------------------------- */
stmtlist: "{" stmts "}"        {$$ = Hs::Stmts{$2};}
|         VOCURLY stmts close  {$$ = Hs::Stmts{$2};}

stmts: stmts ";" stmt  {$$ = $1; $$.push_back($3);}
|      stmts ";"       {$$ = $1;}
|      stmt            {$$.push_back($1);}
|      %empty          {}

/*maybe_stmt:   stmt
|             %empty */

stmt: qual              {$$ = $1;}
|     "rec" stmtlist    {$$ = Hs::RecStmt($2);}

qual: bindpat "<-" exp  {$$ = Hs::PatQual($1,$3);}
|     exp               {$$ = Hs::SimpleQual($1);}
|     "let" binds       {$$ = Hs::LetQual($2);}


/* ------------- Record Field Update/Construction ---------------- */

fbinds: fbinds1
|       %empty

fbinds1: fbind "," fbinds1
|        fbind
|        ".."

fbind: qvar "=" texp
|      qvar

/* ------------- Implicit Parameter Bindings --------------------- */
/* GHC Extension: implicit param ?x */
/* This won't happen because the lexer doesn't recognize these right now 

dbinds: dbinds ";" dbind
|       dbinds ";"
|       dbind
|       %empty

dbind:  ipvar "=" exp

ipvar: IPDUPVARID { $$ = $1; }

*/
/* ------------- Implicit Parameter Bindings --------------------- */

/* GHC Extension: overloaded labels #x */
/* This won't happen because the lexer doesn't recognize these right now 
overloaded_label: LABELVARID { $$ = $1; }
*/

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

sysdcon_no_list:  "(" ")"   { $$ =  "()"; }
|                 "(" commas   ")" { $$ = "("+std::string($2,',')+")"; }
|                 "(#" "#)" { $$ = "(##)"; }
|                 "(#" commas "#)" { $$ = "(#"+std::string($2,',')+"#)"; }

sysdcon: sysdcon_no_list { $$ = $1; }
|        "[" "]"         { $$ = "[]"; }

conop: consym { $$ = $1; }
|      "`" conid "`" { $$ = $2; }

qconop: qconsym { $$ = $1; }
|      "`" qconid "`" { $$ = $2; }

/* ------------- Type Constructors ------------------------------- */
gtycon:   ntgtycon   { $$ = $1; }
|         "(" ")"   { $$ = "()"; }
|         "(#" "#)" { $$ = "(##)"; }

ntgtycon: oqtycon          { $$ = $1; }
|        "(" commas   ")" { $$ = "("+std::string($2,',')+")"; }
|        "(#" commas "#)" { $$ = "(#"+std::string($2,',')+"#)"; }
|        "(" "->" ")"     { $$ = "->"; }
|        "[" "]"          { $$ = "[]"; }

oqtycon: qtycon            { $$ = $1; }
|        "(" qtyconsym ")" { $$ = $2; }
|        "(" "~" ")"       { $$ = "~"; }

oqtycon_no_varcon: qtycon  { $$ = $1; }
|        "(" QCONSYM ")"   { $$ = $2; }
|        "(" CONSYM  ")"   { $$ = $2; }
|        "(" ":"  ")"      { $$ = ":"; }
|        "(" "~"  ")"      { $$ = "~"; }


qtyconop: qtyconsym      {$$ = $1; }
|         "`" qtycon "`" { $$ = $2; }

qtycondoc: qtycon {$$ = $1;}

qtycon:  QCONID { $$ = $1; }
|        tycon  { $$ = $1; }

/* qtycondoc */

tycon:     CONID    { $$ = $1; }

qtyconsym: QCONSYM  { $$ = $1; }
|          QVARSYM  { $$ = $1; }
|          tyconsym { $$ = $1; }

tyconsym: CONSYM { $$ = $1; }
|         VARSYM { $$ = $1; }
|         ":"    { $$ = ":"; }
|         "-"    { $$ = "-"; }


/* ------------- Operators --------------------------------------- */

op : varop { $$ = $1; }
|    conop { $$ = $1; }

varop: varsym   { $$ = $1; }
| "`" varid "`" { $$ = $2; }

qop:  qvarop    { $$ = Hs::Var({@1,$1}); }
|     qconop    { $$ = Hs::Con({@1,$1}); }
/* |     hole_op   { $$ = $1; } */

qopm: qvaropm   { $$ = Hs::Var({@1,$1}); }
|     qconop    { $$ = Hs::Con({@1,$1}); }
/* |     hole_op   { $$ = $1; } */

/* hole_op: "`" "_" "`"  { $$ = "_"; } */

qvarop: qvarsym  { $$ = $1; }
|       "`" qvarid "`" { $$ = $2; }

qvaropm: qvarsym_no_minus  { $$ =$1; }
| "`" qvarid "`" { $$ = $2; }

/* ------------- Type Variables ---------------------------------- */

tyvar: tyvarid            { $$ = $1; }

tyvarop:  "`" tyvarid "`" { $$ = $2; }

tyvarid: VARID            { $$ = $1; }
| special_id              { $$ = $1; }
| "unsafe"                { $$ = "unsafe"; }
| "safe"                  { $$ = "safe"; }
| "interruptible"         { $$ = "interruptible"; }

/* ------------- Variables --------------------------------------- */
var: varid { $$ = $1; }
| "(" varsym ")" {$$ = $2; }

qvar: qvarid { $$ = $1; }
| "(" varsym ")" {$$ = $2; }
| "(" qvarsym1 ")" {$$ = $2; }

qvarid: varid { $$ = $1; }
| QVARID { $$ = $1; }

varid: VARID        { $$ = $1; }
| special_id        { $$ = $1; }
| "unsafe"          { $$ = "unsafe"; }
| "safe"            { $$ = "safe"; }
| "interruptible"   { $$ = "interruptible"; }
| "forall"          { $$ = "forall"; }
| "family"          { $$ = "family"; }
| "role"            { $$ = "role"; }

qvarsym: varsym     { $$ = $1; }
| qvarsym1          { $$ = $1; }

qvarsym_no_minus: varsym_no_minus {$$ = $1;}
|                 qvarsym1 {$$ = $1;}

qvarsym1: QVARSYM        { $$ = $1; }

varsym: varsym_no_minus  { $$ = $1; }
|        "-"             { $$ = "-"; }

varsym_no_minus: VARSYM      {$$ = $1; }
|                special_sym {$$ = $1; }

special_id:  "as"         { $$ = "as"; }
|            "qualified"  { $$ = "qualified"; }
|            "hiding"     { $$ = "hiding"; }
|            "export"     { $$ = "export"; }
|            "label"      { $$ = "label"; }
|            "dynamic"    { $$ = "dynamic"; }
|            "stdcall"    { $$ = "stdcall"; }
|            "ccall"      { $$ = "ccall"; }
|            "capi"       { $$ = "capi"; }
|            "prim"       { $$ = "prim"; }
|            "javascript" { $$ = "javascript"; }
|            "group"      { $$ = "group"; }
|            "stock"      { $$ = "stock"; }
|            "anyclass"   { $$ = "anyclass"; }
|            "via"        { $$ = "via"; }
|            "unit"       { $$ = "unit"; }
|            "dependency" { $$ = "dependency"; }
|            "signature"  { $$ = "signature"; }

special_sym: "!" { $$ = "!"; }
|            "." { $$ = "."; }
|            "*" { $$ = "*"; }

/* ------------- Data constructors ------------------------------- */

qconid:  conid   { $$ = $1; }
|        QCONID  { $$ = $1; }

conid:   CONID   { $$ = $1; }

qconsym: consym  { $$ = $1; }
|        QCONSYM { $$ = $1; }

consym:  CONSYM  { $$ = $1; }
|        ":"     { $$ = ":"; }

/* ------------- Literal ----------------------------------------- */

literal: CHAR        {$$ = Hs::Literal(Hs::Char{$1});}
|        STRING      {$$ = Hs::Literal(Hs::String{$1});}
|        INTEGER     {$$ = Hs::Literal(Hs::Integer{$1});}
|        RATIONAL    {$$ = Hs::Literal(Hs::Double{$1});}
|        PRIMINTEGER {$$ = Hs::Literal(Hs::BoxedInteger{$1});}


/* ------------- Layout ------------------------------------------ */

close: VCCURLY |
       /* Without the yyerrok, the yyerror seems not to be called at the end of the file, 
          so that the drv.pop_error_message() causes a SEGFAULT. */
error { yyerrok; drv.pop_error_message(); drv.pop_context();}

/* ------------- Miscellaneous (mostly renamings) ---------------- */

modid: CONID {$$ = $1;}
| QCONID {$$ = $1;}

commas: commas "," {$$ = $1 + 1;}
|       ","        {$$ = 1;}

/*
bars0: bars        {$$ = $1 + 1;}
|     %empty       {$$ = 0;}

bars: bars "|"     {$$ = $1 + 1;}
|     "|"          {$$ = 1;}
*/
%%

using std::optional;
using std::string;
using std::vector;
using std::pair;

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

pair<vector<Hs::ImpDecl>, optional<Hs::Decls>> make_body(const std::vector<Hs::ImpDecl>& imports, const std::optional<Hs::Decls>& topdecls)
{
    if (topdecls)
    {
        auto topdecls2 = Hs::Decls(*topdecls);
        return {imports, topdecls2};
    }
    else
        return {imports, {}};
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<Hs::Type>>
check_type_or_class_header(const Hs::Type& type)
{
    auto [type_head, type_args] = Hs::decompose_type_apps(type);

    // FIXME -- add location!
    if (not type_head.is_a<Hs::TypeCon>())
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    auto name = unloc(type_head.as_<Hs::TypeCon>().name);

    return {name, type_args};
}

vector<Hs::TypeVar> check_all_type_vars(const vector<Hs::Type>& types)
{
    vector<Hs::TypeVar> type_vars;
    for(auto& type: types)
    {
        if (type.is_a<Hs::TypeVar>())
        {
            type_vars.push_back(type.as_<Hs::TypeVar>());
        }
        else
        {
            throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
    }
    return type_vars;
}

Hs::TypeSynonymDecl make_type_synonym(const Located<Hs::Type>& lhs_type, const Located<Hs::Type>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(unloc(lhs_type));
    return {name, check_all_type_vars(type_args), rhs_type};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::Type& header, const vector<Hs::Constructor>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<name<<"' may only have 1 constructors with 1 field";
    return {d_or_n, name, check_all_type_vars(type_args), context, constrs};
}

Hs::InstanceDecl make_instance_decl(const Located<Hs::Type>& ltype, const optional<Located<Hs::Binds>>& binds)
{
    // GHC stores the instance as a polytype?
    // This would seem to allow (instance forall a.Eq a => forall a.Eq [a] x y ....)

    auto type = unloc(ltype);
    if (type.is_a<Hs::ForallType>())
        throw myexception()<<"instance declaration '"<<type<<"' is malformed";
    Hs::Context context;
    if (type.is_a<Hs::ConstrainedType>())
    {
        auto& T = type.as_<Hs::ConstrainedType>();
        context = T.context;
        type = T.type;
    }

    return {context, type, binds};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::Type& header, const optional<Located<Hs::Binds>>& binds)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name, check_all_type_vars(type_args), context, binds};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Hs::Context make_context(const Hs::Type& context)
{
    vector<Hs::Type> constraints;
    if (context.is_a<Hs::TupleType>())
    {
        constraints = context.as_<Hs::TupleType>().element_types;
    }
    else
        constraints.push_back(context);

    return {constraints};
}

std::optional<Hs::Kind> type_to_kind_(const Hs::Type& kind)
{
    auto [kind_head, kind_args] = Hs::decompose_type_apps(kind);

    if (not kind_head.is_a<Hs::TypeCon>()) return {};
    auto V = kind_head.as_<Hs::TypeCon>();
    auto head_name = unloc(V.name);

    if (kind_args.empty())
    {
        if (head_name == "*")
            return kind_star();
        else
            return {};
    }
    else if (kind_args.size() == 2)
    {
        auto k1 = type_to_kind_(kind_args[0]);
        auto k2 = type_to_kind_(kind_args[1]);
        if (k1 and k2 and head_name == "->")
            return kind_arrow(*k1, *k2);
        else
            return {};
    }
    else
        return {};
}

Hs::Kind type_to_kind(const Hs::Type& kind)
{
    auto maybe_kind = type_to_kind_(kind);

    if (not maybe_kind)
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return *maybe_kind;
}

optional<pair<string, Hs::FieldDecls>> is_record_con(const Hs::Type& typeish)
{
    auto [head,args] = Hs::decompose_type_apps(typeish);

    if (args.size() != 1) return {};

    if (not head.is_a<Hs::TypeCon>()) return {};

    if (not args[0].is_a<Hs::FieldDecls>()) return {};

    return {{unloc(head.as_<Hs::TypeCon>().name), args[0].as_<Hs::FieldDecls>()}};
}

optional<pair<string, std::vector<Hs::Type>>> is_normal_con(const Hs::Type& typeish)
{
    if (is_record_con(typeish)) return {};

    auto [head,args] = Hs::decompose_type_apps(typeish);

    if (not head.is_a<Hs::TypeCon>())
        return {};

    return {{unloc(head.as_<Hs::TypeCon>().name), args}};
}

Hs::Constructor make_constructor(const vector<Hs::TypeVar>& forall, const std::optional<Hs::Context>& c, const Hs::Type& typeish)
{
    if (auto constr = is_record_con(typeish))
    {
        auto [name, fields] = *constr;
        return {forall, c, name, fields};
    }
    else if (auto constr = is_normal_con(typeish))
    {
        auto [name, fields] = *constr;
        return {forall, c, name, fields};
    }
    else
        throw myexception()<<"constructor '"<<typeish<<"' does not make sense";
}

expression_ref make_infixexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else
	return Hs::InfixExp(args);
}


expression_ref make_minus(const expression_ref& exp)
{
    return Hs::InfixExp({Hs::Neg(),exp});
}

expression_ref make_fexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else {
	expression_ref f = args[0];
	for(int i=1;i<args.size();i++)
	    f = {f,args[i]};
	return f;
    }
}
