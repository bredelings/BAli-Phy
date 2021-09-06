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
  # include "computation/expression/AST_node.H"
  # include "computation/operations.H"
  # include "computation/expression/list.H"
  # include "computation/expression/tuple.H"
  # include "computation/parser/haskell.H"

  class driver;

  expression_ref make_module(const std::string& name, const expression_ref& exports, const expression_ref& body);
  expression_ref make_body(const std::vector<expression_ref>& imports, const std::vector<expression_ref>& topdecls);

  expression_ref make_exports(const std::vector<expression_ref>& exports);
  expression_ref make_infix(const std::string& infix, std::optional<int>& prec, std::vector<std::string>& ops);
  expression_ref make_builtin_expr(const std::string& name, int args, const std::string& s1, const std::string& s2);
  expression_ref make_builtin_expr(const std::string& name, int args, const std::string& s);

  expression_ref make_sig_vars(const std::vector<std::string>& sig_vars);
  Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& type, const Located<expression_ref>& decls);
  Haskell::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type);
  Haskell::DataOrNewtypeDecl make_data_or_newtype(const Haskell::DataOrNewtype& d_or_n, const Haskell::Context& context,
                                                  const expression_ref& header, const std::vector<expression_ref>& constrs);
  Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const Located<expression_ref>& decls);
  Haskell::Context make_context(const expression_ref& context);
  expression_ref make_tv_bndrs(const std::vector<expression_ref>& tv_bndrs);
  expression_ref make_tyapps(const std::vector<expression_ref>& tyapps);
  Located<Haskell::ID> make_id(const yy::location& loc, const std::string& id);
  expression_ref make_type_id(const std::string& id);
  expression_ref make_type_var(const std::string& id);
  expression_ref make_type_con(const std::string& id);

  expression_ref make_rhs(const expression_ref& exp, const expression_ref& wherebinds);
  expression_ref make_gdrhs(const std::vector<expression_ref>& gdrhs, const expression_ref& wherebinds);
  expression_ref make_gdrh(const std::vector<expression_ref>& gdpats, const expression_ref& wherebinds);

  expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type);
  expression_ref make_infixexp(const std::vector<expression_ref>& args);
  expression_ref make_minus(const expression_ref& exp);
  expression_ref make_fexp(const std::vector<expression_ref>& args);

  expression_ref make_as_pattern(const Located<Haskell::ID>& x, const expression_ref& body);
  expression_ref make_lazy_pattern(const expression_ref& pat);
  expression_ref make_strict_pattern(const expression_ref& pat);

  expression_ref make_lambda(const std::vector<expression_ref>& pats, const expression_ref& body);
  expression_ref make_let(const expression_ref& binds, const expression_ref& body);
  expression_ref make_if(const expression_ref& cond, const expression_ref& alt_true, const expression_ref& alt_false);
  expression_ref make_case(const expression_ref& obj, const expression_ref& alts);
  expression_ref make_do(const std::vector<expression_ref>& stmts);
  expression_ref make_mdo(const std::vector<expression_ref>& stmts);
  expression_ref yy_make_tuple(const std::vector<expression_ref>& tup_exprs);

  expression_ref make_list(const std::vector<expression_ref>& items);
  expression_ref make_alts(const std::vector<expression_ref>& alts);
  expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs);

  expression_ref make_stmts(const std::vector<expression_ref>& stmts);

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


%type <expression_ref> module
 /* %type <void> missing_module_keyword */
 /* %type <expression_ref> maybemodwarning */
%type <expression_ref> body2
%type <expression_ref> body
%type <expression_ref> top
%type <expression_ref> top1

%type <expression_ref> maybeexports
%type <std::vector<expression_ref>> exportlist
%type <std::vector<expression_ref>> exportlist1
%type <expression_ref> export
 /*%type <expression_ref> export_subspec */
%type <std::vector<expression_ref>> qcnames
%type <std::vector<expression_ref>> qcnames1
%type <expression_ref> qcname_ext_w_wildcard
%type <expression_ref> qcname_ext
%type <expression_ref> qcname

%type <std::vector<expression_ref>> importdecls
%type <std::vector<expression_ref>> importdecls_semi
%type <expression_ref> importdecl
%type <bool> maybe_src
%type <bool> maybe_safe
%type <std::optional<std::string>> maybe_pkg
%type <bool> optqualified
%type <std::optional<std::string>> maybeas
%type <expression_ref> maybeimpspec
%type <expression_ref> impspec

%type <std::optional<int>> prec
%type <std::string> infix
%type <std::vector<std::string>> ops


%type <std::vector<expression_ref>> topdecls
%type <std::vector<expression_ref>> topdecls_semi
%type <expression_ref> topdecl
%type <expression_ref> cl_decl
%type <expression_ref> ty_decl
%type <expression_ref> inst_decl
 /*
%type <void> overlap_pragma
%type <void> deriv_strategy_no_via
%type <void> deriv_strategy_via
 */
%type <Haskell::DataOrNewtype> data_or_newtype
 /* %type <void> opt_kind_sig */
%type <std::pair<Haskell::Context,expression_ref>> tycl_hdr
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
%type <expression_ref> decllist
%type <expression_ref> binds
%type <expression_ref> wherebinds
 /*

%type <void> strings
%type <void> stringlist
 */
%type <expression_ref> opt_sig
%type <expression_ref> opt_tyconsig
%type <expression_ref> sigtype
%type <expression_ref> sigtypedoc
%type <std::vector<std::string>> sig_vars
%type <std::vector<expression_ref>> sigtypes1

%type <std::string> strict_mark
%type <std::string> strictness
 /* %type <void> unpackedness */
%type <expression_ref> ctype
%type <expression_ref> ctypedoc
%type <Haskell::Context> context
%type <Haskell::Context> context_no_ops
%type <expression_ref> type
%type <expression_ref> typedoc
%type <expression_ref> btype
%type <std::vector<expression_ref>> btype_no_ops
%type <std::vector<expression_ref>> tyapps
%type <expression_ref> tyapp
%type <expression_ref> atype_docs
%type <expression_ref> atype
%type <expression_ref> inst_type
 /*
%type <void> deriv_types
 */
%type <std::vector<expression_ref>> comma_types0
%type <std::vector<expression_ref>> comma_types1
 /*
%type <void> bar_types2
 */
%type <std::vector<expression_ref>> tv_bndrs
%type <expression_ref> tv_bndr
 /*
%type <void> fds
%type <void> fds1
%type <void> fd
%type <void> varids0
 */
%type <expression_ref> kind

%type <std::vector<expression_ref>> constrs
%type <std::vector<expression_ref>> constrs1
%type <expression_ref> constr
%type <expression_ref> forall
%type <expression_ref> constr_stuff
%type <std::vector<expression_ref>> fielddecls
%type <std::vector<expression_ref>> fielddecls1
%type <expression_ref> fielddecl
 /*
%type <void> maybe_derivings
%type <void> derivings
%type <void> deriv_clause_types
 */

%type <expression_ref> decl_no_th
%type <expression_ref> decl
%type <expression_ref> rhs
%type <std::vector<expression_ref>> gdrhs
%type <expression_ref> gdrh
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
%type <expression_ref> transformqual

%type <std::vector<expression_ref>> guardquals
%type <std::vector<expression_ref>> guardquals1

%type <std::vector<expression_ref>> altslist
%type <std::vector<expression_ref>> alts
%type <std::vector<expression_ref>> alts1
%type <expression_ref> alt
%type <expression_ref> alt_rhs
%type <std::vector<expression_ref>> gdpats
%type <expression_ref> ifgdpats
%type <expression_ref> gdpat
%type <expression_ref> pat
%type <expression_ref> bindpat
%type <expression_ref> apat
%type <std::vector<expression_ref>> apats1

%type <std::vector<expression_ref>> stmtlist
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
%type <std::string> qop
%type <std::string> qopm
%type <std::string> hole_op
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

%expect 143

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

module: "module" modid maybemodwarning maybeexports "where" body {$$ = make_module($2,$4,$6);}
| body2                                                          {$$ = make_module("Main",{},$1);}

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

maybeexports: "(" exportlist ")"      {$$ = make_exports($2);}
|             %empty                  {}

exportlist: exportlist1               {$$ = $1;}

exportlist1: exportlist1 "," export   {$$ = $1; $$.push_back($3);}
|            export                   {$$.push_back($1);}

export: qcname_ext export_subspec     {$$ = $1;} /* This ignores subspec */
|       "module" modid                {$$ = AST_node("module",$2);}
|       "pattern" qcon                {}

export_subspec: %empty
|              "(" qcnames ")"

qcnames: %empty    {}
|        qcnames1  {$$ = $1;}

qcnames1 : qcnames1 "," qcname_ext_w_wildcard "," {$$ = $1; $$.push_back($3);}
|          qcname_ext_w_wildcard              {$$.push_back($1);}

qcname_ext_w_wildcard: qcname_ext    {$$ = $1;}
| ".."                               {}

qcname_ext: qcname                   {$$ = $1;}
|           "type" oqtycon           {}

qcname: qvar                         {$$ = AST_node("qvar",$1); }
|       oqtycon_no_varcon            {$$ = AST_node("qvar",$1); }

/* ------------- Import Declarations ----------------------------- */

semis1: semis1 ";"
|       ";"

semis: semis ";"
|      %empty

importdecls: importdecls_semi importdecl { $$ = $1, $$.push_back($2); }

importdecls_semi: importdecls_semi importdecl semis1 { $$ = $1; $$.push_back($2); }
|                 %empty { }

importdecl: "import" maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec {
    std::vector<expression_ref> e;
    if ($4) e.push_back(AST_node("qualified"));
    e.push_back(String($6));
    if ($7) e.push_back(AST_node("as", *$7));
    if ($8) e.push_back($8);
    $$ = expression_ref(new expression(AST_node("ImpDecl"),std::move(e)));
}

maybe_src: "{-# SOURCE" "#-}"  { $$ = true; }
|          %empty              { $$ = false; }

maybe_safe: "safe"             { $$ = true; }
|           %empty             { $$ = false; }

maybe_pkg: STRING              { $$ = $1; }
|          %empty              { }

optqualified: "qualified"      { $$ = true; }
|             %empty           { $$ = false; }

maybeas:  "as" modid           { $$ = $2; }
|         %empty               { }

maybeimpspec: impspec          { $$ = $1; }
|             %empty           { }

impspec: "(" exportlist ")"           { $$ = expression_ref{AST_node("only"),$2}; }
|        "hiding" "(" exportlist ")"  { $$ = expression_ref{AST_node("hiding"),$3}; }


/* ------------- Fixity Declarations ----------------------------- */

prec: %empty       { }
|     INTEGER      { $$ = $1; }

infix: "infix"     { $$ = "infix";  }
|      "infixl"    { $$ = "infixl"; }
|      "infixr"    { $$ = "infixr"; }

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
|        "default" "(" comma_types0 ")"        {}
/*
|        "foreign" fdecl
|        "{-# DEPRECATED" deprecations "#-}"
|        "{-# WARNING" warnings "#-}"
|        "{-# RULES" rules "#-}"
|        annotation*/
|        decl_no_th                            {$$ = $1;}
/* What is this for? How is this a decl ? */
|        infixexp_top                          {$$ = make_infixexp($1);}
|        "builtin" var INTEGER STRING STRING   {$$ = make_builtin_expr($2,$3,$4,$5);}
|        "builtin" var INTEGER STRING          {$$ = make_builtin_expr($2,$3,$4);}
|        "builtin" varop INTEGER STRING STRING {$$ = make_builtin_expr($2,$3,$4,$5);}
|        "builtin" varop INTEGER STRING        {$$ = make_builtin_expr($2,$3,$4);}

cl_decl: "class" tycl_hdr /*fds*/ wherebinds   {$$ = make_class_decl($2.first,$2.second,{@3,$3});}

ty_decl: "type" type "=" ctypedoc                                          {$$ = make_type_synonym({@2,$2},{@4,$4});}
|        data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings       {$$ = make_data_or_newtype($1,$3.first,$3.second,$4);}
|        data_or_newtype capi_ctype tycl_hdr opt_kind_sig                  {}
/* |        "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family */
/* |        "data" "family" type opt_datafam_kind_sig */

// inst_type -> sigtype -> ctype --maybe--> context => type
inst_decl: "instance" overlap_pragma inst_type wherebinds                  {$$ = make_instance_decl({@3,$3},{@4,$4});}
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

data_or_newtype: "data"    {$$=Haskell::DataOrNewtype::data;}
|                "newtype" {$$=Haskell::DataOrNewtype::newtype;}

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

/* Remove specializtion of binds for classes and instances */

decls: decls ";" decl   {$$ = $1; $$.push_back($3);}
|      decls ";"        {$$ = $1;}
|      decl             {$$.push_back($1);}
|      %empty           {}

decllist: "{" decls "}"          {$$ = expression_ref{AST_node("Decls"),$2};}
|         VOCURLY decls close    {$$ = expression_ref{AST_node("Decls"),$2};}

binds: decllist                  {$$ = $1;}

wherebinds: "where" binds        {$$ = $2;}
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

opt_sig: %empty  {}
| "::" sigtype   {$$ = $2;}

opt_tyconsig: %empty {}
| "::" gtycon        {$$ = make_type_con($2);}

sigtype: ctype   {$$ = $1;}

sigtypedoc: ctypedoc {$$ = $1;}

sig_vars: sig_vars "," var {$$ = $1; $$.push_back($3);}
|         var              {$$.push_back($1);}

sigtypes1: sigtype               {$$.push_back($1);}
|          sigtypes1 "," sigtype {$$ = $1; $$.push_back($3);}

/* ------------- Types ------------------------------------------- */

strict_mark: strictness                     {$$ = $1;}
|            unpackedness                   {}
|            unpackedness strictness        {$$ = $2;}

strictness: "!" {$$ = "!";}
|           "~" {$$ = "~";}

unpackedness: "{-# UNPACK" "#-"
|             "{-# NOUNPACK" "#-"

ctype: "forall" tv_bndrs "." ctype {$$ = new expression(AST_node("forall"),{make_tv_bndrs($2),$4});}
|      context "=>" ctype          {$$ = $3;} // FIXME - add the context!
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

context_no_ops: btype_no_ops       {$$ = make_context(make_tyapps($1));}

type: btype                        {$$ = $1;}
|     btype "->" ctype             {$$ = make_tyapps({make_type_con("->"),$1,$3});}

typedoc: type                      {$$ = $1;}
/* typedoc: .... */

btype: tyapps                      {$$ = make_tyapps($1);}

btype_no_ops: atype_docs               {$$.push_back($1);}
|             btype_no_ops atype_docs  {$$ = $1; $$.push_back($2);}

tyapps: tyapp                      {$$.push_back($1);}
|       tyapps tyapp               {$$ = $1; $$.push_back($2);}

tyapp: atype                       {$$ = $1;}
|      qtyconop                    {$$ = make_type_con($1);}
|      tyvarop                     {$$ = make_type_var($1);}
/* Template Haskell
|      SIMPLEQUOTE qconop
|      SIMPLEQUOTE varop
*/

atype_docs: atype /* FIX */        {$$ = $1;}

atype: ntgtycon                        {$$ = make_type_con($1);}
|      tyvar                           {$$ = make_type_var($1);}
|      "*"                             {$$ = AST_node("kind_star");}
|      strict_mark atype               {$$ = expression_ref(AST_node("strictness"),{$1,$2});}
|      "{" fielddecls "}"              {$$ = expression_ref{AST_node("FieldDecls"),$2};}
|      "(" ")"                         {$$ = make_type_con("()");}
|      "(" comma_types1 "," ctype")"   {auto ts = $2;ts.push_back($4);$$ = expression_ref{AST_node("TupleType"),ts};}
|      "(#" "#)"                       {}
|      "(#" comma_types1 "#)"          {}
|      "(#" bar_types2   "#)"          {}
|      "[" ctype "]"                   {$$ = expression_ref(AST_node("ListType"),{$2});}
|      "(" ctype ")"                   {$$ = $2;}
|      "(" ctype "::" kind ")"         {$$ = expression_ref(AST_node("TypeOfKind"),{$2,$4});}
/* Template Haskell */

inst_type: sigtype                     {$$ = $1;}

deriv_types: typedoc
|            typedoc "," deriv_types

comma_types0: comma_types1             {$$ = $1;}
|             %empty                   {}

comma_types1: ctype                    {$$.push_back($1);}
|             comma_types1 "," ctype   {$$ = $1; $$.push_back($3);}

bar_types2: ctype "|" ctype
|           ctype "|" bar_types2

tv_bndrs:   tv_bndrs tv_bndr   {$$ = $1; $$.push_back($2);}
|           %empty             {}

tv_bndr:    tyvar                   {$$ = AST_node("type_id",$1);}
|           "(" tyvar "::" kind ")" {$$ = new expression(AST_node("type_of_kind"),{AST_node("type_id",$2),$4});}


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

kind: ctype  {$$ = $1;}



/* ------------- Datatype declarations --------------------------- */

constrs: "=" constrs1           {$$ = $2;}

constrs1: constrs1 "|" constr   {$$ = $1; $$.push_back($3);}
|         constr                {$$.push_back($1);}

constr: forall context_no_ops "=>" constr_stuff {$$ = $4;}  // expand the constructor AST struct to have a context.
|       forall constr_stuff                     {$$ = $2;}

forall: "forall" tv_bndrs "."   {if ($2.size()>1) $$ = make_tv_bndrs($2);}
|       %empty                  {}

constr_stuff: btype_no_ops                      {$$ = make_tyapps($1);}
|             btype_no_ops conop btype_no_ops   {$$ = make_tyapps({AST_node("type_id",$2),make_tyapps($1),make_tyapps($3)});}

fielddecls: %empty              {}
|           fielddecls1         {$$ = $1;}

fielddecls1: fielddecls1 "," fielddecl  {$$ = $1; $$.push_back($3);}
|            fielddecl                  {$$.push_back($1);}

fielddecl: sig_vars "::" ctype          {$$ = new expression(AST_node("FieldDecl"),{make_sig_vars($1),$3});}

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
| "!" aexp rhs                {$$ = new expression(AST_node("Decl:Strict"),{($2),$3});}
/* what is the opt_sig doing here? */
| infixexp_top opt_sig rhs    {$$ = new expression(AST_node("Decl"),{make_infixexp($1),$3});}
| pattern_synonym_decl        {}
/* | docdel */

decl: decl_no_th              {$$ = $1;}
/*  | splice_exp */

// rhs is like altrhs but with = instead of ->
rhs: "=" exp wherebinds       {$$ = make_rhs($2,$3);}
|    gdrhs wherebinds         {$$ = make_gdrhs($1,$2);}

gdrhs: gdrhs gdrh             {$$ = $1; $$.push_back($2);}
|      gdrh                   {$$.push_back($1);}


// gdrh is like gdpat, but with = instead of ->
gdrh: "|" guardquals "=" exp  {$$ = make_gdrh($2,$4);}

sigdecl: infixexp_top "::" sigtypedoc        { $$ = expression_ref(AST_node("Decl:sigtype"),{make_infixexp($1),$3});}
|        var "," sig_vars "::" sigtypedoc {}
|        infix prec ops  { $$ = make_infix($1,$2,$3); }
|        pattern_synonym_sig {}
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

exp: infixexp "::" sigtype { $$ = make_typed_exp(make_infixexp($1),$3); }
|    infixexp              { $$ = make_infixexp($1); }

infixexp: exp10                 {$$.push_back($1);}
|         infixexp qop exp10    {$$ = $1; $$.push_back(make_id(@2,$2)); $$.push_back($3);}

infixexp_top: exp10_top         {$$.push_back($1);}
|             infixexp_top qop exp10_top  {$$ = $1; $$.push_back(make_id(@2,$2)); $$.push_back($3);}

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

aexp: qvar "@" aexp              {$$ = make_as_pattern(make_id(@1,$1),$3);}
|     "~" aexp                   {$$ = make_lazy_pattern($2);}
|     "\\" apats1 "->" exp       {$$ = make_lambda($2,$4);}
|     "let" binds "in" exp       {$$ = make_let($2,$4);}
|     "\\" "case" altslist       {}
|     "if" exp optSemi "then" exp optSemi "else" exp   {$$ = make_if($2,$5,$8);}
|     "if" ifgdpats              {}
|     "case" exp "of" altslist   {$$ = make_case($2,make_alts($4));}
|     "do" stmtlist              {$$ = make_do($2);}
|     "mdo" stmtlist             {$$ = make_mdo($2);}
|     "proc" aexp "->" exp       {}
|     aexp1                      {$$ = $1;}

aexp1: aexp1 "{" fbinds "}"   {}
|      aexp2                  {$$ = $1;}

aexp2: qvar                   {$$ = make_id(@1,$1);}
|      qcon                   {$$ = make_id(@1,$1);}
|      literal                {$$ = $1;}
|      "(" texp ")"           {$$ = $2;}
|      "(" tup_exprs ")"      {$$ = yy_make_tuple($2);}
|      "(#" texp "#)"         {}
|      "(#" tup_exprs "#)"    {}
|      "[" list "]"           {$$ = $2;}
|      "_"                    {$$ = Haskell::WildcardPattern();}
/* Skip Template Haskell Extensions */

/* ------------- Tuple expressions ------------------------------- */

texp: exp             {$$ = $1;}
|     infixexp qop    {$$ = new expression(AST_node("LeftSection"),{make_infixexp($1),make_id(@2,$2)});}
|     qopm infixexp   {$$ = new expression(AST_node("RightSection"),{make_id(@1,$1),make_infixexp($2)});}
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

list: texp                       { $$ = make_list({$1}); }
|     lexps                      { $$ = make_list($1); }
|     texp ".."                  { $$ = expression_ref(AST_node("enumFrom"),{$1}); }
|     texp "," exp ".."          { $$ = expression_ref(AST_node("enumFromThen"),{$1,$3}); }
|     texp ".." exp              { $$ = expression_ref(AST_node("enumFromTo"),{$1,$3}); }
|     texp "," exp ".." exp      { $$ = expression_ref(AST_node("enumFromThenTo"),{$1,$3,$5}); }
|     texp "|" squals            { auto quals = $3; quals.push_back($1); $$ = expression_ref(AST_node("ListComprehension"),quals); }

lexps: lexps "," texp            { $$ = $1; $$.push_back($3);}
|      texp "," texp             { $$.push_back($1); $$.push_back($3);}


/* ------------- List Comprehensions ----------------------------- */

/*
flattenedpquals: pquals                   {$$ = $1;}

pquals: squals "|" pquals                 {$$.push_back(make_squals($1));$$.insert($$.end(),$3.begin(),$3.end());}
|       squals                            {$$.push_back(make_squals($1));}
*/

squals: squals "," transformqual          {$$ = $1; $$.push_back($3);}
|       squals "," qual                   {$$ = $1; $$.push_back($3);}
|       transformqual                     {$$.push_back($1);}
|       qual                              {$$.push_back($1);}

transformqual: "then" exp                           {}
|              "then" exp "by" exp                  {}
|              "then" "group" "using" exp           {}
|              "then" "group" "by" exp "using" exp  {}

/* ------------- Guards ------------------------------------------ */
guardquals: guardquals1            {$$ = $1;}

guardquals1: guardquals1 "," qual  {$$ = $1;$$.push_back($3);}
|            qual                  {$$.push_back($1);}

/* ------------- Case alternatives ------------------------------- */
altslist: "{" alts "}"           {$$ = $2;}
|         VOCURLY alts close     {$$ = $2;}
|         "{" "}"                {}
|         VOCURLY close          {}

alts: alts1                      {$$ = $1;}
|     ";" alts                   {$$ = $2;}

alts1: alts1 ";" alt             {$$ = $1; $$.push_back($3);}
|      alts1 ";"                 {$$ = $1;}
|      alt                       {$$.push_back($1);}

alt:   pat alt_rhs               {$$ = yy_make_alt($1,$2);}

alt_rhs: "->" exp wherebinds     {$$ = make_rhs($2,$3);}
|        gdpats   wherebinds     {$$ = make_gdrhs($1,$2);}

gdpats: gdpats gdpat             {$$ = $1; $$.push_back($2);}
|       gdpat                    {$$.push_back($1);}

ifgdpats : "{" gdpats "}"        {}
|          gdpats close          {}

gdpat: "|" guardquals "->" exp   {$$=make_gdrh($2,$4);}

pat: exp      {$$ = $1;}
|   "!" aexp  {$$ = make_strict_pattern($2);}

bindpat: exp  {$$ = $1;}
|   "!" aexp  {$$ = make_strict_pattern($2);}

apat: aexp    {$$ = $1;}
|    "!" aexp {$$ = make_strict_pattern($2);}

apats1: apats1 apat {$$ = $1; $$.push_back($2);}
|       apat        {$$.push_back($1);}

/* ------------- Statement sequences ----------------------------- */
stmtlist: "{" stmts "}"        {$$ = $2;}
|         VOCURLY stmts close  {$$ = $2;}

stmts: stmts ";" stmt  {$$ = $1; $$.push_back($3);}
|      stmts ";"       {$$ = $1;}
|      stmt            {$$.push_back($1);}
|      %empty          {}

/*maybe_stmt:   stmt
|             %empty */

stmt: qual              {$$ = $1;}
|     "rec" stmtlist    {$$ = new expression(AST_node("Rec"),{$2});}

qual: bindpat "<-" exp  {$$ = Haskell::PatQual($1,$3);}
|     exp               {$$ = Haskell::SimpleQual($1);}
|     "let" binds       {$$ = Haskell::LetQual($2);}


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

qop:  qvarop    { $$ = $1; }
|     qconop    { $$ = $1; }
|     hole_op   { $$ = $1; }

qopm: qvaropm   { $$ = $1; }
|     qconop    { $$ = $1; }
|     hole_op   { $$ = $1; }

hole_op: "`" "_" "`"  { $$ = "_"; }

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

literal: CHAR     {$$ = $1;}
|        STRING   {$$ = yy_make_string($1);}
|        INTEGER  {$$ = $1;}
|        RATIONAL {$$ = $1;}


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

void
yy::parser::error (const location_type& l, const std::string& m)
{
    drv.push_error_message({l,m});
}

expression_ref make_module(const string& name, const expression_ref& exports, const expression_ref& body)
{
    vector<expression_ref> e = {String(name)};
    if (exports)
	e.push_back(exports);
    e.push_back(body);
    return new expression(AST_node("Module"),e);
}

expression_ref make_body(const std::vector<expression_ref>& imports, const std::vector<expression_ref>& topdecls)
{
    expression_ref i = new expression(AST_node("impdecls"),imports);
    expression_ref t = new expression(AST_node("TopDecls"),topdecls);
    return new expression(AST_node("Body"),{i,t});
}

expression_ref make_exports(const vector<expression_ref>& exports)
{
    return new expression(AST_node("Exports"),exports);
}

expression_ref make_builtin_expr(const string& name, int args, const string& s1, const string& s2)
{
    return new expression(AST_node("Builtin"),{String(name), args, String(s1), String(s2)});
}

expression_ref make_builtin_expr(const string& name, int args, const string& s1)
{
    return new expression(AST_node("Builtin"),{String(name), args, String(s1)});
}

vector<expression_ref> make_String_vec(const vector<string>& strings)
{
    vector<expression_ref> Strings;
    for(auto& string: strings)
        Strings.push_back(String(string));
    return Strings;
}

expression_ref make_sig_vars(const vector<std::string>& sig_vars)
{
    return new expression(AST_node("sig_vars"),make_String_vec(sig_vars));
}

// See PostProcess.hs:checkTyClHdr
std::tuple<string, vector<expression_ref>>
check_type_or_class_header(expression_ref type)
{
    string name;
    vector<expression_ref> type_args;

    if (is_AST(type,"TypeApply"))
    {
        for(int i=1;i<type.sub().size();i++)
            type_args.push_back(type.sub()[i]);
        type = type.sub()[0];
    }

    // FIXME -- add location!
    if (not is_AST(type,"type_id"))
        throw myexception()<<"Malformed type or class header '"<<type<<"'";
    name = type.as_<AST_node>().value;

    return {name, type_args};
}

Haskell::InstanceDecl make_instance_decl(const Located<expression_ref>& type, const Located<expression_ref>& decls)
{
    return {type, decls};
}

Haskell::TypeSynonymDecl make_type_synonym(const Located<expression_ref>& lhs_type, const Located<expression_ref>& rhs_type)
{
    auto [name, type_args] = check_type_or_class_header(lhs_type);
    return {name, type_args, rhs_type};
}

Haskell::DataOrNewtypeDecl make_data_or_newtype(const Haskell::DataOrNewtype& d_or_n, const Haskell::Context&  context,
                                                const expression_ref& header, const vector<expression_ref>& constrs)
{
    auto [name, type_args] = check_type_or_class_header(header);
    expression_ref c = new expression(AST_node("constrs"),constrs);
    return {d_or_n, name, type_args, context, c};
}

Haskell::ClassDecl make_class_decl(const Haskell::Context& context, const expression_ref& header, const Located<expression_ref>& decls)
{
    auto [name, type_args] = check_type_or_class_header(header);
    return {name,type_args,context,decls};
}

Haskell::Context make_context(const expression_ref& context)
{
    vector<Haskell::Type> constraints;
    if (context.is_a<Haskell::Tuple>())
    {
        constraints = context.as_<Haskell::Tuple>().elements;
    }
    else
        constraints.push_back(context);

    return {constraints};
}

expression_ref make_tv_bndrs(const vector<expression_ref>& tv_bndrs)
{
    return new expression(AST_node("tv_bndrs"),tv_bndrs);
}

expression_ref type_apply_ast(const expression_ref& e1, const expression_ref& e2)
{
    if (is_AST(e1, "TypeApply"))
	return e1 + e2;
    else
	return AST_node("TypeApply") + e1 + e2;
}


expression_ref make_tyapps(const std::vector<expression_ref>& tyapps)
{
    assert(not tyapps.empty());
    expression_ref E = tyapps[0];
    for(int i=1;i<tyapps.size();i++)
	E = type_apply_ast(E,tyapps[i]);
    return E;
}

Located<Haskell::ID> make_id(const yy::location& loc, const string& id)
{
    return Located<Haskell::ID>(loc, {id});
}

expression_ref make_type_var(const string& id)
{
    return AST_node("type_var",id);
}

expression_ref make_type_con(const string& id)
{
    return AST_node("type_con",id);
}

expression_ref make_typed_exp(const expression_ref& exp, const expression_ref& type)
{
    return new expression(AST_node("typed_exp"),{exp,type});
}

expression_ref make_rhs(const expression_ref& exp, const expression_ref& wherebinds)
{
    vector<expression_ref> e = {exp};
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return expression_ref{AST_node("rhs"), std::move(e)};
}

expression_ref make_infixexp(const vector<expression_ref>& args)
{
    if (args.size() == 1)
	return args[0];
    else
	return new expression(AST_node("infixexp"),args);
}


expression_ref make_minus(const expression_ref& exp)
{
    return new expression(AST_node("infixexp"),{AST_node("neg"),exp});
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

expression_ref make_as_pattern(const Located<Haskell::ID>& x, const expression_ref& pat)
{
    return Haskell::AsPattern(x,pat);
}

expression_ref make_lazy_pattern(const expression_ref& pat)
{
    return Haskell::LazyPattern(pat);
}

expression_ref make_strict_pattern(const expression_ref& pat)
{
    return new expression(AST_node("StrictPattern"), {pat});
}

expression_ref make_lambda(const vector<expression_ref>& pats, const expression_ref& body)
{
    auto e = pats;
    e.push_back(body);
    return new expression(AST_node("Lambda"), e);
}

expression_ref make_let(const expression_ref& binds, const expression_ref& body)
{
    return new expression(AST_node("Let"), {binds, body});
}

expression_ref make_if(const expression_ref& cond, const expression_ref& alt_true, const expression_ref& alt_false)
{
    return new expression(AST_node("If"), {cond, alt_true, alt_false});
}

expression_ref make_case(const expression_ref& obj, const expression_ref& alts)
{
    return new expression(AST_node("Case"), {obj, alts});
}

expression_ref make_do(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Do"), stmts);
}

expression_ref make_mdo(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("MDo"), stmts);
}

expression_ref yy_make_tuple(const vector<expression_ref>& elements)
{
    return Haskell::Tuple(elements);
}

expression_ref make_list(const vector<expression_ref>& elements)
{
    return Haskell::List(elements);
}

expression_ref make_alts(const vector<expression_ref>& alts)
{
    return expression_ref(AST_node("alts"), alts);
}

expression_ref yy_make_alt(const expression_ref& pat, const expression_ref& alt_rhs)
{
    return AST_node("alt") + pat + alt_rhs;
}

expression_ref make_gdrhs(const vector<expression_ref>& guards, const expression_ref& wherebinds)
{
    vector<expression_ref> e = {expression_ref{AST_node("guards"),guards}};
    if (wherebinds and wherebinds.size())
	e.push_back(wherebinds);
    return expression_ref{AST_node("gdrhs"),std::move(e)};
}

expression_ref make_gdrh(const vector<expression_ref>& guardquals, const expression_ref& exp)
{
    return expression_ref(AST_node("gdrh"), {expression_ref(AST_node("guards"),guardquals),exp});
}

expression_ref make_stmts(const vector<expression_ref>& stmts)
{
    return new expression(AST_node("Stmts"), stmts);
}

expression_ref make_infix(const string& infix, optional<int>& prec, vector<string>& op_names)
{
    expression_ref ops = new expression(AST_node("Ops"), make_String_vec(op_names));

    vector<expression_ref> e;
    e.push_back(String(infix));
    if (prec)
	e.push_back(*prec);
    e.push_back(ops);

    return new expression(AST_node("FixityDecl"),e);
}

expression_ref yy_make_string(const std::string& s)
{
    vector<expression_ref> chars;
    for(char c: s)
	chars.push_back(c);
    return make_list(chars);
}

