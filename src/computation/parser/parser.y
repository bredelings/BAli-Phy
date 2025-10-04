%skeleton "lalr1.cc" /* -*- C++ -*- */

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
  # include <optional>
  # include "computation/expression/expression_ref.H"
  # include "computation/haskell/haskell.H"
  # include "computation/typecheck/types.H"
  # include "computation/typecheck/kind.H"
  # include "util/string/join.H"

  class driver;

  std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>> make_body(const std::vector<Hs::LImpDecl>& imports, const std::optional<Hs::Decls>& topdecls);

  Hs::Kind type_to_kind(const Hs::LType& kind);
  Hs::ConstructorDecl make_constructor(const std::vector<Hs::LTypeVar>& forall, const Hs::Context& c, const Hs::LType& typeish);
  Hs::InstanceDecl make_instance_decl(const std::optional<std::string>& oprag, const Hs::LType& type, const std::optional<Located<Hs::Decls>>& decls);
  Hs::TypeSynonymDecl make_type_synonym(const Hs::LType& lhs_type, const Hs::LType& rhs_type);
  Hs::FamilyDecl make_family_decl(Hs::FamilyInfo info, const Hs::LType& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig,
				  const std::optional<std::vector<Hs::TypeFamilyInstanceEqn>>& eqns);
  Hs::TypeFamilyInstanceEqn make_type_family_instance_eqn(const Hs::LType& lhs_type, const Hs::LType& rhs_type);
  Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context& context,
                                             const Hs::LType& header, const std::optional<Hs::Kind>&, const Hs::ConstructorsDecl& constrs);
  Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context& context,
                                             const Hs::LType& header, const std::optional<Hs::Kind>&, const Hs::GADTConstructorsDecl& constrs);
  Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::LType& header, const std::vector<Hs::FunDep>& fds, const std::optional<Located<Hs::Decls>>& decls);
  Hs::Context make_context(const Hs::LType& context);
  std::tuple<Located<Hs::TypeCon>, std::vector<Hs::LType>> check_type_or_class_header2(const Hs::LType& type);

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
  TIGHT_INFIX_AT
  PREFIX_AT     "@"
  PREFIX_TILDE
  TILDE         "~"
  DARROW        "=>"
  PREFIX_MINUS
  MINUS         "-"
  PREFIX_BANG
  BANG          "!"
  STAR          "*"
  lARROWTAIL    "-<"
  rARROWTAIL    ">-"
  LARROWTAIL    "-<<"
  RARROWTAIL    ">>-"
  TIGHT_INFIX_DOT
  PREFIX_DOT
  DOT           "."

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

%token <std::string> INLINE_PRAG "{-# INLINE"
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
%token <integer>       INTEGER  "INTEGER"
%token <rational>      RATIONAL "RATIONAL"

%token <char>          PRIMCHAR    "PRIMCHAR"
%token <std::string>   PRIMSTRING  "PRIMSTRING"
%token <integer>       PRIMINTEGER "PRIMINTEGER"
%token <int>           PRINTWORD   "PRIMWORD"
%token <float>         PRIMFLOAT   "PRIMFLOAT"
%token <double>        PRIMDOUBLE  "PRIMDOUBLE"

 /* DOCNEXT DOCPREV DOCNAMED DOCSECTION: skipped tokens.*/

 /* Template Haskell: skipped tokens.*/


%type <Hs::Module> module
 /* %type <void> missing_module_keyword */
 /* %type <expression_ref> maybemodwarning */
%type <std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>>> body2
%type <std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>>> body
%type <std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>>> top
%type <std::pair<std::vector<Hs::LImpDecl>, std::optional<Hs::Decls>>> top1

%type <std::optional<std::vector<Hs::LExport>>> maybeexports
%type <std::vector<Hs::LExport>> exportlist
%type <std::vector<Hs::LExport>> exportlist1
%type <Hs::LExport> export
%type <std::optional<Hs::ExportSubSpec>> export_subspec
%type <std::vector<Located<std::string>>> qcnames
%type <std::vector<Located<std::string>>> qcnames1
%type <Located<std::string>> qcname

%type <std::vector<Hs::LImpDecl>> importdecls
%type <std::vector<Hs::LImpDecl>> importdecls_semi
%type <Hs::LImpDecl> importdecl
 // %type <bool> maybe_src
 // %type <bool> maybe_safe
 // %type <std::optional<std::string>> maybe_pkg
%type <bool> optqualified
%type <std::optional<Located<std::string>>> maybeas
%type <std::optional<Hs::ImpSpec>> maybeimpspec
%type <std::optional<Located<std::string>>> opt_class
%type <Hs::ImpSpec> impspec
%type <std::vector<Hs::LExport>> importlist
%type <std::vector<Hs::LExport>> importlist1
%type <Hs::LExport> import

%type <std::optional<int>> prec
%type <Hs::Fixity> infix
%type <std::vector<Located<std::string>>> ops


%type <Hs::Decls> topdecls
%type <Hs::Decls> topdecls_semi
%type <Located<expression_ref>> topdecl
%type <Located<expression_ref>> cl_decl
%type <Located<expression_ref>> ty_decl
%type <Located<expression_ref>> inst_decl

%type <Located<expression_ref>> standalone_kind_sig

%type <std::vector<Hs::LTypeCon>> sks_vars

%type <std::optional<std::string>> overlap_pragma
 /*
%type <void> deriv_strategy_no_via
%type <void> deriv_strategy_via
 */
%type <Hs::DataOrNewtype> data_or_newtype
%type <std::optional<Hs::Kind>> opt_kind_sig
%type <std::optional<Located<Hs::Kind>>> opt_datafam_kind_sig
%type <std::optional<Located<Hs::Kind>>> opt_tyfam_kind_sig
%type <std::optional<Located<Hs::Kind>>> opt_at_kind_inj_sig

%type <std::pair<Hs::Context,Hs::LType>> tycl_hdr
%type <std::tuple<std::optional<std::vector<Hs::LTypeVar>>,std::optional<Located<Hs::Context>>,Hs::LType>> datafam_inst_hdr
/* %type <void> capi_ctype 


%type <void> pattern_synonym_decl
%type <void> pattern_synonym_lhs
%type <void> vars0
%type <void> cvars1
%type <void> where_decls
%type <void> pattern_synonym_sig
*/

%type <Located<expression_ref>> decl_cls
%type <Hs::Decls> decls_cls
%type <Located<Hs::Decls>> decllist_cls
%type <std::optional<Located<Hs::Decls>>> where_cls

%type <Located<expression_ref>> at_decl_cls
%type <Located<expression_ref>> at_decl_inst

%type <Located<expression_ref>> decl_inst
%type <Hs::Decls> decls_inst
%type <Located<Hs::Decls>> decllist_inst
%type <std::optional<Located<Hs::Decls>>> where_inst

%type <std::vector<Located<expression_ref>>> decls
%type <Hs::Decls> decllist
%type <Located<Hs::Binds>> binds
%type <std::optional<Located<Hs::Binds>>> wherebinds

%type <std::optional<std::vector<Hs::TypeFamilyInstanceEqn>>> where_type_family
%type <std::vector<Hs::TypeFamilyInstanceEqn>> ty_fam_inst_eqn_list
%type <std::vector<Hs::TypeFamilyInstanceEqn>> ty_fam_inst_eqns
%type <Hs::TypeFamilyInstanceEqn> ty_fam_inst_eqn

 /*

%type <void> strings
%type <void> stringlist
%type <expression_ref> opt_sig
 */
%type <std::optional<Hs::LType>> opt_tyconsig
%type <Hs::LType> sigtype
 /* %type <Hs::Type> sigktype */
%type <Hs::LType> sigtypedoc
%type <std::vector<Hs::LVar>> sig_vars
%type <std::vector<Hs::LType>> sigtypes1

%type <Hs::LType> ktype
%type <Hs::LType> ctype
%type <Hs::LType> ctypedoc
%type <Hs::Context> context
%type <Hs::Context> context_no_ops
%type <Hs::LType> type
%type <Hs::LType> typedoc
%type <Hs::LType> btype
%type <std::vector<Hs::LType>> btype_no_ops
%type <Hs::LType> infixtype
%type <Hs::LType> ftype
%type <Hs::LType> tyarg
%type <std::string> tyop
%type <Hs::LType> atype_docs
%type <Hs::LType> atype
%type <Hs::LType> inst_type
 /*
%type <void> deriv_types
 */
%type <std::vector<Hs::LType>> comma_types0
%type <std::vector<Hs::LType>> comma_types1
 /*
%type <void> bar_types2
 */
%type <std::vector<Hs::LTypeVar>> tv_bndrs
%type <Hs::LTypeVar> tv_bndr
%type <Hs::LTypeVar> tv_bndr_no_braces


%type <std::vector<Hs::FunDep>> fds
%type <std::vector<Hs::FunDep>> fds1
%type <Hs::FunDep> fd
%type <std::vector<Hs::LTypeVar>> varids0

%type <Hs::Kind> kind

%type <Hs::GADTConstructorsDecl> gadt_constrlist
%type <Hs::GADTConstructorsDecl> gadt_constrs
%type <Hs::GADTConstructorDecl> gadt_constr

%type <Hs::ConstructorsDecl> constrs
%type <Hs::ConstructorsDecl> constrs1
%type <Hs::ConstructorDecl> constr
%type <std::vector<Hs::LTypeVar>> forall
%type <Hs::LType> constr_stuff
%type <std::vector<Hs::FieldDecl>> fielddecls
%type <std::vector<Hs::FieldDecl>> fielddecls1
%type <Hs::FieldDecl> fielddecl
 /*
%type <void> maybe_derivings
%type <void> derivings
%type <void> deriv_clause_types
 */

%type <Located<expression_ref>> decl_no_th
%type <Located<expression_ref>> decl
%type <Hs::MultiGuardedRHS> rhs
%type <std::vector<Hs::GuardedRHS>> gdrhs
%type <Hs::GuardedRHS> gdrh
%type <Located<expression_ref>> sigdecl
 /*
%type <void> activation
%type <void> explicit_activation
 */

%type <Located<expression_ref>> exp
%type <Located<Hs::InfixExp>> infixexp
%type <Located<expression_ref>> exp10

%type <Located<expression_ref>> fexp
%type <Located<expression_ref>> aexp
%type <Located<expression_ref>> aexp1
%type <Located<expression_ref>> aexp2
%type <Located<expression_ref>> texp
%type <std::vector<Located<expression_ref>>> tup_exprs
 /*
%type <void> tup_tail
 */
%type <expression_ref> list
%type <std::vector<Located<expression_ref>>> lexps

 /* 
%type <std::vector<expression_ref>> flattenedpquals 
%type <std::vector<expression_ref>> pquals
 */
%type <std::vector<Located<expression_ref>>> squals
 /* %type <expression_ref> transformqual */

%type <std::vector<Located<expression_ref>>> guardquals
%type <std::vector<Located<expression_ref>>> guardquals1

%type <Hs::Alts> altslist
%type <std::vector<Located<Hs::Alt>>> alts
%type <std::vector<Located<Hs::Alt>>> alts1
%type <Located<Hs::Alt>> alt
%type <Hs::MultiGuardedRHS> alt_rhs
%type <std::vector<Hs::GuardedRHS>> gdpats
 /* %type <expression_ref> ifgdpats */
%type <Hs::GuardedRHS> gdpat
%type <Located<expression_ref>> pat
%type <Located<expression_ref>> bindpat
%type <Located<expression_ref>> apat
%type <std::vector<Located<expression_ref>>> apats1

%type <Hs::Stmts> stmtlist
%type <std::vector<Located<expression_ref>>> stmts
%type <Located<expression_ref>> stmt

%type <Located<expression_ref>> qual

%type <Located<Hs::FieldBindings>> fbinds
%type <Located<Hs::FieldBindings>> fbinds1
%type <std::optional<Located<Hs::FieldBinding>>> fbind

/*
%type <void> dbinds
%type <void> dbind
%type <std::string> ipvar 
%type <std::string> overloaded_label
*/

 /* %type <std::string> qcon_nowiredlist */
%type <std::string> qcon
%type <std::string> gen_qcon
%type <std::string> con
%type <std::vector<Located<std::string>>> con_list
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

 // These should all be located?
%type <std::string> var
%type <std::string> qvar
%type <std::string> field
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
%type  <Located<std::string>> modid
%type  <int> commas
/*
%type  <int> bars0
%type  <int> bars
*/

%expect 149

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
| body2                                                          {$$ = Hs::Module{{noloc,"Main"},{},$1.first, $1.second};}

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
|           %empty                    {}
|           exportlist1 ','           {$$ = $1;}
|           ','                       {}

exportlist1: exportlist1 "," export   {$$ = $1; $$.push_back($3);}
|            export                   {$$.push_back($1);}

export: qcname export_subspec         {$$ = {@$,Hs::Export{{}, $1, $2}}; }
|       "module" modid                {$$ = {@$,Hs::Export{{{@1,Hs::ImpExpNs::module}}, $2, {}}}; }
|       "default" modid               {$$ = {@$,Hs::Export{{{@1,Hs::ImpExpNs::default_}}, $2, {}}}; }

export_subspec: %empty                {}
|              "(" qcnames ")"        { $$ = Hs::ExportSubSpec{$2}; }
|              "(" ".." ")"           { $$ = Hs::ExportSubSpec(); }

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
    $$ = {@$, Hs::ImpDecl($2,$3,$4,$5)};
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

impspec: "(" importlist ")"           { $$ = Hs::ImpSpec{false, $2}; }
|        "hiding" "(" importlist ")"  { $$ = Hs::ImpSpec{true,  $3}; }

importlist: importlist1               {$$ = $1;}
|           %empty                    {}
|           importlist1 ','           {$$ = $1;}
|           ','                       {}

importlist1: importlist1 "," import   {$$ = $1; $$.push_back($3);}
|            import                   {$$.push_back($1);}

import: qcname export_subspec         {$$ = {@$,Hs::Export{{}, $1, $2}}; }
|       "module" modid                {$$ = {@$,Hs::Export{{{@1,Hs::ImpExpNs::module}}, $2, {}}}; }


/* ------------- Fixity Declarations ----------------------------- */

prec: %empty       { }
|     INTEGER      { $$ = $1.convert_to<int>(); }

infix: "infix"     { $$ = Hs::Fixity::infix; }
|      "infixl"    { $$ = Hs::Fixity::infixl; }
|      "infixr"    { $$ = Hs::Fixity::infixr; }

ops:   ops "," op  { $$ = $1; $$.push_back({@3,$3}); }
|      op          { $$ = {{@1,$1}}; }

/* ------------- Top-Level Declarations -------------------------- */

topdecls: topdecls_semi topdecl  { $$ = $1; $$.push_back($2); }

topdecls_semi: topdecls_semi topdecl semis1 { $$ = $1; $$.push_back($2); }
|              %empty                       { }

topdecl: cl_decl                               {$$ = $1;}
|        ty_decl                               {$$ = $1;}
|        standalone_kind_sig                   {$$ = $1;}
|        inst_decl                             {$$ = $1;}
/*|        stand_alone_deriving */
/*|        role_annot */
|        "default" opt_class "(" comma_types0 ")"        {$$ = {@$,Hs::DefaultDecl($2,$4)}; }
|        "foreign" "import" "bpcall" STRING var "::" sigtypedoc  {$$ = {@$,Hs::ForeignDecl($4, {@5,$5}, $7)};}
/*
|        "{-# DEPRECATED" deprecations "#-}"
|        "{-# WARNING" warnings "#-}"
|        "{-# RULES" rules "#-}"
|        annotation*/
|        decl_no_th                            {$$ = $1;}
/* What is this for? How is this a decl ? */
|        infixexp                              {$$ = $1;}


cl_decl: "class" tycl_hdr fds where_cls   {$$ = {@$,make_class_decl($2.first,$2.second,$3,$4)};}


ty_decl: "type" type "=" ktype                                             {$$ = {@$, make_type_synonym($2,$4)};}
|        data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings       {$$ = {@$, make_data_or_newtype($1, $3.first, $3.second,{},$4)};}
/* This is kind of a hack to allow `data X` */
|        data_or_newtype capi_ctype tycl_hdr opt_kind_sig gadt_constrlist maybe_derivings {$$ = {@$, make_data_or_newtype($1, $3.first, $3.second, $4, $5)};}
|        "type" "family" type opt_tyfam_kind_sig opt_injective_info where_type_family     {$$ = {@$, make_family_decl(Hs::TypeFamily, $3, $4, $6)};}
|        "data" "family" type opt_datafam_kind_sig                                        {$$ = {@$, make_family_decl(Hs::DataFamily, $3, $4, {})};}

standalone_kind_sig: "type" sks_vars "::" kind                             {$$ = {@$,Hs::KindSigDecl($2,$4)};}

sks_vars: sks_vars "," oqtycon                                             {$$ = $1; $$.push_back({@3,Hs::TypeCon($3)}); }
|         oqtycon                                                          {$$ = {{@1,Hs::TypeCon($1)}}; }

// inst_type -> sigtype -> ctype --maybe--> context => type
inst_decl: "instance" overlap_pragma inst_type where_inst                  {$$ = {@$,make_instance_decl($2,$3,$4)};}
|          "type" "instance" ty_fam_inst_eqn                               {$$ = {@$,Hs::TypeFamilyInstanceDecl($3)};}
|          data_or_newtype "instance" capi_ctype datafam_inst_hdr constrs maybe_derivings
           {
	       auto& [tvs,context,type] = $4;
	       auto [con, args] = check_type_or_class_header2(type);
	       $$ = {@$, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn($1, {}, {}, $5))};
	   }
|          data_or_newtype "instance" capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
           {
	       auto& [tvs,context,type] = $4;
	       auto [con, args] = check_type_or_class_header2(type);
	       $$ = {@$, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn($1, Hs::Context(), $5, $6))};
	   }

overlap_pragma: "{-# OVERLAPPABLE" "#-}"       { $$ = "OVERLAPPABLE"; }
|               "{-# OVERLAPPING" "#-}"        { $$ = "OVERLAPPING"; }
|               "{-# OVERLAPS" "#-}"           { $$ = "OVERLAPS"; }
|               "{-# INCOHERENT" "#-}"         { $$ = "INCOHERENT"; }
|               %empty                         {}
   
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

/* Injective type families */

opt_injective_info: %empty
|                   "|" injectivity_cond

injectivity_cond: tyvarid "->" inj_varids

inj_varids: inj_varids tyvarid
|           tyvarid

/* Closed type families  */

where_type_family: %empty                                  {}
|                  "where" ty_fam_inst_eqn_list            {$$ = $2;}

ty_fam_inst_eqn_list: "{" ty_fam_inst_eqns "}"             {$$ = $2;}
|                     VOCURLY ty_fam_inst_eqns close       {$$ = $2;}
|                     "{" ".." "}"                         {}
|                     VOCURLY ".." close                   {}

ty_fam_inst_eqns: ty_fam_inst_eqns ";" ty_fam_inst_eqn     {$$ = $1; $$.push_back($3);}
|                 ty_fam_inst_eqns ";"                     {$$ = $1;}
|                 ty_fam_inst_eqn                          {$$ = {$1};}
|                 %empty                                   {}

ty_fam_inst_eqn: type "=" ctype                            {$$ = make_type_family_instance_eqn($1,$3);}

/* Associated type family declarations */
at_decl_cls: "data" opt_family type opt_datafam_kind_sig       { $$ = {@$, make_family_decl(Hs::DataFamily, $3, $4, {})}; }
              /* we can't use opt_family or we get shift/reduce conflicts*/
|            "type" type opt_at_kind_inj_sig                   { $$ = {@$, make_family_decl(Hs::TypeFamily, $2, $3, {})}; }

|            "type" "family" type opt_at_kind_inj_sig          { $$ = {@$, make_family_decl(Hs::TypeFamily, $3, $4, {})}; }
              /* we can't use opt_family or we get shift/reduce conflicts*/
|            "type" ty_fam_inst_eqn                            { $$ = {@$, Hs::TypeFamilyInstanceDecl($2)};    }
|            "type" "instance" ty_fam_inst_eqn                 { $$ = {@$, Hs::TypeFamilyInstanceDecl($3)};    }

opt_family: %empty | "family"

opt_instance: %empty | "instance"

/* Associated type instance declarations */

at_decl_inst: "type" opt_instance ty_fam_inst_eqn             { $$ = {@$,Hs::TypeFamilyInstanceDecl($3)};    }

|             data_or_newtype opt_instance capi_ctype datafam_inst_hdr constrs maybe_derivings
              {
		  auto& [tvs, context, type] = $4;
		  auto [con, args] = check_type_or_class_header2(type);
		  $$ = {@$, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn($1, {}, {}, $5))};
	      }

|             data_or_newtype opt_instance capi_ctype datafam_inst_hdr opt_kind_sig gadt_constrlist maybe_derivings
              {
		  auto& [tvs,context,type] = $4;
		  auto [con, args] = check_type_or_class_header2(type);
		  $$ = {@$, Hs::DataFamilyInstanceDecl(con, args, Hs::DataDefn($1, Hs::Context(), $5, $6))};
	      }

data_or_newtype: "data"    {$$=Hs::DataOrNewtype::data;}
|                "newtype" {$$=Hs::DataOrNewtype::newtype;}


opt_class: %empty {$$ = {};}
|          qtycon {$$ = {@1,$1};}

/* Family results and kind signatures */

opt_kind_sig: %empty       {$$ = {};}
|             "::" kind    {$$ = $2;}

opt_datafam_kind_sig: %empty     {$$ = {};}
|                     "::" kind  {$$ = {{@2,$2}};}

opt_tyfam_kind_sig: %empty            {}
|                   "::" kind         {$$ = {{@2,$2}};}
|                   "=" tv_bndr       {}

opt_at_kind_inj_sig: %empty           {}
|                    "::" kind        {$$ = {{@2,$2}};}
|                    "=" tv_bndr_no_braces "|" injectivity_cond   {}

/* Type class header */
tycl_hdr: context "=>" type  {$$ = {$1,$3};}
|         type               {$$ = {{},$1};}


datafam_inst_hdr: "forall" tv_bndrs "." context "=>" type { $$ = {{$2}, {{@4,$4}}, $6}; }
|                  "forall" tv_bndrs "=>" type            { $$ = {{$2}, {}, $4}; }
|                  context "=>" type                      { $$ = {{}, {{@1,$1}}, $3}; }
|                  type                                   { $$ = {{}, {}, $1}; }

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

decl_cls:  at_decl_cls  {$$ = $1;}
|          decl         {$$ = $1;}

decls_cls: decls_cls ";" decl_cls          {$$ = $1; $$.push_back($3);}
|          decls_cls ";"                   {$$ = $1;}
|          decl_cls                        {$$.push_back($1);}
|          %empty                          {}

decllist_cls: "{" decls_cls "}"            {$$ = {@2,$2};}
|               VOCURLY decls_cls close    {$$ = {@2,$2};}

where_cls: "where" decllist_cls            {$$ = $2;}
|            %empty                        {}

decl_inst: at_decl_inst                    {$$ = $1;}
|          decl                            {$$ = $1;}

decls_inst: decls_inst ";" decl_inst       {$$ = $1; $$.push_back($3);}
|          decls_inst ";"                  {$$ = $1;}
|          decl_inst                       {$$.push_back($1);}
|          %empty                          {}

decllist_inst: "{" decls_inst "}"          {$$ = {@2,$2};}
|               VOCURLY decls_inst close   {$$ = {@2,$2};}

where_inst: "where" decllist_inst          {$$ = $2;}
|            %empty                        {}


decls: decls ";" decl   {$$ = $1; $$.push_back($3);}
|      decls ";"        {$$ = $1;}
|      decl             {$$.push_back($1);}
|      %empty           {}

decllist: "{" decls "}"          {$$ = $2;}  // location here should include { }?
|         VOCURLY decls close    {$$ = $2;}

binds: decllist                  {$$ = {@1,{$1}};}

wherebinds: "where" binds        {$$ = $2;}                   // location here should include "where"
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

opt_tyconsig: %empty             {$$ = {};}
|             "::" gtycon        {$$ = {{@2,Hs::TypeCon($2)}};}

/*
sigktype: sigtype                {$$ = $1;}
|         ctype "::" kind        {$$ = Hs::TypeOfKind($1, $3);}
*/


/* This is for types that obey the "forall or nothing" rule. */
sigtype: ctype

sigtypedoc: ctypedoc

sig_vars: sig_vars "," var {$$ = $1; $$.push_back({@3,Hs::Var($3)});}
|         var              {$$.push_back({@1,Hs::Var($1)});}

sigtypes1: sigtype               {$$.push_back($1);}
|          sigtypes1 "," sigtype {$$ = $1; $$.push_back($3);}

/* ------------- Types ------------------------------------------- */

/*
unpackedness: "{-# UNPACK" "#-}"
|             "{-# NOUNPACK" "#-}"
*/

ktype: ctype                       {$$ = $1;}
|      ctype "::" kind             {$$ = {@$,Hs::TypeOfKind($1, $3)};}

ctype: "forall" tv_bndrs "." ctype {$$ = {@$,Hs::ForallType($2, $4)};}
|      context "=>" ctype          {$$ = {@$,Hs::ConstrainedType($1,$3)};}
/* |      ipvar "::" type             {} */
|      type

ctypedoc: ctype

/*
ctypedoc:  "forall" tv_bnrds "." ctypedoc
|      context "=>" ctypedoc
|      ipvar "::" type
|      typedoc
*/

context: btype                     {$$ = make_context($1);}

context_no_ops: btype_no_ops       {$$ = make_context(Hs::type_apply($1));}

type: btype
|     btype "->" ctype             {$$ = Hs::type_apply({{@2,Hs::TypeCon("->")},$1,$3});}

typedoc: type
/* typedoc: .... */

btype: infixtype

infixtype: ftype
|          btype tyop btype         {$$ = Hs::type_apply({{@2,Hs::TypeCon($2)},$1,$3});}

btype_no_ops: atype_docs               {$$.push_back($1);}
|             btype_no_ops atype_docs  {$$ = $1; $$.push_back($2);}

ftype: atype
/* |      tyop                     fail op too few args? */
|      ftype tyarg                 { $$ = {@$,Hs::TypeApp($1,$2)}; }
|      ftype PREFIX_AT atype       /* kind application */

tyarg: atype

tyop:  qtyconop                    {$$ = $1;}
|      tyvarop                     {$$ = $1;}
/* Template Haskell
|      SIMPLEQUOTE qconop
|      SIMPLEQUOTE varop
*/


atype_docs: atype /* FIX */

/* '*' is either a binary type operator (m * n) or the * kind, unless the StarIsType
 * extension is enabled.
 * Do I need a separate rule for it if I'm not using StarIsType?
 */

atype: ntgtycon                        {$$ = {@$,Hs::TypeCon($1)};}
|      tyvar                           {$$ = {@$,Hs::TypeVar($1)};}
|      "*"                             {$$ = {@$,Hs::TypeCon("*")};}
|      PREFIX_BANG atype               {$$ = {@$,Hs::StrictType($2)};}
|      PREFIX_TILDE atype              {$$ = {@$,Hs::LazyType($2)};}
|      "{" fielddecls "}"              {$$ = {@$,Hs::FieldDecls($2)};}
|      "(" ")"                         {$$ = {@$,Hs::TypeCon("()")};}
|      "(" comma_types1 "," ktype")"   {auto ts = $2;ts.push_back($4);$$ = {@$,Hs::TupleType(ts)};}
/*
|      "(#" "#)"                       {}
|      "(#" comma_types1 "#)"          {}
|      "(#" bar_types2   "#)"          {}
*/
|      "[" ktype "]"                   {$$ = {@$,Hs::ListType{$2}}; }
|      "(" ktype ")"                   {$$ = $2;}
/* Template Haskell */

inst_type: sigtype                     {$$ = $1;}

deriv_types: typedoc
|            typedoc "," deriv_types

comma_types0: comma_types1             {$$ = $1;}
|             %empty                   { /* default construction OK */ }

comma_types1: ktype                    {$$.push_back($1);}
|             comma_types1 "," ktype   {$$ = $1; $$.push_back($3);}

/*
bar_types2: ktype "|" ktype
|           ktype "|" bar_types2
*/

tv_bndrs:   tv_bndrs tv_bndr   {$$ = $1; $$.push_back($2);}
|           %empty             { /* default construction OK */}

tv_bndr: tv_bndr_no_braces       {$$ = $1;}
|        "{" tyvar "}"           {$$ = {@2,Hs::TypeVar($2)};}
|        "{" tyvar "::" kind "}" {$$ = {@2,Hs::TypeVar($2)};}

/* If we put the kind into the type var (maybe as an optional) we could unify these two */
tv_bndr_no_braces:    tyvar                   {$$ = {@$,Hs::TypeVar($1)};}
|                     "(" tyvar "::" kind ")" {$$ = {@$,Hs::TypeVar($2,$4)};}


/* fds are functional dependencies = FunDeps */
fds:        %empty                  { /* default to empty */ }
|           "|" fds1                { $$ = $2; }

fds1:       fds1 "," fd             { $$ = $1; $$.push_back($3); }
|           fd                      { $$.push_back($1); }


fd:         varids0 "->" varids0    { $$ = Hs::FunDep($1, $3); }

varids0:    varids0 tyvar           { $$ = $1; $$.push_back({@2,$2});}
|           %empty                  { /* default to empty */}


/* ------------- Kinds ------------------------------------------- */

kind: ctype  {$$ = type_to_kind($1);}



/* ------------- Datatype declarations --------------------------- */

gadt_constrlist: "where" "{" gadt_constrs "}"        {$$ = $3;}
|                "where" VOCURLY gadt_constrs close  {$$ = $3;}
|                %empty                              {$$ = {};}

gadt_constrs: gadt_constrs ";" gadt_constr           {$$=$1; $$.push_back($3);}
|             gadt_constr                            {$$.push_back($1);}

gadt_constr: optSemi con_list "::" sigtype           {$$ = Hs::GADTConstructorDecl($2,$4);}

constrs: "=" constrs1           {$$ = $2;}

constrs1: constrs1 "|" constr   {$$ = $1; $$.push_back($3);}
|         constr                {$$.push_back($1);}

constr: forall context_no_ops "=>" constr_stuff {$$ = make_constructor($1,$2, $4);}
|       forall constr_stuff                     {$$ = make_constructor($1,{}, $2);}

forall: "forall" tv_bndrs "."   {$$ = $2;}
|       %empty                  {}

constr_stuff: btype_no_ops                      {$$ = Hs::type_apply($1);}
|             btype_no_ops conop btype_no_ops   {$$ = Hs::type_apply({
                                                                          {@2,Hs::TypeCon($2)},
                                                                          Hs::type_apply($1),
                                                                          Hs::type_apply($3)
                                                                       });}

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
| infixexp rhs                {$$ = {@$,Hs::ValueDecl({$1.loc,unloc($1)},$2)};}

decl: decl_no_th              {$$ = $1;}
/*  | splice_exp */

// rhs is like altrhs but with = instead of ->
rhs: "=" exp wherebinds       {$$ = Hs::SimpleRHS($2,$3);}
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

sigdecl: sig_vars "::" sigtypedoc { $$ = {@$,Hs::TypeSigDecl{$1,$3}}; }
|        infix prec ops  { $$ = {@$,Hs::FixityDecl{$1,$2,$3}}; }
/* |        pattern_synonym_sig {}  */
|        "{-# COMPLETE" con_list opt_tyconsig "#-}" {}
|        "{-# INLINE" activation qvar "#-}" { $$ = {@$, Hs::InlinePragma({@1,$1},{@3,$3})}; }
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

/* EP */
exp: infixexp "::" sigtype { $$ = {@1+@3, Hs::TypedExp({$1.loc,unloc($1)},$3)}; }
|    infixexp              { $$ = {$1.loc,unloc($1)}; }

/* EP */

infixexp: exp10                 {$$ = {@1,Hs::InfixExp({$1})};}
|         infixexp qop exp10    {$$ = $1; $$.loc = @1+@3; unloc($$).terms.push_back({@2,$2}); unloc($$).terms.push_back($3);}

exp10: PREFIX_MINUS fexp                {$$ = {@1+@2,Hs::InfixExp( { {@1,Hs::Neg()}, $2} )};}
|      fexp                    {$$ = $1;}

optSemi: ";"
|        %empty

/* hpc_annot */

/* EP */
fexp: fexp aexp                  {$$ = {@$,Hs::ApplyExp($1, $2)};}
|     fexp PREFIX_AT atype       {}
|     "static" aexp              {}
|     aexp                       {$$ = $1;}

/* EP */
aexp: qvar TIGHT_INFIX_AT aexp   {$$ = {@$, Hs::AsPattern({@1,Hs::Var($1)},$3)}; }
|     PREFIX_TILDE aexp          {$$ = {@$, Hs::LazyPattern($2)}; }
|     PREFIX_BANG  aexp          {$$ = {@$, Hs::StrictPattern($2)}; }
|     "\\" apats1 "->" exp       {$$ = {@$, Hs::LambdaExp($2,$4)}; }
|     "let" binds "in" exp       {$$ = {@$, Hs::LetExp($2,$4)}; }
/* |     "\\" "case" altslist       {} LambdaCase extension not currently handled */
|     "if" exp optSemi "then" exp optSemi "else" exp   {$$ = {@1+@8,Hs::IfExp($2,$5,$8)}; }
/* |     "if" ifgdpats              {} MultiWayIf extension not currently handled */
|     "case" exp "of" altslist   {$$ = {@$, Hs::CaseExp($2,$4)}; }
|     "do" stmtlist              {$$ = {@$, Hs::Do($2)}; }
|     "mdo" stmtlist             {$$ = {@$, Hs::MDo($2)}; }
/* |     "proc" aexp "->" exp       {} -XArrows not currently handled */
|     aexp1                      {$$ = $1;}

/* EP */
aexp1: aexp1 "{" fbinds "}"          { $$ = {@$,Hs::RecordExp{$1,$3}}; }
|      aexp1 TIGHT_INFIX_DOT field   { }
|      aexp2                         { $$ = $1; }

/* EP */
aexp2: qvar                   {$$ = {@$, Hs::Var($1)};}
|      qcon                   {$$ = {@$, Hs::Con($1)};}
|      literal                {$$ = {@$, $1};}
|      "(" texp ")"           {$$ = {@$, unloc($2)};}
|      "(" tup_exprs ")"      {$$ = {@$, Hs::Tuple($2)};}
|      "(" projection ")"     {}  // only possible with OverloadedRecordDot
/*
|      "(#" texp "#)"         {}
|      "(#" tup_exprs "#)"    {}
*/
|      "[" list "]"           {$$ = {@$, $2};}
|      "_"                    {$$ = {@$, Hs::WildcardPattern()};}
/* Skip Template Haskell Extensions */

projection: projection TIGHT_INFIX_DOT field
|           PREFIX_DOT field

/* ------------- Tuple expressions ------------------------------- */

/* EP */
texp: exp             {$$ = $1;}
|     infixexp qop    {$$ = {@$, Hs::LeftSection ( {$1.loc, unloc($1)}, {@2,$2} )}; }
|     qopm infixexp   {$$ = {@$, Hs::RightSection( {@1,$1}, {$2.loc,unloc($2)} )}; }
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

list: texp                       { $$ = Hs::List({$1}); }
|     lexps                      { $$ = Hs::List($1); }
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

alt_rhs: "->" exp wherebinds     {$$ = Hs::SimpleRHS($2,$3);}
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

bindpat: exp  {$$ = $1;}

apat: aexp    {$$ = $1;}

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
|     "rec" stmtlist    {$$ = {@$, Hs::RecStmt($2)};}

qual: bindpat "<-" exp  {$$ = {@$, Hs::PatQual($1,$3)};}
|     exp               {$$ = {@$, Hs::SimpleQual($1)};}
|     "let" binds       {$$ = {@$, Hs::LetQual($2)};}


/* ------------- Record Field Update/Construction ---------------- */

fbinds: fbinds1         {$$ = $1;}
|       %empty          {}

fbinds1: fbind "," fbinds1  {$$ = $3; unloc($$).insert(unloc($$).begin(), *$1); $$.loc = @$;}
|        fbind              {unloc($$).push_back(*$1); $$.loc = @$;}
|        ".."               {unloc($$).dotdot = true; $$.loc = @$;}

fbind: qvar "=" texp        {$$ = {{@$, Hs::FieldBinding({@$,Hs::Var($1)}, $3)}};}
|      qvar                 {$$ = {{@$, Hs::FieldBinding({@$,Hs::Var($1)})}};}
|      field TIGHT_INFIX_DOT fieldToUpdate "=" texp   {}
|      field TIGHT_INFIX_DOT fieldToUpdate            {}


fieldToUpdate : fieldToUpdate TIGHT_INFIX_DOT field
|               field

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

con_list: con_list "," con  { $$ = $1; $$.push_back({@3,$3});}
|         con               { $$.push_back({@1,$1});}

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

oqtycon_no_varcon: qtycon  { $$ = $1; }
|        "(" QCONSYM ")"   { $$ = $2; }
|        "(" CONSYM  ")"   { $$ = $2; }
|        "(" ":"  ")"      { $$ = ":"; }


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

qop:  qvarop    { $$ = Hs::Var($1); }
|     qconop    { $$ = Hs::Con($1); }
/* |     hole_op   { $$ = $1; } */

qopm: qvaropm   { $$ = Hs::Var($1); }
|     qconop    { $$ = Hs::Con($1); }
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

field: varid

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

special_sym: "." { $$ = "."; }
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
|        RATIONAL    {$$ = Hs::Literal(Hs::Floating{$1});}
|        PRIMINTEGER {$$ = Hs::Literal(Hs::BoxedInteger{$1});}


/* ------------- Layout ------------------------------------------ */

close: VCCURLY |
       /* Without the yyerrok, the yyerror seems not to be called at the end of the file, 
          so that the drv.pop_error_message() causes a SEGFAULT. */
error { yyerrok; drv.pop_error_message(); drv.pop_context();}

/* ------------- Miscellaneous (mostly renamings) ---------------- */

modid: CONID  {$$ = {@$,$1};}
|      QCONID {$$ = {@$,$1};}

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
    drv.push_error_message(l,m);
}

pair<vector<Hs::LImpDecl>, optional<Hs::Decls>> make_body(const std::vector<Hs::LImpDecl>& imports, const std::optional<Hs::Decls>& topdecls)
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
std::tuple<Located<Hs::TypeCon>, vector<Hs::LType>>
check_type_or_class_header2(const Hs::LType& type)
{
    auto [type_head, type_args] = Hs::decompose_type_apps(type);

    // FIXME -- add location!
    auto tc = unloc(type_head).to<Hs::TypeCon>();

    // Convert to error message!
    if (not tc)
        throw myexception()<<"Malformed type or class header '"<<type<<"'";

    auto name = tc->name;

    return {{type_head.loc, *tc}, type_args};
}

vector<Hs::LTypeVar> check_all_type_vars(const vector<Hs::LType>& ltypes)
{
    vector<Hs::LTypeVar> ltype_vars;
    for(auto& ltype: ltypes)
    {
        auto& [loc,type] = ltype;
        if (auto tv = type.to<Hs::TypeVar>())
        {
            ltype_vars.push_back({loc,*tv});
        }
        else
        {
            throw myexception()<<"Type '"<<type<<"' is not a type variable";
        }
    }
    return ltype_vars;
}

Hs::TypeSynonymDecl make_type_synonym(const Hs::LType& lhs_type, const Hs::LType& rhs_type)
{
    auto [con, type_args] = check_type_or_class_header2(lhs_type);
    return {con, check_all_type_vars(type_args), rhs_type};
}

Hs::FamilyDecl make_family_decl(Hs::FamilyInfo info, const Hs::LType& lhs_type, const std::optional<Located<Hs::Kind>>& kind_sig,
				const std::optional<std::vector<Hs::TypeFamilyInstanceEqn>>& eqns)
{
    auto [lhead, largs] = Hs::decompose_type_apps(lhs_type);

    // Get type con
    auto con = unloc(lhead).to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family '"<<lhs_type.print()<<"' does not begin with a type constructor.";
    auto lcon = Hs::LTypeCon{lhead.loc, *con};
    
    // Get args as type vars
    std::vector<Hs::LTypeVar> tyvars;
    for(auto larg: largs)
    {
        std::optional<Hs::Kind> kind;
        if (auto ktype = unloc(larg).to<Hs::TypeOfKind>())
        {
            larg = ktype->type;
            kind = ktype->kind;
        }

        if (auto tyvar = unloc(larg).to<Hs::TypeVar>())
        {
            auto tv = Hs::LTypeVar{larg.loc, *tyvar};
            unloc(tv).kind = kind;
            tyvars.push_back(tv);
        }
        else
            throw myexception()<<"Type family '"<<lhs_type.print()<<"' argument '"<<larg.print()<<"' is not a type variable.";
    }

    std::optional<Hs::Kind> kind;
    if (kind_sig)
        kind = kind_sig->value();

    return Hs::FamilyDecl(info, lcon, tyvars, kind, eqns);
}

Hs::TypeFamilyInstanceEqn make_type_family_instance_eqn(const Hs::LType& lhs_type, const Hs::LType& rhs_type)
{
    auto [head, args] = Hs::decompose_type_apps(lhs_type);

    // Get type con
    auto con = unloc(head).to<Hs::TypeCon>();
    if (not con)
        throw myexception()<<"Type family instance '"<<lhs_type.print()<<"' does not begin with a type constructor.";
    
    return Hs::TypeFamilyInstanceEqn({head.loc, *con}, args, rhs_type);
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k,
                                           const Hs::ConstructorsDecl& constrs)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    if (d_or_n == Hs::DataOrNewtype::newtype and constrs.size() != 1)
        throw myexception()<<"newtype '"<<con<<"' may only have 1 constructors with 1 field";
    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs)};
}

Hs::DataOrNewtypeDecl make_data_or_newtype(const Hs::DataOrNewtype& d_or_n, const Hs::Context&  context,
                                           const Hs::LType& header, const std::optional<Hs::Kind>& k, const Hs::GADTConstructorsDecl& constrs)
{
    auto [con, type_args] = check_type_or_class_header2(header);
    if (d_or_n == Hs::DataOrNewtype::newtype)
    {
        if (constrs.size() != 1 or constrs[0].con_names.size() != 1)
            throw myexception()<<"newtype '"<<con<<"' may only have 1 constructors with 1 field";
    }

    return {con, check_all_type_vars(type_args), Hs::DataDefn(d_or_n, context, k, constrs)};
}

Hs::InstanceDecl make_instance_decl(const std::optional<std::string>& oprag, const Hs::LType& polytype, const optional<Located<Hs::Decls>>& decls)
{
    std::vector<Hs::TypeFamilyInstanceDecl> type_inst_decls;
    Hs::Decls method_decls;
    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                type_inst_decls.push_back(*TI);
            else if (auto V = decl.to<Hs::ValueDecl>())
                method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of instance "<<unloc(polytype).print()<<", I don't recognize declaration:\n   "<<decl.print();
        }
    return {oprag, polytype, type_inst_decls, method_decls};
}

Hs::ClassDecl make_class_decl(const Hs::Context& context, const Hs::LType& header,
                              const std::vector<Hs::FunDep>& fds, const optional<Located<Hs::Decls>>& decls)
{
    auto [con, type_args] = check_type_or_class_header2(header);

    std::vector<Hs::FixityDecl> fixity_decls;
    std::vector<Hs::FamilyDecl> fam_decls;
    std::vector<Hs::TypeFamilyInstanceDecl> default_type_inst_decls;
    std::vector<Hs::TypeSigDecl> sig_decls;
    Hs::Decls default_method_decls;

    if (decls)
        for(auto& [loc,decl]: unloc(*decls))
        {
            if (auto F = decl.to<Hs::FixityDecl>())
                fixity_decls.push_back(*F);
            else if (auto TF = decl.to<Hs::FamilyDecl>())
                fam_decls.push_back(*TF);
            else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
                default_type_inst_decls.push_back(*TI);
            else if (auto S = decl.to<Hs::TypeSigDecl>())
                sig_decls.push_back(*S);
            else if (auto V = decl.to<Hs::ValueDecl>())
                default_method_decls.push_back({loc,*V});
            else
                throw myexception()<<"In declaration of class "<<con<<", I don't recognize declaration:\n   "<<decl.print();
        }

    return {context, con, check_all_type_vars(type_args), fixity_decls, fds, fam_decls, default_type_inst_decls, sig_decls, default_method_decls};
}

// Can we change the context parsing rule to expect:
// nothing
// | ctype => header
// | ( ctypes2 ) => header
Hs::Context make_context(const Hs::LType& context)
{
    vector<Hs::LType> constraints;
    if (auto tuple = unloc(context).to<Hs::TupleType>())
        constraints = tuple->element_types;
    else
        constraints.push_back(context);

    return {constraints};
}

std::optional<Hs::Kind> type_to_kind_(const Hs::LType& kind)
{
    auto [kind_head, kind_args] = Hs::decompose_type_apps(kind);

    if (not unloc(kind_head).is_a<Hs::TypeCon>()) return {};
    auto V = unloc(kind_head).as_<Hs::TypeCon>();
    auto head_name = V.name;

    if (kind_args.empty())
    {
        if (head_name == "*" or head_name == "Type")
            return kind_type();
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

Hs::Kind type_to_kind(const Hs::LType& kind)
{
    auto maybe_kind = type_to_kind_(kind);

    if (not maybe_kind)
        throw myexception()<<"Kind '"<<kind<<"' is malformed";

    return *maybe_kind;
}

Hs::ConstructorDecl make_constructor(const vector<Hs::LTypeVar>& qtvs, const Hs::Context& context, const Hs::LType& typeish)
{
    // 1. Split into head and arguments
    auto [head,args] = Hs::decompose_type_apps(typeish);

    // 2. Get the constructor name.
    auto tc = unloc(head).to<Hs::TypeCon>();
    if (not tc)
        throw myexception()<<"In constructor `"<<typeish<<"`:\n    `"<<head<<"` is not a data constructor!";
    auto con = Hs::LCon{head.loc, Hs::Con(tc->name)};

    // 3. If we have 1 arg and its a FieldDecls, then make a record constructor.
    if (args.size() == 1)
    {
        if (auto fd = unloc(args[0]).to<Hs::FieldDecls>())
        {
            return {qtvs, context, con, *fd};
        }
    }

    // 4. Otherwise make a normal constructor.
    return {qtvs, context, con, args};
}
