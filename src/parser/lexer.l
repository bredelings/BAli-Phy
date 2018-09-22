%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>
# include "driver.hh"
# include "parser.hh"
# include "util.H"

// Work around an incompatibility in flex (at least versions
// 2.5.31 through 2.5.33): it generates code that does
// not conform to C89.  See Debian bug 333231
// <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.
# undef yywrap
# define yywrap() 1

// Pacify warnings in yy_init_buffer (observed with Flex 2.6.4)
// and GCC 7.3.0.
#if defined __GNUC__ && 7 <= __GNUC__
# pragma GCC diagnostic ignored "-Wnull-dereference"
#endif

// Forward declarations
yy::parser::symbol_type varid(const yy::parser::location_type&);
yy::parser::symbol_type varsym(const yy::parser::location_type&);
yy::parser::symbol_type consym(const yy::parser::location_type&);

yy::parser::symbol_type make_integer(int radix, bool positive, int shift,
				     const yy::parser::location_type& loc);

yy::parser::symbol_type make_rational(const yy::parser::location_type& loc);

yy::parser::symbol_type make_char(const yy::parser::location_type& loc);
yy::parser::symbol_type make_string(const yy::parser::location_type& loc);

%}

%option noyywrap nounput batch debug noinput stack
%x layout layout_do layout_if bol

unispace     \x05
nl           [\n\r\f]
white_no_nl  [\v\ ]|{unispace}
whitechar    {white_no_nl}|{nl}
tab          \t

ascdigit     [0-9]
unidigit     \x03
decdigit     {ascdigit}
digit        {ascdigit}|{unidigit}

special      [(),;[\]`\{\}]
ascsymbol_except_colon    [!#$%&\*+./<=>?@\\^|\-~]
ascsymbol    {ascsymbol_except_colon}|:
unisymbol    \x05
symbol_except_colon {ascsymbol_except_colon}
symbol       {ascsymbol}

unilarge     \x01
asclarge     [A-Z]
large        {asclarge}|{unilarge}

unismall     \x02
ascsmall     [a-z]
small        {ascsmall}|{unismall}|\_

unigraphic   \x06
graphic      {small}|{large}|{symbol}|{digit}|{special}|{unigraphic}|\"\'
graphicnonq  {small}|{large}|{symbol}|{digit}|{special}|{unigraphic}|\"
graphicnonqq {small}|{large}|{symbol}|{digit}|{special}|{unigraphic}|\'

binit        [0-1]
octit        [0-7]
hexit        {decdigit}|[A-Fa-f]

uniidchar    \x07
idchar       {small}|{large}|{digit}|{uniidchar}|\'

pragmachar   {small}|{large}|{digit}

docsym       [|\^*$]

varid        {small}{idchar}*
conid        {large}{idchar}*

varsym       {symbol_except_colon}{symbol}*
consym       :{symbol}*

numspc       _*
decimal      {decdigit}({numspc}{decdigit})*
binary       {binit}({numspc}{binit})*
octal        {octit}({numspc}{octit})*
hexadecimal  {hexit}({numspc}{hexit})*
exponent     {numspc}[eE][\-\+]?{decimal}
bin_exponent {numspc}[pP][\-\+]?{decimal}
escape	     \\[abfnrtv\"\']

qual         ({conid}\.)+
qvarid       {qual}{varid}
qconid       {qual}{conid}
qvarsym      {qual}{varsym}
qconsym      {qual}{consym}

floating_point      {numspc}{decimal}\.{decimal}{exponent}?|{numspc}{decimal}{exponent}
hex_floating_point  {numspc}{hexadecimal}\.{hexadecimal}{exponent}?|{numspc}{hexadecimal}{binexponent}

negative     \-
signed       {negative}?


%{
  // Code run each time a pattern is matched.
  # define YY_USER_ACTION  loc.columns (yyleng);
%}

%%

%{
  // A handy shortcut to the location held by the driver.
  yy::location& loc = drv.location;
  // Code run each time yylex is called.
  loc.step ();
%}

{white_no_nl}+   loc.step ();
{tab}	         loc.step ();

  /* GHC calls some handwritten code to deal with nested comments */
"{-[^-\n]*-}"      loc.step();

"--"-*[^!#$%&*+./<=>?@\\^|\-~:\n].*           loc.step();
"--"-*$           loc.step();
"-- "$            loc.step();

  /* bol state */
[\n]+      loc.lines (yyleng); loc.step ();

  /* layout, layout_do, layout_if states */

  /* layout_if */

  /* layout */
  /* layout_do */

  /* layout_left */

  /* lots of stuff */

  /* unboxed tuples: (# and #) */

"("      return yy::parser::make_OPAREN (loc);
")"      return yy::parser::make_CPAREN (loc);
"["      return yy::parser::make_OBRACK (loc);
"]"      return yy::parser::make_CBRACK (loc);
","      return yy::parser::make_COMMA  (loc);
";"      return yy::parser::make_SEMI   (loc);
"`"      return yy::parser::make_BACKQUOTE (loc);

  /* action: push a "NoLayout" context into the stack */
"{"      return yy::parser::make_OCURLY (loc);
  /* action: pop the top context from the stack */
"}"      return yy::parser::make_CCURLY (loc);

{qvarid}      return yy::parser::make_QVARID (yytext, loc);
{qconid}      return yy::parser::make_QCONID (yytext, loc);
{varid}       return varid(loc);
{conid}       return yy::parser::make_CONID  (yytext, loc);


  /* Here we look for {qvarid}#+ ... {conid}#+ if magicHashEnabled */

{qvarsym}      return yy::parser::make_QVARSYM  (yytext,loc);
{qconsym}      return yy::parser::make_QCONSYM  (yytext,loc);
{varsym}       return varsym(loc);
{consym}       return consym(loc);

{decimal}                              return make_integer(10,true,0,loc);
  /* 0[bB]{numspc}{binary}      make_integer(2,true,2,loc); */
0[oO]{numspc}{octal}                   return make_integer( 8,true,2,loc);
0[xX]{numspc}{hexadecimal}             return make_integer(16,true,2,loc);

  /* negative literals depend on an extension */
{floating_point}                       return make_rational(loc);

\'({graphicnonq}|" "|{escape})\'         return make_char(loc);
[\"]({graphicnonqq}|" "|{escape})*[\"]   return make_string(loc);

.          {
             throw yy::parser::syntax_error
               (loc, "invalid character: " + std::string(yytext));
}

<<EOF>>    return yy::parser::make_END (loc);
%%



void
driver::scan_begin ()
{
  yy_flex_debug = trace_scanning;
  if (file.empty () || file == "-")
    yyin = stdin;
  else if (!(yyin = fopen (file.c_str (), "r")))
    {
      std::cerr << "cannot open " << file << ": " << strerror(errno) << '\n';
      exit (EXIT_FAILURE);
    }
}


void
driver::scan_end ()
{
  fclose (yyin);
}

using namespace yy;

std::map<std::string,std::pair<yy::parser::token_type,int>> reserved_words =
{
    {"_",{parser::token::TOK_UNDERSCORE,0}},
    {"as",{parser::token::TOK_AS,0}},
    {"case",{parser::token::TOK_CASE,0}},
    {"data",{parser::token::TOK_DATA,0}},
    {"default",{parser::token::TOK_DEFAULT,0}},
    {"deriving",{parser::token::TOK_DERIVING,0}},
    {"do",{parser::token::TOK_DO,0}},
    {"else",{parser::token::TOK_ELSE,0}},
    {"hiding",{parser::token::TOK_HIDING,0}},
    {"if",{parser::token::TOK_IF,0}},
    {"import",{parser::token::TOK_IMPORT,0}},
    {"in",{parser::token::TOK_IN,0}},
    {"infix",{parser::token::TOK_INFIX,0}},
    {"infixl",{parser::token::TOK_INFIXL,0}},
    {"infixr",{parser::token::TOK_INFIXR,0}},
    {"instance",{parser::token::TOK_INSTANCE,0}},
    {"let",{parser::token::TOK_LET,0}},
    {"module",{parser::token::TOK_MODULE,0}},
    {"newtype",{parser::token::TOK_NEWTYPE,0}},
    {"of",{parser::token::TOK_OF,0}},
    {"qualified",{parser::token::TOK_QUALIFIED,0}},
    {"then",{parser::token::TOK_THEN,0}},
    {"type",{parser::token::TOK_TYPE,0}},
    {"where",{parser::token::TOK_WHERE,0}},
    {"builtin",{parser::token::TOK_BUILTIN,0}}
};
/*

         ( "as",             ITas,            0 ),
         ( "case",           ITcase,          0 ),
         ( "class",          ITclass,         0 ),
         ( "data",           ITdata,          0 ),
         ( "default",        ITdefault,       0 ),
         ( "deriving",       ITderiving,      0 ),
         ( "do",             ITdo,            0 ),
         ( "else",           ITelse,          0 ),
         ( "hiding",         IThiding,        0 ),
         ( "if",             ITif,            0 ),
         ( "import",         ITimport,        0 ),
         ( "in",             ITin,            0 ),
         ( "infix",          ITinfix,         0 ),
         ( "infixl",         ITinfixl,        0 ),
         ( "infixr",         ITinfixr,        0 ),
         ( "instance",       ITinstance,      0 ),
         ( "let",            ITlet,           0 ),
         ( "module",         ITmodule,        0 ),
         ( "newtype",        ITnewtype,       0 ),
         ( "of",             ITof,            0 ),
         ( "qualified",      ITqualified,     0 ),
         ( "then",           ITthen,          0 ),
         ( "type",           ITtype,          0 ),
         ( "where",          ITwhere,         0 ),
         ( "forall",         ITforall NormalSyntax,
                                              xbit ExplicitForallBit .|.
                                              xbit InRulePragBit),
         ( "mdo",            ITmdo,           xbit RecursiveDoBit),
             -- See Note [Lexing type pseudo-keywords]
         ( "family",         ITfamily,        0 ),
         ( "role",           ITrole,          0 ),
         ( "pattern",        ITpattern,       xbit PatternSynonymsBit),
         ( "static",         ITstatic,        0 ),
         ( "stock",          ITstock,         0 ),
         ( "anyclass",       ITanyclass,      0 ),
         ( "via",            ITvia,           0 ),
         ( "group",          ITgroup,         xbit TransformComprehensionsBit),
         ( "by",             ITby,            xbit TransformComprehensionsBit),
         ( "using",          ITusing,         xbit TransformComprehensionsBit),
         ( "foreign",        ITforeign,       xbit FfiBit),
         ( "export",         ITexport,        xbit FfiBit),
         ( "label",          ITlabel,         xbit FfiBit),
         ( "dynamic",        ITdynamic,       xbit FfiBit),
         ( "safe",           ITsafe,          xbit FfiBit .|.
                                              xbit SafeHaskellBit),
         ( "interruptible",  ITinterruptible, xbit InterruptibleFfiBit),
         ( "unsafe",         ITunsafe,        xbit FfiBit),
         ( "stdcall",        ITstdcallconv,   xbit FfiBit),
         ( "ccall",          ITccallconv,     xbit FfiBit),
         ( "capi",           ITcapiconv,      xbit CApiFfiBit),
         ( "prim",           ITprimcallconv,  xbit FfiBit),
         ( "javascript",     ITjavascriptcallconv, xbit FfiBit),
         ( "unit",           ITunit,          0 ),
         ( "dependency",     ITdependency,       0 ),
         ( "signature",      ITsignature,     0 ),
         ( "rec",            ITrec,           xbit ArrowsBit .|.
                                              xbit RecursiveDoBit),
         ( "proc",           ITproc,          xbit ArrowsBit)
     ]
*/

yy::parser::symbol_type varid(const yy::parser::location_type& loc)
{
    auto it = reserved_words.find(yytext);
    if (it == reserved_words.end())
	return yy::parser::make_VARID  (yytext, loc);
    else
    {
	auto tok = it->second.first;
	return yy::parser::symbol_type(tok, loc);
    }
}

std::map<std::string,std::pair<yy::parser::token_type,int>> reserved_symbols =
{
    {"..",{parser::token::TOK_DOTDOT,0}},
    {":",{parser::token::TOK_COLON,0}},
    {"::",{parser::token::TOK_DCOLON,0}},
    {"=",{parser::token::TOK_EQUAL,0}},
    {"\\",{parser::token::TOK_LAM,0}},
    {"|",{parser::token::TOK_VBAR,0}},
    {"<-",{parser::token::TOK_LARROW,0}},
    {"->",{parser::token::TOK_RARROW,0}},
    {"@",{parser::token::TOK_AT,0}},
    {"~",{parser::token::TOK_TILDE,0}},
    {"=>",{parser::token::TOK_DARROW,0}},
    {"-",{parser::token::TOK_MINUS,0}},
    {"!",{parser::token::TOK_BANG,0}},
    {"*",{parser::token::TOK_STAR,0}}
};

/*
   map (\ (x,y,z) -> (mkFastString x,(y,z)))
      [ ("..",  ITdotdot,              always)
        -- (:) is a reserved op, meaning only list cons
       ,(":",   ITcolon,               always)
       ,("::",  ITdcolon NormalSyntax, always)
       ,("=",   ITequal,               always)
       ,("\\",  ITlam,                 always)
       ,("|",   ITvbar,                always)
       ,("<-",  ITlarrow NormalSyntax, always)
       ,("->",  ITrarrow NormalSyntax, always)
       ,("@",   ITat,                  always)
       ,("~",   ITtilde,               always)
       ,("=>",  ITdarrow NormalSyntax, always)
       ,("-",   ITminus,               always)
       ,("!",   ITbang,                always)
       ,("*", ITstar NormalSyntax, starIsTypeEnabled)
        -- For 'forall a . t'
       ,(".", ITdot,  always) -- \i -> explicitForallEnabled i || inRulePrag i)
       ,("-<",  ITlarrowtail NormalSyntax, arrowsEnabled)
       ,(">-",  ITrarrowtail NormalSyntax, arrowsEnabled)
       ,("-<<", ITLarrowtail NormalSyntax, arrowsEnabled)
       ,(">>-", ITRarrowtail NormalSyntax, arrowsEnabled)
       ,("∷",   ITdcolon UnicodeSyntax, unicodeSyntaxEnabled)
       ,("⇒",   ITdarrow UnicodeSyntax, unicodeSyntaxEnabled)
       ,("∀",   ITforall UnicodeSyntax, unicodeSyntaxEnabled)
       ,("→",   ITrarrow UnicodeSyntax, unicodeSyntaxEnabled)
       ,("←",   ITlarrow UnicodeSyntax, unicodeSyntaxEnabled)
       ,("⤙",   ITlarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤚",   ITrarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤛",   ITLarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤜",   ITRarrowtail UnicodeSyntax,
                                \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("★",   ITstar UnicodeSyntax,
                  \i -> unicodeSyntaxEnabled i && starIsTypeEnabled i)
        -- ToDo: ideally, → and ∷ should be "specials", so that they cannot
        -- form part of a large operator.  This would let us have a better
        -- syntax for kinds: ɑ∷*→* would be a legal kind signature. (maybe).
       ]

*/

yy::parser::symbol_type varsym(const yy::parser::location_type& loc)
{
    auto it = reserved_symbols.find(yytext);
    if (it == reserved_symbols.end())
	return yy::parser::make_VARSYM  (yytext, loc);
    else
    {
	auto tok = it->second.first;
	return yy::parser::symbol_type(tok, loc);
    }
}

yy::parser::symbol_type consym(const yy::parser::location_type& loc)
{
    auto it = reserved_symbols.find(yytext);
    if (it == reserved_symbols.end())
	return yy::parser::make_CONSYM  (yytext, loc);
    else
    {
	auto tok = it->second.first;
	return yy::parser::symbol_type(tok, loc);
    }
}

yy::parser::symbol_type make_integer(int radix, bool positive, int shift,
				     const yy::parser::location_type& loc)
{
    errno = 0;
    auto n = std::stoi(yytext+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw yy::parser::syntax_error(loc, "integer is out of range: " + std::string(yytext));
    if (not positive)
	n = -n;
    return yy::parser::make_INTEGER(n, loc);
}

yy::parser::symbol_type make_rational(const yy::parser::location_type& loc)
{
    std::string s(yytext);
    double d = convertTo<double>(std::string(yytext));
    return yy::parser::make_RATIONAL(d,loc);
}

char get_char(const char* s, int& i)
{
    char c = s[i];
    if (c == '\\')
    {
	i++;
	c = s[i];
	if (c == 'a')
	    c = '\a';
	else if (c == 'b')
	    c = '\b';
	else if (c == 'f')
	    c = '\f';
	else if (c == 'n')
	    c = '\n';
	else if (c == 'r')
	    c = '\r';
	else if (c == 't')
	    c = '\t';
	else if (c == 'v')
	    c = '\v';
	else if (c == '"')
	    ;
	else if (c == '\'')
	    ;
	else
	    std::abort();
    }
    return c;
}

yy::parser::symbol_type make_char(const yy::parser::location_type& loc)
{
    int i=1;
    char c = get_char(yytext, i);
    return yy::parser::make_CHAR(c,loc);
}

yy::parser::symbol_type make_string(const yy::parser::location_type& loc)
{
    std::string s;
    s.resize(yyleng);
    int j=0;
    for(int i=1;i<yyleng-1;i++)
	s[j++] = get_char(yytext,i);
    s.resize(j);
    return yy::parser::make_STRING(s,loc);
}
