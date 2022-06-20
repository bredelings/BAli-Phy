%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>
# include "driver.hh"
# include "parser.hh"

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

yy::parser::symbol_type make_boxed_integer(int radix, bool positive, int shift,
				           const yy::parser::location_type& loc);

yy::parser::symbol_type make_rational(const yy::parser::location_type& loc);

yy::parser::symbol_type make_char(const yy::parser::location_type& loc);
yy::parser::symbol_type make_string(const yy::parser::location_type& loc);

%}

%option params="driver& drv"
%option noyywrap nounput batch debug noinput stack never-interactive nounistd
%s layout layout_do layout_if layout_left BOL nested_comment nested_comment2
%s option_prags line_prag1a line_prag2 line_prag2a

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
small        {ascsmall}|{unismall}|_

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
hex_floating_point  {numspc}{hexadecimal}\.{hexadecimal}{bin_exponent}?|{numspc}{hexadecimal}{bin_exponent}

negative     \-
signed       {negative}?


%{
  // Code run each time a pattern is matched.
  # undef YY_USER_ACTION
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
  /* "{-"[^\n-#]*"-}"      loc.step(); */
<nested_comment,nested_comment2>{
"{-"/[^#]                    yy_push_state(nested_comment2);
[^-\n]*      // each a chunk with no newline or "-"
"-"          // eat a "-" not followed by "}"
[\n]         loc.lines(1);
<<EOF>>      throw yy::parser::syntax_error(loc, "Unterminated {-");
}
"{-"/[^#]                    yy_push_state(nested_comment);

<nested_comment2>"-}"        yy_pop_state();
<nested_comment>"-}"         loc.step(); yy_pop_state();
"-}"         throw yy::parser::syntax_error(loc, "Too many -}");




"--"-*[^!#$%&*+./<=>?@\\^|\-~:\n].*           loc.step();
"--"-*$           loc.step();
"-- "$            loc.step();

  /* bol state */
<BOL>[\n]  loc.lines(1); loc.step();
<BOL>.     yyless(0); loc.columns(-1); if (auto tok = drv.do_bol(loc)) return *tok;
<BOL><<EOF>>  if (auto tok = drv.do_bol(loc)) return *tok;

<layout,layout_do,layout_if>"{"/[^-]    return drv.hopefully_open_brace(loc);
<layout,layout_do,layout_if>[\n]        loc.lines(1); loc.step();

<layout_if>"|"/[^!#$%&*+./<=>?@\\^|\-~:\n]  drv.hopefully_open_brace(loc);
<layout_if>.                                yyless(0); loc.columns(-1); yy_pop_state();

<layout>.                                 yyless(0); loc.columns(-1); return drv.new_layout_context(loc, true, true, yy::parser::token::TOK_VOCURLY);
<layout_do>.                              yyless(0); loc.columns(-1); return drv.new_layout_context(loc, false, true, yy::parser::token::TOK_VOCURLY);

  /* If we've generated a VOCURLY and started a new line to the left, then
     we need to generate a VCCURLY */
<layout_left>.                          yyless(0); return drv.do_layout_left(loc);

<INITIAL,option_prags>[\n]              loc.lines(1); loc.step (); yy_push_state(BOL);

  /* known pragmas */
"{-#"{whitechar}*{pragmachar}+

  /* <line_prag1> single-line line pragmas: # <line> "<file>" <extra-stuff> \n */

  /* <line_prag2> Haskell-style line pragms, of the form
     {-# LINE <line> "<file>" #-} */

  /* <line_prag2a> */

  /* <column_prag> */

  /* two-word pragmas */
<INITIAL,option_prags>"{-#"{whitechar}*{pragmachar}+{whitechar}+{pragmachar}+  
  /* one-word pragmas */
<INITIAL,option_prags>"{-"${whitechar}*{pragmachar}+
  /* end pragma */
<INITIAL,option_prags>"#-}"

  /* unboxed tuples: (# and #) */

<INITIAL,option_prags>"("      return yy::parser::make_OPAREN (loc);
<INITIAL,option_prags>")"      return yy::parser::make_CPAREN (loc);
<INITIAL,option_prags>"["      return yy::parser::make_OBRACK (loc);
<INITIAL,option_prags>"]"      return yy::parser::make_CBRACK (loc);
<INITIAL,option_prags>","      return yy::parser::make_COMMA  (loc);
<INITIAL,option_prags>";"      return yy::parser::make_SEMI   (loc);
<INITIAL,option_prags>"`"      return yy::parser::make_BACKQUOTE (loc);

  /* action: push a "NoLayout" context into the stack */
"{"      drv.push_context(); return yy::parser::make_OCURLY (loc);
  /* action: pop the top context from the stack */
"}"      drv.pop_context(); return yy::parser::make_CCURLY (loc);

<INITIAL,option_prags>{qvarid}      return yy::parser::make_QVARID (yytext, loc);
<INITIAL,option_prags>{qconid}      return yy::parser::make_QCONID (yytext, loc);
<INITIAL,option_prags>{varid}       return varid(loc);
<INITIAL,option_prags>{conid}       return yy::parser::make_CONID  (yytext, loc);


  /* Here we look for {qvarid}#+ ... {conid}#+ if magicHashEnabled */

<INITIAL>{qvarsym}      return yy::parser::make_QVARSYM  (yytext,loc);
<INITIAL>{qconsym}      return yy::parser::make_QCONSYM  (yytext,loc);
<INITIAL>{varsym}       return varsym(loc);
<INITIAL>{consym}       return consym(loc);

<INITIAL>"-"{decimal}"#"                        return make_boxed_integer(10,false,0,loc);
<INITIAL>{decimal}"#"                           return make_boxed_integer(10,true,0,loc);
<INITIAL>{decimal}                              return make_integer(10,true,0,loc);
  /* 0[bB]{numspc}{binary}      make_integer(2,true,2,loc); */
<INITIAL>0[oO]{numspc}{octal}                   return make_integer( 8,true,2,loc);
<INITIAL>0[xX]{numspc}{hexadecimal}             return make_integer(16,true,2,loc);

  /* negative literals depend on an extension */
<INITIAL>{floating_point}                       return make_rational(loc);

<INITIAL>\'({graphicnonq}|" "|{escape})\'         return make_char(loc);
<INITIAL>[\"]({graphicnonqq}|" "|{escape})*[\"]   return make_string(loc);

<*>.          {
             throw yy::parser::syntax_error
               (loc, "invalid character: " + std::string(yytext));
}

<<EOF>>    return yy::parser::make_END (loc);
%%



void
driver::scan_begin (const std::string& content)
{
  yy_flex_debug = trace_scanning;
  yy_scan_string(content.c_str());
}


void
driver::scan_end ()
{
  yy_delete_buffer(YY_CURRENT_BUFFER);
}

using namespace yy;

std::map<std::string,std::pair<yy::parser::token_type,int>> reserved_words =
{
    {"_",{parser::token::TOK_UNDERSCORE,0}},
    {"as",{parser::token::TOK_AS,0}},
    {"bpcall",{parser::token::TOK_BPCALL,0}},
    {"case",{parser::token::TOK_CASE,0}},
    {"class",{parser::token::TOK_CLASS,0}},
    {"data",{parser::token::TOK_DATA,0}},
    {"default",{parser::token::TOK_DEFAULT,0}},
    {"deriving",{parser::token::TOK_DERIVING,0}},
    {"do",{parser::token::TOK_DO,0}},
    {"else",{parser::token::TOK_ELSE,0}},
    {"forall",{parser::token::TOK_FORALL,0}},
    {"foreign",{parser::token::TOK_FOREIGN,0}},
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
    {"mdo", {parser::token::TOK_MDO,0}},
    {"rec", {parser::token::TOK_REC,0}}
};


yy::parser::symbol_type varid(const yy::parser::location_type& loc)
{
    auto it = reserved_words.find(yytext);
    if (it == reserved_words.end())
	return yy::parser::make_VARID  (yytext, loc);
    else
    {
	auto tok = it->second.first;
	switch(tok)
	{
	case parser::token::TOK_DO:
	case parser::token::TOK_MDO:
//	    std::cerr<<"switch to <layout_do> @ "<<loc<<"\n";
	    yy_push_state(layout_do);
	    break;
	case parser::token::TOK_OF:
	case parser::token::TOK_LCASE:
	case parser::token::TOK_LET:
	case parser::token::TOK_WHERE:
	case parser::token::TOK_REC:
//	    std::cerr<<"switch to <layout> @ "<<loc<<"\n";
	    yy_push_state(layout);
	    break;
	case parser::token::TOK_IF:
//	    std::cerr<<"switch to <layout_do> @ "<<loc<<"\n";
	    yy_push_state(layout_if);
	    break;
	default:
	    break;
	}
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
    {".",{parser::token::TOK_DOT,0}},
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

yy::parser::symbol_type make_boxed_integer(int radix, bool positive, int shift,
                                           const yy::parser::location_type& loc)
{
    errno = 0;
    auto n = std::stoi(yytext+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw yy::parser::syntax_error(loc, "integer is out of range: " + std::string(yytext));
    if (not positive)
	n = -n;
    return yy::parser::make_PRIMINTEGER(n, loc);
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

std::optional<yy::parser::symbol_type> driver::do_bol(const yy::parser::location_type& loc)
{
    auto x = get_offside(loc);
    int delta_offset = x.offset;
    bool gen_semis = x.gen_semis;
//    std::cerr<<"do_bol @ "<<loc<<std::endl;
    if (delta_offset < 0)
    {
//	std::cerr<<"offset = "<<loc.end.column<<", LT, generating '}'"<<std::endl;
	pop_context();
	return yy::parser::make_VCCURLY(loc);
    }
    else if (delta_offset == 0 and gen_semis)
    {
//	std::cerr<<"offset = "<<loc.end.column<<", EQ, generating ';'"<<std::endl;
	yy_pop_state();
	return yy::parser::make_SEMI(loc);
    }
    else
    {
	yy_pop_state();
	return {};
    }
}

yy::parser::symbol_type driver::new_layout_context(const yy::parser::location_type& loc, bool strict, bool gen_semis, yy::parser::token_type tok)
{
    yy_pop_state();
    int offset = loc.end.column;
//    std::cerr<<"Opening new layout context at "<<loc<<std::endl;
    if (auto layout_context = get_context())
    {
	int prev_offset = layout_context->offset;
	if ((strict and prev_offset > offset) or
	    ((not strict) and prev_offset >= offset))
	{
//	    std::cerr<<"   - Going to layout_left"<<std::endl;
	    yy_push_state(layout_left);
	    return yy::parser::symbol_type(tok,loc);
	}
    }

    push_context({offset,gen_semis});
    return yy::parser::symbol_type(tok,loc);
}

yy::parser::symbol_type driver::do_layout_left(const yy::parser::location_type& loc)
{
//    std::cerr<<"Doing layout_left at "<<loc<<std::endl;
    yy_pop_state();
    yy_push_state(BOL);
    return yy::parser::make_VCCURLY(loc);
}

yy::parser::symbol_type driver::hopefully_open_brace(const yy::parser::location_type& loc)
{
    if (auto layout_context = get_context())
    {
	int offset = loc.end.column;
	if (layout_context->offset >= offset)
	    throw yy::parser::syntax_error(loc,"Missing block");

    }
    yy_pop_state();
    push_context();
    return yy::parser::make_OCURLY(loc);
}

void use_things() {
    yy_top_state();
}
