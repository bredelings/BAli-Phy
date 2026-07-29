%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>
# include <string_view>
# include "driver.hh"
# include "parser.hh"
# include "util/string/convert.H"    

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
zz::parser::symbol_type zz_make_integer(std::string_view text, int radix, bool positive, int shift,
				     const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_integer10(std::string_view text, const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_float(std::string_view text, const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_string(std::string_view text, const zz::parser::location_type& loc);

%}

%option noyywrap nounput batch debug noinput never-interactive nounistd
%option prefix="zz"
%s EXPRESSION TYPE

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
unisymbol    \x04
symbol_except_colon {ascsymbol_except_colon}
symbol       {ascsymbol}

 /* Model operators exclude delimiters that must remain visible without surrounding whitespace. */
model_symbol_except_colon [!#$%&\*+./<=>?\\^|\-]
model_symbol {model_symbol_except_colon}|:

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

uniidchar    \x07
idchar       {small}|{large}|{digit}|{uniidchar}|\'

pragmachar   {small}|{large}|{digit}

docsym       [|\^*$]

varid        {small}{idchar}*
conid        {large}{idchar}*

varsym       {model_symbol_except_colon}{model_symbol}*
consym       :{symbol}*

numspc       _*
decimal      {decdigit}({numspc}{decdigit})*
exponent     {numspc}[eE][\-\+]?{decimal}
escape	     \\[abfnrtv\"\']

qual         ({conid}\.)+
qvarid       {qual}{varid}
qconid       {qual}{conid}
qvarsym      {qual}{varsym}
qconsym      {qual}{consym}

floating_point      {numspc}{decimal}\.{decimal}{exponent}?|{numspc}{decimal}{exponent}

negative     \-
signed       {negative}?


%{
  // Audit note: this flex hook is the remaining implicit text-length dependency.
  // Token helpers receive matched text explicitly from rule actions.
  # define YY_USER_ACTION  loc.columns (yyleng);
%}

%%

  /* Code run each time yylex is called. */
%{
    // A handy shortcut to the location held by the driver.
    auto& loc = drv.location;

    if (int t = drv.get_start_token())
    {
	if (t == 1)
	{
	    BEGIN(EXPRESSION);
	    return zz::parser::make_START_EXP (loc);
	}
	else if (t == 2)
	{
	    BEGIN(TYPE);
	    return zz::parser::make_START_TYPE (loc);
	}
	else if (t == 3)
	{
	    BEGIN(EXPRESSION);
	    return zz::parser::make_START_DEFS (loc);
	}
	else
	    std::abort();
    }

    loc.step ();
%}

{white_no_nl}+   loc.step ();
{tab}	         loc.step ();

 /* Comment */
"#"#*[^!#$%&*+./<=>?@\\^|\-~:\n].*           loc.step();

  /* EOL */
[\n]              loc.lines(1); loc.step ();

"("      return zz::parser::make_OPAREN (loc);
")"      return zz::parser::make_CPAREN (loc);
"|"      return zz::parser::make_BAR    (loc);
"["      return zz::parser::make_OBRACK (loc);
"]"      return zz::parser::make_CBRACK (loc);
"{"      return zz::parser::make_OCURLY (loc);
"}"      return zz::parser::make_CCURLY (loc);
","      return zz::parser::make_COMMA  (loc);
"="      return zz::parser::make_EQUAL  (loc);
";"      return zz::parser::make_SEMI   (loc);
":"      return zz::parser::make_COLON  (loc);
"+>"     return zz::parser::make_STACK  (loc);
"->"     return zz::parser::make_ARROW  (loc);
"@"      return zz::parser::make_AT     (loc);
<EXPRESSION>"`" return zz::parser::make_BACKQUOTE (loc);
"~"      return zz::parser::make_TILDE  (loc);
"-"      return zz::parser::make_MINUS  (loc);

 /* Keep angle brackets separate in types so List<List<a>> closes twice. */
<TYPE>"<"      return zz::parser::make_LT (loc);
<TYPE>">"      return zz::parser::make_GT (loc);


{qvarid}      return zz::parser::make_QVARID (std::string(yytext, yyleng), loc);
{qconid}      return zz::parser::make_QVARID (std::string(yytext, yyleng), loc);
{varid}       return drv.varid(std::string_view(yytext, yyleng), loc);
{conid}       return drv.varid(std::string_view(yytext, yyleng), loc);

<EXPRESSION>{qvarsym} return zz::parser::make_QVARSYM(std::string(yytext, yyleng), loc);
<EXPRESSION>{varsym}  return zz::parser::make_VARSYM(std::string(yytext, yyleng), loc);

{decimal}                              return zz_make_integer10(std::string_view(yytext, yyleng), loc);

{floating_point}                       return zz_make_float(std::string_view(yytext, yyleng), loc);
[\"]({graphicnonqq}|" "|{escape})*[\"]   return zz_make_string(std::string_view(yytext, yyleng), loc);

<*>.          {
             throw zz::parser::syntax_error
               (loc, "invalid character: " + std::string(yytext, yyleng));
}

<<EOF>>    return zz::parser::make_END (loc);
%%



void
zz_driver::scan_begin (const std::string& content)
{
  // Audit note: buffer ownership still uses flex globals until scanner state is explicit.
  yy_flex_debug = trace_scanning;
  yy_scan_string(content.c_str());
}


void
zz_driver::scan_end ()
{
  // Audit note: buffer ownership still uses flex globals until scanner state is explicit.
  yy_delete_buffer(YY_CURRENT_BUFFER);
}

using namespace yy;

zz::parser::symbol_type zz_driver::varid(std::string_view text, const zz::parser::location_type& loc) const
{
    std::string token(text);
    auto it = reserved_words.find(token);
    if (it == reserved_words.end())
	return zz::parser::make_VARID  (token, loc);
    else
    {
	auto tok = it->second.first;
	return zz::parser::symbol_type(tok, loc);
    }
}

zz::parser::symbol_type zz_make_integer(std::string_view text, int radix, bool positive, int shift,
				     const zz::parser::location_type& loc)
{
    errno = 0;
    std::string token(text);
    auto n = std::stoi(token.c_str()+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw zz::parser::syntax_error(loc, "integer is out of range: " + token);
    if (not positive)
	n = -n;
    return zz::parser::make_INTEGER(n, loc);
}

zz::parser::symbol_type zz_make_integer10(std::string_view text, const zz::parser::location_type& loc)
{
    int shift = 0;
    bool positive = true;
    if (text[0] == '-')
    {
        shift++;
        positive = false;
    }
    return zz_make_integer(text, 10, positive, shift, loc);
}

zz::parser::symbol_type zz_make_float(std::string_view text, const zz::parser::location_type& loc)
{
    std::string s(text);
    double d = convertTo<double>(s);
    return zz::parser::make_FLOAT(d,loc);
}

char zz_get_char(std::string_view s, int& i)
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

zz::parser::symbol_type zz_make_string(std::string_view text, const zz::parser::location_type& loc)
{
    std::string s;
    s.resize(text.size());
    int j=0;
    for(int i=1;i<text.size()-1;i++)
	s[j++] = zz_get_char(text,i);
    s.resize(j);
    return zz::parser::make_STRING(s,loc);
}
