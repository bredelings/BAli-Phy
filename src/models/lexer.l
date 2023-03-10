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
zz::parser::symbol_type zz_make_integer(int radix, bool positive, int shift,
				     const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_integer10(const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_float(const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_string(const zz::parser::location_type& loc);

%}

%option noyywrap nounput batch debug noinput stack never-interactive nounistd
%option prefix="zz"

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

varsym       {symbol_except_colon}{symbol}*
consym       :{symbol}*

numspc       _*
decimal      {decdigit}({numspc}{decdigit})*
exponent     {numspc}[eE][\-\+]?{decimal}
escape	     \\[abfnrtv\"\']

qual         ({conid}\.)+
qvarid       {qual}{varid}
qconid       {qual}{conid}

floating_point      {numspc}{decimal}\.{decimal}{exponent}?|{numspc}{decimal}{exponent}

negative     \-
signed       {negative}?


%{
  // Code run each time a pattern is matched.
  # define YY_USER_ACTION  loc.columns (yyleng);
%}

%%

  /* Code run each time yylex is called. */
%{
  // A handy shortcut to the location held by the driver.
    auto& loc = drv.location;
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
"["      return zz::parser::make_OBRACK (loc);
"]"      return zz::parser::make_CBRACK (loc);
","      return zz::parser::make_COMMA  (loc);
"="      return zz::parser::make_EQUAL  (loc);
"+"      return zz::parser::make_PLUS   (loc);
"@"      return zz::parser::make_AT     (loc);
"~"      return zz::parser::make_TILDE  (loc);


{qvarid}      return zz::parser::make_QVARID (yytext, loc);
{qconid}      return zz::parser::make_QVARID (yytext, loc);
{varid}       return drv.varid(loc);
{conid}       return drv.varid(loc);

{signed}{decimal}                              return zz_make_integer10(loc);

{signed}{floating_point}                       return zz_make_float(loc);
[\"]({graphicnonqq}|" "|{escape})*[\"]   return zz_make_string(loc);

<*>.          {
             throw zz::parser::syntax_error
               (loc, "invalid character: " + std::string(yytext));
}

<<EOF>>    return zz::parser::make_END (loc);
%%



void
zz_driver::scan_begin (const std::string& content)
{
  yy_flex_debug = trace_scanning;
  yy_scan_string(content.c_str());
}


void
zz_driver::scan_end ()
{
  yy_delete_buffer(YY_CURRENT_BUFFER);
}

using namespace yy;

zz::parser::symbol_type zz_driver::varid(const zz::parser::location_type& loc) const
{
    auto it = reserved_words.find(yytext);
    if (it == reserved_words.end())
	return zz::parser::make_VARID  (yytext, loc);
    else
    {
	auto tok = it->second.first;
	return zz::parser::symbol_type(tok, loc);
    }
}

zz::parser::symbol_type zz_make_integer(int radix, bool positive, int shift,
				     const zz::parser::location_type& loc)
{
    errno = 0;
    auto n = std::stoi(yytext+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw zz::parser::syntax_error(loc, "integer is out of range: " + std::string(yytext));
    if (not positive)
	n = -n;
    return zz::parser::make_INTEGER(n, loc);
}

zz::parser::symbol_type zz_make_integer10(const zz::parser::location_type& loc)
{
    int shift = 0;
    bool positive = true;
    if (yytext[0] == '-')
    {
        shift++;
        positive = false;
    }
    return zz_make_integer(10, positive, shift, loc);
}

zz::parser::symbol_type zz_make_float(const zz::parser::location_type& loc)
{
    std::string s(yytext);
    double d = convertTo<double>(std::string(yytext));
    return zz::parser::make_FLOAT(d,loc);
}

char zz_get_char(const char* s, int& i)
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

zz::parser::symbol_type zz_make_string(const zz::parser::location_type& loc)
{
    std::string s;
    s.resize(yyleng);
    int j=0;
    for(int i=1;i<yyleng-1;i++)
	s[j++] = zz_get_char(yytext,i);
    s.resize(j);
    return zz::parser::make_STRING(s,loc);
}

void zz_use_things() {
    yy_top_state();
    yy_push_state(0);
    yy_pop_state();
}
