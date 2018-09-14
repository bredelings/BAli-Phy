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
%}

%option noyywrap nounput batch debug noinput

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
ascsymbol_except_colon    [!#$%&*+./<=>?@\\^|\-~]
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
exponent     {numspc}[eE][\-\+]?{@decimal}
bin_exponent {numspc}[pP][\-\+]?{@decimal}

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
  /* Here we check for reserved ids?*/
{varid}       return yy::parser::make_VARID  (yytext, loc);
{conid}       return yy::parser::make_CONID  (yytext, loc);

{qvarsym}      return yy::parser::make_QVARSYM  (yytext,loc);
{qconsym}      return yy::parser::make_QCONSYM  (yytext,loc);
  /* Here we check for reserved ids?*/
"="     return yy::parser::make_EQUAL (loc);
{varsym}      return yy::parser::make_VARSYM  (yytext,loc);
  /* Here we check for reserved ids?*/
{consym}      return yy::parser::make_CONSYM  (yytext,loc);


{decimal}      {
  errno = 0;
  long n = strtol (yytext, NULL, 10);
  if (! (INT_MIN <= n && n <= INT_MAX && errno != ERANGE))
    throw yy::parser::syntax_error (loc, "integer is out of range: "
                                    + std::string(yytext));
  return yy::parser::make_INTEGER (n, loc);
}


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
