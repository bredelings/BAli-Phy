%top{
# include "driver.hh"
}

%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>
# include <string_view>
# include "driver.hh"
# include "parser.hh"
# include "util/string/convert.hh"

// Forward declarations
zz::parser::symbol_type zz_make_integer(std::string_view text, int radix, bool positive, int shift,
				     const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_integer10(std::string_view text, const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_float(std::string_view text, const zz::parser::location_type& loc);

zz::parser::symbol_type zz_make_string(std::string_view text, const zz::parser::location_type& loc);

%}

%option unicode flex noyywrap batch debug never-interactive prefix="zz"
%option lexer=ModelLexer lex=lex token-type="zz::parser::symbol_type" token-eof="zz::parser::make_END(yy::location())" params="zz_driver& drv"
%s EXPRESSION TYPE

unispace     \x05
nl           [\n\r\f]
white_no_nl  [\v\ ]|{unispace}
whitechar    {white_no_nl}|{nl}
tab          \t

ascdigit     [0-9]
unidigit     \p{Nd}
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

unilarge     \p{Lu}|\p{Lt}
asclarge     [A-Z]
large        {asclarge}|{unilarge}

unismall     \p{Ll}|\p{Lo}
ascsmall     [a-z]
small        {ascsmall}|{unismall}|_

unigraphic   \x06
graphic      {small}|{large}|{symbol}|{digit}|{special}|{unigraphic}|\"\'
graphicnonq  {small}|{large}|{symbol}|{digit}|{special}|{unigraphic}|\"
graphicnonqq {small}|{large}|{symbol}|{digit}|{special}|{unigraphic}|\'

uniidchar    \p{Lm}|\p{Mn}|\p{Nl}|\p{No}
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
 /* Unicode mode matches complete UTF-8 characters; keep raw ASCII controls excluded. */
stringchar   [\x20-\x21\x23-\x5b\x5d-\x7e]|\p{Non_ASCII_Unicode}
 /* Admit unsupported escapes into the token so the decoder can report a parse error. */
escape       \\[^\n\r\f]

qual         ({conid}\.)+
qvarid       {qual}{varid}
qconid       {qual}{conid}
qvarsym      {qual}{varsym}
qconsym      {qual}{consym}

floating_point      {numspc}{decimal}\.{decimal}{exponent}?|{numspc}{decimal}{exponent}

negative     \-
signed       {negative}?


%{
  // Preserve the model parser's byte-based columns.
  # undef YY_USER_ACTION
  # define YY_USER_ACTION drv.location.columns(yyleng);
%}

%%

{white_no_nl}+   drv.location.step ();
{tab}	         drv.location.step ();

 /* Comment */
"#"#*[^!#$%&*+./<=>?@\\^|\-~:\n].*           drv.location.step();

  /* EOL */
[\n]              drv.location.lines(1); drv.location.step ();

"("      return zz::parser::make_OPAREN (drv.location);
")"      return zz::parser::make_CPAREN (drv.location);
"|"      return zz::parser::make_BAR    (drv.location);
"["      return zz::parser::make_OBRACK (drv.location);
"]"      return zz::parser::make_CBRACK (drv.location);
"{"      return zz::parser::make_OCURLY (drv.location);
"}"      return zz::parser::make_CCURLY (drv.location);
","      return zz::parser::make_COMMA  (drv.location);
"="      return zz::parser::make_EQUAL  (drv.location);
";"      return zz::parser::make_SEMI   (drv.location);
":"      return zz::parser::make_COLON  (drv.location);
"+>"     return zz::parser::make_STACK  (drv.location);
"->"     return zz::parser::make_ARROW  (drv.location);
"@"      return zz::parser::make_AT     (drv.location);
<EXPRESSION>"`" return zz::parser::make_BACKQUOTE (drv.location);
"~"      return zz::parser::make_TILDE  (drv.location);
"-"      return zz::parser::make_MINUS  (drv.location);

 /* Keep angle brackets separate in types so List<List<a>> closes twice. */
<TYPE>"<"      return zz::parser::make_LT (drv.location);
<TYPE>">"      return zz::parser::make_GT (drv.location);


{qvarid}      return zz::parser::make_QVARID (std::string(yytext, yyleng), drv.location);
{qconid}      return zz::parser::make_QVARID (std::string(yytext, yyleng), drv.location);
{varid}       return drv.varid(std::string_view(yytext, yyleng), drv.location);
{conid}       return drv.varid(std::string_view(yytext, yyleng), drv.location);

<EXPRESSION>{qvarsym} return zz::parser::make_QVARSYM(std::string(yytext, yyleng), drv.location);
<EXPRESSION>{varsym}  return zz::parser::make_VARSYM(std::string(yytext, yyleng), drv.location);

{decimal}                              return zz_make_integer10(std::string_view(yytext, yyleng), drv.location);

{floating_point}                       return zz_make_float(std::string_view(yytext, yyleng), drv.location);
[\"]({stringchar}|{escape})*[\"]   return zz_make_string(std::string_view(yytext, yyleng), drv.location);

<*>.          {
             throw zz::parser::syntax_error
               (drv.location, "invalid character: " + std::string(yytext, yyleng));
}

<<EOF>>    return zz::parser::make_END (drv.location);
%%



// Destroy the scanner where its generated class is complete.
void ModelLexerDeleter::operator()(ModelLexer* scanner) const
{
    delete scanner;
}

// Select the grammar entry point before requesting real tokens from the scanner.
zz::parser::symbol_type zzlex(zz_driver& drv)
{
    auto& scanner = *drv.scanner;
    auto& loc = drv.location;
    switch (drv.get_start_token())
    {
        case 1:
            scanner.start(EXPRESSION);
            return zz::parser::make_START_EXP(loc);
        case 2:
            scanner.start(TYPE);
            return zz::parser::make_START_TYPE(loc);
        case 3:
            scanner.start(EXPRESSION);
            return zz::parser::make_START_DEFS(loc);
    }
    loc.step();
    return scanner.lex(drv);
}

// Input borrows parse_string's content, which remains alive throughout the parse.
// A fresh scanner keeps input and start conditions separate between driver instances.
void zz_driver::scan_begin(const std::string& content)
{
    scanner.reset(new ModelLexer(reflex::Input(content)));
    scanner->set_debug(trace_scanning);
}

void zz_driver::scan_end()
{
    scanner.reset();
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

// Decode ASCII escapes while copying ordinary UTF-8 bytes unchanged.
char zz_get_char(std::string_view s, int& i, const zz::parser::location_type& loc)
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
	else if (c == '\\')
	    ;
	else
	    throw zz::parser::syntax_error(loc, "invalid string escape");
    }
    return c;
}

zz::parser::symbol_type zz_make_string(std::string_view text, const zz::parser::location_type& loc)
{
    std::string s;
    s.resize(text.size());
    int j=0;
    for(int i=1;i<text.size()-1;i++)
	s[j++] = zz_get_char(text,i,loc);
    s.resize(j);
    return zz::parser::make_STRING(s,loc);
}
