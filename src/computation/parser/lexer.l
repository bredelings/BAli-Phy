%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <string>
# include <regex>
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
yy::parser::symbol_type make_integer(int radix, bool positive, int shift,
				     const yy::parser::location_type& loc);

yy::parser::symbol_type make_boxed_integer(int radix, bool positive, int shift,
				           const yy::parser::location_type& loc);

yy::parser::symbol_type make_integer10(const yy::parser::location_type& loc);

yy::parser::symbol_type make_boxed_integer10(const yy::parser::location_type& loc);

yy::parser::symbol_type make_rational(const yy::parser::location_type& loc);

yy::parser::symbol_type make_char(const yy::parser::location_type& loc, driver& drv);
yy::parser::symbol_type make_string(const yy::parser::location_type& loc, driver& drv);

%}

%option noyywrap nounput batch debug noinput stack never-interactive nounistd
%s layout layout_do layout_if layout_left bol nested_comment nested_comment2
%s option_prags line_prag1a line_prag2 line_prag2a

unispace     \x05
nl           [\n\r\f]
space        " "|{unispace}
whitechar    {nl}|\v|{space}
white_no_nl  [\v\ ]|{unispace}
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
charescape   [a b f n r t v \\ \" \' \&]

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

gap          \\{whitechar}+\\
escape	     \\{charescape}
escapechar   \\({charescape}|" "|{graphic})
stringchar   {graphicnonqq}|" "|{escape}|{gap}
/* We want to parse incorrect escapes as char/string so that we can give better error messages.
   If we don't do so, then bad escapes fail to lex and fall through to "invalid character" error message. */
char         {graphicnonq}|" "|{escapechar}

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
  # define YY_USER_ACTION  loc.columns (yyleng); drv.step_closing_token();
%}

%%

  /* Code run each time yylex is called. */
%{
  // A handy shortcut to the location held by the driver.
  yy::location& loc = drv.location;
  loc.step ();
%}

{white_no_nl}+   loc.step ();
{tab}	         loc.step ();

  /* GHC calls some handwritten code to deal with nested comments */
<nested_comment2>"-}"        yy_pop_state();
<nested_comment>"-}"         loc.step(); yy_pop_state();
"-}"         throw yy::parser::syntax_error(loc, "Too many -}");

<nested_comment,nested_comment2>{
"{-"         yy_push_state(nested_comment2);
"{"          // eat a chunk with no newline or "-"
[^-\n{]*     // eat a chunk with no newline or "-"
"-"          // eat a "-" not followed by "}"
[\n]         loc.lines(1);
<<EOF>>      throw yy::parser::syntax_error(loc, "Unterminated {-");
}
"{-"/[^#]                    yy_push_state(nested_comment);




"--"-*[^!#$%&*+./<=>?@\\^|\-~:\n].*           loc.step();
"--"-*$           loc.step();
"-- "$            loc.step();

  /* bol state */
<bol>[\n]  loc.lines(1); loc.step();
<bol>""/.  if (auto tok = drv.do_bol(loc)) return *tok;
<bol><<EOF>>  if (auto tok = drv.do_bol(loc)) return *tok;

<layout,layout_do,layout_if>"{"/[^-]    return drv.hopefully_open_brace(loc);
<layout,layout_do,layout_if>[\n]        loc.lines(1); loc.step();

<layout_if>"|"/[^!#$%&*+./<=>?@\\^|\-~:\n]  drv.hopefully_open_brace(loc);
<layout_if>""/.                             yy_pop_state();

<layout>""/.                              return drv.new_layout_context(loc, true, true, yy::parser::token::TOK_VOCURLY);
<layout_do>""/.                           return drv.new_layout_context(loc, false, true, yy::parser::token::TOK_VOCURLY);

  /* If we've generated a VOCURLY and started a new line to the left, then
     we need to generate a VCCURLY */
<layout_left>""/.                       return drv.do_layout_left(loc);

<INITIAL,option_prags>[\n]              loc.lines(1); loc.step (); yy_push_state(bol);

  /* line pragmas */

  /* <line_prag1> single-line line pragmas: # <line> "<file>" <extra-stuff> \n */

  /* <line_prag2> Haskell-style line pragms, of the form
     {-# LINE <line> "<file>" #-} */

  /* <line_prag2a> */

  /* <column_prag> */

  /* two-word pragmas */
<INITIAL,option_prags>"{-#"{whitechar}*{pragmachar}+{whitechar}+{pragmachar}+  if (auto prag = drv.prag(loc)) return *prag; else yy_push_state(nested_comment);
  /* one-word pragmas */
<INITIAL,option_prags>"{-#"{whitechar}*{pragmachar}+                           if (auto prag = drv.prag(loc)) return *prag; else yy_push_state(nested_comment);
  /* end pragma */
<INITIAL,option_prags>"#-}"                                                    return yy::parser::make_CLOSE_PRAG(loc);

  /* unboxed tuples: (# and #) */

<INITIAL,option_prags>"("      return yy::parser::make_OPAREN (loc);
<INITIAL,option_prags>")"      drv.set_closing_token(); return yy::parser::make_CPAREN (loc);
<INITIAL,option_prags>"["      return yy::parser::make_OBRACK (loc);
<INITIAL,option_prags>"]"      drv.set_closing_token(); return yy::parser::make_CBRACK (loc);
<INITIAL,option_prags>","      return yy::parser::make_COMMA  (loc);
<INITIAL,option_prags>";"      return yy::parser::make_SEMI   (loc);
<INITIAL,option_prags>"`"      return yy::parser::make_BACKQUOTE (loc);

  /* action: push a "NoLayout" context into the stack */
"{"      drv.push_context(); return yy::parser::make_OCURLY (loc);
  /* action: pop the top context from the stack */
"}"      drv.set_closing_token(); drv.pop_context(); return yy::parser::make_CCURLY (loc);

<INITIAL,option_prags>{qvarid}      drv.set_closing_token(); return yy::parser::make_QVARID (yytext, loc);
<INITIAL,option_prags>{qconid}      drv.set_closing_token(); return yy::parser::make_QCONID (yytext, loc);
<INITIAL,option_prags>{varid}       drv.set_closing_token(); return drv.varid(loc);
<INITIAL,option_prags>{conid}       drv.set_closing_token(); return yy::parser::make_CONID  (yytext, loc);
  /* Here we look for {qvarid}#+ ... {conid}#+ if magicHashEnabled */


  /* precededByClosing and followedByOpening -> tight infix
     followedByOpening                       -> prefix
     precededByClosing                       -> suffix
     otherwise                               -> loose infix */
<INITIAL>{varsym}/"{-"                   return drv.varsym(drv.check_closing_token(), false, loc);
<INITIAL>{varsym}/[{([\"\'_A-Za-z0-9]    return drv.varsym(drv.check_closing_token(), true, loc);
<INITIAL>{varsym}                        return drv.varsym(drv.check_closing_token(), false, loc);

<INITIAL>{qvarsym}      return yy::parser::make_QVARSYM  (yytext,loc);
<INITIAL>{qconsym}      return yy::parser::make_QCONSYM  (yytext,loc);
<INITIAL>{consym}       return drv.consym(loc);

<INITIAL>"-"{decimal}"#"                        drv.set_closing_token(); return make_boxed_integer10(loc);
<INITIAL>{decimal}"#"                           drv.set_closing_token(); return make_boxed_integer10(loc);
<INITIAL>{decimal}                              drv.set_closing_token(); return make_integer10(loc);
  /* 0[bB]{numspc}{binary}      make_integer(2,true,2,loc); */
<INITIAL>0[oO]{numspc}{octal}                   drv.set_closing_token(); return make_integer( 8,true,2,loc);
<INITIAL>0[xX]{numspc}{hexadecimal}             drv.set_closing_token(); return make_integer(16,true,2,loc);

  /* negative literals depend on an extension */
<INITIAL>{floating_point}                       drv.set_closing_token(); return make_rational(loc);

  /* Its important that we only allow escaped quotes inside char or string literals */
<INITIAL>\'({char})\'         drv.set_closing_token(); return make_char(loc, drv);
<INITIAL>[\"]({stringchar})*[\"]                drv.set_closing_token(); return make_string(loc, drv);

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

std::map<std::string, std::function<yy::parser::symbol_type(const yy::parser::location_type&)>> pragmas = {
    {"incoherent", yy::parser::make_INCOHERENT_PRAG},
    {"inline"      , yy::parser::make_INLINE_PRAG},
    {"overlappable", yy::parser::make_OVERLAPPABLE_PRAG},
    {"overlapping", yy::parser::make_OVERLAPPING_PRAG},
    {"overlaps", yy::parser::make_OVERLAPS_PRAG},
    {"specialize", yy::parser::make_SPECIALIZE_PRAG}
};

std::string clean_pragma(std::string prag)
{
    // 1. Remove pragma prefix
    if (prag.starts_with("{-#"))
        prag = prag.substr(3);

    // 2. Lower-case all the characters
    for(auto& c: prag)
        c = std::tolower(c);

    // 3. Split into words
    std::vector<std::string> words;
    bool inword = false;
    for(auto c: prag)
    {
        if (std::isspace(c))
            inword = false;
        else
        {
            if (not inword)
            {
                words.push_back({});
                inword=true;
            }
            words.back() += c;
        }
    }
    
    // 4. Connect the words with ' '
    return join(words, ' ');
}

std::optional<yy::parser::symbol_type> driver::prag(const yy::parser::location_type& loc)
{
    auto prag = clean_pragma(yytext);
    auto it = pragmas.find(prag);
    if (it == pragmas.end())
    {
        push_warning_message(loc, "Unknown pragma");
        return {};
    }
    else
        return it->second(loc);
}

yy::parser::symbol_type driver::varid(const yy::parser::location_type& loc) const
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

yy::parser::symbol_type driver::varsym(bool precededByClosing, bool followedByOpening, const yy::parser::location_type& loc) const
{
    // if loose infix occurrences are always operators, then:
    // if we handle type operators, then we should be able to parse (a ~ b) with ~ being a type operator.

    // tight infix reserved symbols
    if (precededByClosing and followedByOpening)
    {
        auto it = tight_infix_reserved_symbols.find(yytext);
        if (it != tight_infix_reserved_symbols.end())
        {
            auto tok = it->second.first;
            return yy::parser::symbol_type(tok, loc);
        }
    }

    // prefix reserved symbols
    if (precededByClosing == false and followedByOpening == true)
    {
        auto it = prefix_reserved_symbols.find(yytext);
        if (it != prefix_reserved_symbols.end())
        {
            auto tok = it->second.first;
            return yy::parser::symbol_type(tok, loc);
        }
    }

    // other reserved symbols
    auto it = reserved_symbols.find(yytext);
    if (it == reserved_symbols.end())
	return yy::parser::make_VARSYM  (yytext, loc);

    // general varsyms
    auto tok = it->second.first;
    return yy::parser::symbol_type(tok, loc);
}

yy::parser::symbol_type driver::consym(const yy::parser::location_type& loc) const
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

yy::parser::symbol_type make_integer10(const yy::parser::location_type& loc)
{
    return yy::parser::make_INTEGER(Haskell::integerFromString(yytext), loc);
}

yy::parser::symbol_type make_boxed_integer10(const yy::parser::location_type& loc)
{
    std::string s(yytext);
    assert(s.back() == '#');
    s.resize(s.size()-1);
    return yy::parser::make_PRIMINTEGER(Haskell::integerFromString(s), loc);
}

yy::parser::symbol_type make_integer(int radix, bool positive, int shift,
				     const yy::parser::location_type& loc)
{
    // This doesn't handle underscore separators
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
    // This doesn't handle underscore separators
    errno = 0;
    auto n = std::stoi(yytext+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw yy::parser::syntax_error(loc, "integer is out of range: " + std::string(yytext));
    if (not positive)
	n = -n;
    return yy::parser::make_PRIMINTEGER(n, loc);
}

#include "computation/haskell/literal.H"

yy::parser::symbol_type make_rational(const yy::parser::location_type& loc)
{
    rational r = Haskell::rationalFromString(yytext);
    return yy::parser::make_RATIONAL(r, loc);
}

std::optional<char> get_char(const char* s, int& i)
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
	    c = '"';
	else if (c == '\'')
	    c = '\'';
	else if (c == '\\')
	    c = '\\';
	else
            return {};
    }
    return c;
}

yy::parser::symbol_type make_char(const yy::parser::location_type& loc, driver& drv)
{
    int i=1;
    auto c = get_char(yytext, i);

    // Check for unconsumed "\" right before the final "'"
    if (i == yyleng - 1)
    {
        assert(yyleng > 2 and yytext[yyleng-2] == '\\');
        yy::parser::location_type loc2(loc.begin+(yyleng-2));
        loc2 += 2;
        auto msg = Note()<<"invalid escape of ending quote";
        drv.push_error_message(loc2, msg);

        return yy::parser::make_CHAR('?',loc);
    }

    if (c)
        return yy::parser::make_CHAR(*c,loc);
    else
    {
        yy::parser::location_type loc2(loc.begin+1);
        loc2 += 2;
        auto msg = Note()<<"invalid escape sequence '"<<yytext[1]<<yytext[2]<<"'";
        drv.push_error_message(loc2, msg);

        return yy::parser::make_CHAR('?',loc);
    }
            
}

yy::parser::symbol_type make_string(const yy::parser::location_type& loc, driver& drv)
{
    std::string s;
    s.resize(yyleng);
    int j=0;
    for(int i=1;i<yyleng-1;i++)
    {
        if (auto c = get_char(yytext, i))
            s[j++] = *c;
        else
        {
            yy::parser::location_type loc2(loc.begin+(i-1));
            loc2 += 2;
            auto msg = Note()<<"invalid escape sequence '"<<yytext[i-1]<<yytext[i]<<"'";
            drv.push_error_message(loc2, msg);

            s[j++] = '?';
        }

        // Check for unconsumed "\" right before the final "'"
        if (i == yyleng - 1)
        {
            assert(yyleng > 2 and yytext[yyleng-2] == '\\');
            yy::parser::location_type loc2(loc.begin+(yyleng-2));
            loc2 += 2;
            auto msg = Note()<<"invalid escape of ending quote";
            drv.push_error_message(loc2, msg);

            s[j-1] = '?';
        }
    }
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
    yy_push_state(bol);
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
