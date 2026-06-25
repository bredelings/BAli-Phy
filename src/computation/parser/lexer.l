%top{
# include "driver.hh"
}

%{ /* -*- C++ -*- */
# include <cerrno>
# include <cassert>
# include <climits>
# include <cstdlib>
# include <memory>
# include <string>
# include <string_view>
# include <regex>
# include "driver.hh"
# include "parser.hh"

// Forward declarations
yy::parser::symbol_type make_integer(std::string_view text, int radix, bool positive, int shift,
				     const yy::parser::location_type& loc);

yy::parser::symbol_type make_boxed_integer(std::string_view text, int radix, bool positive, int shift,
				           const yy::parser::location_type& loc);

yy::parser::symbol_type make_integer10(std::string_view text, const yy::parser::location_type& loc);

yy::parser::symbol_type make_boxed_integer10(std::string_view text, const yy::parser::location_type& loc);

yy::parser::symbol_type make_rational(std::string_view text, const yy::parser::location_type& loc);

yy::parser::symbol_type make_char(std::string_view text, const yy::parser::location_type& loc, driver& drv);
yy::parser::symbol_type make_string(std::string_view text, const yy::parser::location_type& loc, driver& drv);

static SymbolOccurrence symbol_occurrence(bool preceded_by_closing, bool followed_by_opening);

%}

%option batch debug never-interactive bison-cc bison-locations YYSTYPE=int YYLTYPE=yy::location
%option lexer=HaskellRawLexer lex=raw_yylex token-type=LexedToken token-eof="LexedToken(yy::parser::make_END(yy::location()))" params="driver& drv"
%s nested_comment nested_comment2
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

%%

  /* Code run each time yylex is called. */
%{
  // RE/flex's bison-cc interface passes yylval even though complete Bison
  // symbols carry semantic values here; mark it used until that interface goes.
  (void)yylval;
  drv.location.step ();
%}

{white_no_nl}+   drv.location.step ();
{tab}	         drv.location.step ();

  /* GHC calls some handwritten code to deal with nested comments */
<nested_comment2>"-}"        pop_state();
<nested_comment>"-}"         drv.location.step(); pop_state();
<INITIAL,option_prags>"-}"   throw yy::parser::syntax_error(drv.location, "Too many -}");

<nested_comment,nested_comment2>{
"{-"         push_state(nested_comment2);
"{"          // eat a chunk with no newline or "-"
[^-\n{]*     // eat a chunk with no newline or "-"
"-"          // eat a "-" not followed by "}"
[\n]         drv.location.lines(1);
<<EOF>>      throw yy::parser::syntax_error(drv.location, "Unterminated {-");
}
"{-"/[^#]                    push_state(nested_comment);


  /* These rules should not be active in a nested comment */
<INITIAL,option_prags,line_prag1a,line_prag2,line_prag2a>{
"--"-*[^!#$%&*+./<=>?@\\^|\-~:\n].*           drv.location.step();
"--"-*$           drv.location.step();
"-- "$            drv.location.step();
}

<INITIAL,option_prags>[\n]              drv.location.lines(1); drv.location.step(); drv.mark_next_real_token_starts_line();

  /* line pragmas */

  /* <line_prag1> single-line line pragmas: # <line> "<file>" <extra-stuff> \n */

  /* <line_prag2> Haskell-style line pragms, of the form
     {-# LINE <line> "<file>" #-} */

  /* <line_prag2a> */

  /* <column_prag> */

  /* two-word pragmas */
  /* <INITIAL,option_prags>"{-#"{whitechar}*{pragmachar}+{whitechar}+{pragmachar}+  if (auto prag = drv.prag(std::string_view(text(), size()), drv.location)) return *prag; else push_state(nested_comment); */
  /* one-word pragmas */
<INITIAL,option_prags>"{-#"{whitechar}*{pragmachar}+                           if (auto prag = drv.prag(std::string_view(text(), size()), drv.location)) return *prag; else push_state(nested_comment);
  /* end pragma */
<INITIAL,option_prags>"#-}"                                                    return yy::parser::make_CLOSE_PRAG(drv.location);

  /* unboxed tuples: (# and #) */

<INITIAL,option_prags>"("      return yy::parser::make_OPAREN (drv.location);
<INITIAL,option_prags>")"      return LexedToken(yy::parser::make_CPAREN(drv.location), TokenEffects{.closes_atom = true});
<INITIAL,option_prags>"["      return yy::parser::make_OBRACK (drv.location);
<INITIAL,option_prags>"]"      return LexedToken(yy::parser::make_CBRACK(drv.location), TokenEffects{.closes_atom = true});
<INITIAL,option_prags>","      return yy::parser::make_COMMA  (drv.location);
<INITIAL,option_prags>";"      return yy::parser::make_SEMI   (drv.location);
<INITIAL,option_prags>"`"      return yy::parser::make_BACKQUOTE (drv.location);

  /* action: push a "NoLayout" context into the stack */
<INITIAL,option_prags>"{"      return LexedToken(yy::parser::make_OCURLY(drv.location), TokenEffects{.push_no_layout_context = true});
  /* action: pop the top context from the stack */
<INITIAL,option_prags>"}"      return LexedToken(yy::parser::make_CCURLY(drv.location), TokenEffects{.closes_atom = true, .pop_context = true});

<INITIAL,option_prags>{qvarid}      return LexedToken(yy::parser::make_QVARID(std::string(text(), size()), drv.location), TokenEffects{.closes_atom = true});
<INITIAL,option_prags>{qconid}      return LexedToken(yy::parser::make_QCONID(std::string(text(), size()), drv.location), TokenEffects{.closes_atom = true});
<INITIAL,option_prags>{varid}       {
                                      auto classified = drv.classify_varid(std::string_view(text(), size()));
                                      auto layout_after = classified.layout_after;
                                      if (classified.token)
                                        return LexedToken(yy::parser::symbol_type(*classified.token, drv.location),
                                                          TokenEffects{.closes_atom = true, .layout_after = layout_after});
                                      else
                                        return LexedToken(yy::parser::make_VARID(classified.text, drv.location),
                                                          TokenEffects{.closes_atom = true, .layout_after = layout_after});
                                    }
<INITIAL,option_prags>{conid}       return LexedToken(yy::parser::make_CONID(std::string(text(), size()), drv.location), TokenEffects{.closes_atom = true});
  /* Here we look for {qvarid}#+ ... {conid}#+ if magicHashEnabled */


<INITIAL>{varsym}/"{-"                   {
                                            auto classified = drv.classify_varsym(std::string_view(text(), size()),
                                                                                 symbol_occurrence(drv.left_adjacent_closes_atom(drv.location), false));
                                            if (classified.token)
                                              return yy::parser::symbol_type(*classified.token, drv.location);
                                            else
                                              return yy::parser::make_VARSYM(classified.text, drv.location);
                                          }
<INITIAL>{varsym}/[{([\"\'_A-Za-z0-9]    {
                                            auto classified = drv.classify_varsym(std::string_view(text(), size()),
                                                                                 symbol_occurrence(drv.left_adjacent_closes_atom(drv.location), true));
                                            if (classified.token)
                                              return yy::parser::symbol_type(*classified.token, drv.location);
                                            else
                                              return yy::parser::make_VARSYM(classified.text, drv.location);
                                          }
<INITIAL>{varsym}                        {
                                            auto classified = drv.classify_varsym(std::string_view(text(), size()),
                                                                                 symbol_occurrence(drv.left_adjacent_closes_atom(drv.location), false));
                                            if (classified.token)
                                              return yy::parser::symbol_type(*classified.token, drv.location);
                                            else
                                              return yy::parser::make_VARSYM(classified.text, drv.location);
                                          }

<INITIAL>{qvarsym}      return yy::parser::make_QVARSYM  (std::string(text(), size()), drv.location);
<INITIAL>{qconsym}      return yy::parser::make_QCONSYM  (std::string(text(), size()), drv.location);
<INITIAL>{consym}       return drv.consym(std::string_view(text(), size()), drv.location);

<INITIAL>"-"{decimal}"#"                        return LexedToken(make_boxed_integer10(std::string_view(text(), size()), drv.location), TokenEffects{.closes_atom = true});
<INITIAL>{decimal}"#"                           return LexedToken(make_boxed_integer10(std::string_view(text(), size()), drv.location), TokenEffects{.closes_atom = true});
<INITIAL>{decimal}                              return LexedToken(make_integer10(std::string_view(text(), size()), drv.location), TokenEffects{.closes_atom = true});
  /* 0[bB]{numspc}{binary}      make_integer(2,true,2,drv.location); */
<INITIAL>0[oO]{numspc}{octal}                   return LexedToken(make_integer(std::string_view(text(), size()), 8,true,2,drv.location), TokenEffects{.closes_atom = true});
<INITIAL>0[xX]{numspc}{hexadecimal}             return LexedToken(make_integer(std::string_view(text(), size()),16,true,2,drv.location), TokenEffects{.closes_atom = true});

  /* negative literals depend on an extension */
<INITIAL>{floating_point}                       return LexedToken(make_rational(std::string_view(text(), size()), drv.location), TokenEffects{.closes_atom = true});

  /* Its important that we only allow escaped quotes inside char or string literals */
<INITIAL>\'({char})\'         return LexedToken(make_char(std::string_view(text(), size()), drv.location, drv), TokenEffects{.closes_atom = true});
<INITIAL>[\"]({stringchar})*[\"]                return LexedToken(make_string(std::string_view(text(), size()), drv.location, drv), TokenEffects{.closes_atom = true});

<INITIAL,option_prags,line_prag1a,line_prag2,line_prag2a>.          {
             throw yy::parser::syntax_error
               (drv.location, "invalid character: " + std::string(text(), size()));
}

<<EOF>>    return yy::parser::make_END (drv.location);
%%

class BaliPhyHaskellRawLexer: public HaskellRawLexer
{
public:
    using HaskellRawLexer::HaskellRawLexer;

    void yylloc_update(yy::location& yylloc) override;
};

// Compatibility note: RE/flex's generated bison-cc updater uses 0-based
// columns, so adapt it until lexer actions construct tokens from location().
// Convert matcher columns to Bison's 1-based, end-exclusive convention while
// preserving the filename set by the driver.
void BaliPhyHaskellRawLexer::yylloc_update(yy::location& yylloc)
{
    yylloc.begin.line = static_cast<unsigned int>(matcher().lineno());
    yylloc.begin.column = static_cast<unsigned int>(matcher().columno() + 1);
    yylloc.end.line = static_cast<unsigned int>(matcher().lineno_end());
    yylloc.end.column = static_cast<unsigned int>(matcher().columno_end() + 2);
}


class RawLexerState
{
    std::string content;
    BaliPhyHaskellRawLexer scanner;

public:
    RawLexerState(const std::string& content_arg, bool trace_scanning);
    LexedToken lex(driver& drv);
};

// Keep RE/flex's input storage alive for the scanner and initialize scanner
// debug/state for one parse.
RawLexerState::RawLexerState(const std::string& content_arg, bool trace_scanning)
    : content(content_arg),
      scanner(reflex::Input(&content))
{
    scanner.set_debug(trace_scanning);
    scanner.start(HaskellRawLexer::INITIAL);
}

// Return one real source token from the generated RE/flex scanner.
LexedToken RawLexerState::lex(driver& drv)
{
    int yylval = 0;
    return scanner.raw_yylex(yylval, drv.location, drv);
}

// Delete the opaque scanner state where the RE/flex-generated class is
// complete, keeping RE/flex headers out of driver.hh.
void RawLexerStateDeleter::operator()(RawLexerState* state) const
{
    delete state;
}

driver::~driver() = default;

// Bridge the parser driver wrapper to the generated scanner object without
// exposing RE/flex types in driver.hh.
LexedToken
raw_yylex(driver& drv)
{
  assert(drv.raw_lexer);
  return drv.raw_lexer->lex(drv);
}

// Create a fresh RE/flex scanner for this parse so input and start-condition
// state do not leak between parser runs.
void
driver::scan_begin (const std::string& content)
{
  raw_lexer.reset(new RawLexerState(content, trace_scanning));
}


// Drop the parse-local scanner and its owned input once parsing is complete.
void
driver::scan_end ()
{
  raw_lexer.reset();
}

using namespace yy;

std::map<std::string, std::function<yy::parser::symbol_type(const yy::parser::location_type&)>> pragmas = {
    // "rules"
    {"inline"      , [](const parser::location_type& loc) {return parser::make_INLINE_PRAG("INLINE"   , loc);}}, 
    {"inlinable"   , [](const parser::location_type& loc) {return parser::make_INLINE_PRAG("INLINABLE", loc);}}, 
    {"inlineable"  , [](const parser::location_type& loc) {return parser::make_INLINE_PRAG("INLINABLE", loc);}}, 
    {"noinline"    , [](const parser::location_type& loc) {return parser::make_INLINE_PRAG("NOTINLINE", loc);}}, 
    {"notinline"   , [](const parser::location_type& loc) {return parser::make_INLINE_PRAG("NOTINLINE", loc);}}, 
    // "opaque"
    {"specialize", parser::make_SPECIALIZE_PRAG},
    // "source"
    // "warning"
    // "deprecated"
    // "scc"
    // "unpack"
    // "nounpack"
    // "ann"
    // "minimal"
    {"overlaps", parser::make_OVERLAPS_PRAG},
    {"overlappable", parser::make_OVERLAPPABLE_PRAG},
    {"overlapping", parser::make_OVERLAPPING_PRAG},
    {"incoherent", parser::make_INCOHERENT_PRAG},
    // "ctype"
    // "complete"
    // "column"
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

std::optional<yy::parser::symbol_type> driver::prag(std::string_view text, const yy::parser::location_type& loc)
{
    auto prag = clean_pragma(std::string(text));
    auto it = pragmas.find(prag);
    if (it == pragmas.end())
    {
        push_warning_message(loc, "Unknown pragma");
        return {};
    }
    else
        return it->second(loc);
}

// Name how a symbolic token occurs relative to adjacent lexical tokens, so
// reserved-symbol lookup does not depend on anonymous boolean combinations.
static SymbolOccurrence symbol_occurrence(bool preceded_by_closing, bool followed_by_opening)
{
    if (preceded_by_closing and followed_by_opening)
        return SymbolOccurrence::TightInfix;
    else if (followed_by_opening)
        return SymbolOccurrence::Prefix;
    else if (preceded_by_closing)
        return SymbolOccurrence::Suffix;
    else
        return SymbolOccurrence::LooseInfix;
}

yy::parser::symbol_type driver::consym(std::string_view text, const yy::parser::location_type& loc) const
{
    std::string token(text);
    auto it = reserved_symbols.find(token);
    if (it == reserved_symbols.end())
	return yy::parser::make_CONSYM  (token, loc);
    else
    {
	auto tok = it->second;
	return yy::parser::symbol_type(tok, loc);
    }
}

yy::parser::symbol_type make_integer10(std::string_view text, const yy::parser::location_type& loc)
{
    return yy::parser::make_INTEGER(Haskell::integerFromString(std::string(text)), loc);
}

yy::parser::symbol_type make_boxed_integer10(std::string_view text, const yy::parser::location_type& loc)
{
    std::string s(text);
    assert(s.back() == '#');
    s.resize(s.size()-1);
    return yy::parser::make_PRIMINTEGER(Haskell::integerFromString(s), loc);
}

yy::parser::symbol_type make_integer(std::string_view text, int radix, bool positive, int shift,
				     const yy::parser::location_type& loc)
{
    // This doesn't handle underscore separators
    errno = 0;
    std::string token(text);
    auto n = std::stoi(token.c_str()+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw yy::parser::syntax_error(loc, "integer is out of range: " + token);
    if (not positive)
	n = -n;
    return yy::parser::make_INTEGER(n, loc);
}

yy::parser::symbol_type make_boxed_integer(std::string_view text, int radix, bool positive, int shift,
                                           const yy::parser::location_type& loc)
{
    // This doesn't handle underscore separators
    errno = 0;
    std::string token(text);
    auto n = std::stoi(token.c_str()+shift, NULL, radix);
    if ( not (INT_MIN <= n and n <= INT_MAX and errno != ERANGE))
	throw yy::parser::syntax_error(loc, "integer is out of range: " + token);
    if (not positive)
	n = -n;
    return yy::parser::make_PRIMINTEGER(n, loc);
}

#include "computation/haskell/literal.H"

yy::parser::symbol_type make_rational(std::string_view text, const yy::parser::location_type& loc)
{
    rational r = Haskell::rationalFromString(std::string(text));
    return yy::parser::make_RATIONAL(r, loc);
}

bool is_whitespace_char(char c)
{
    if (c == ' ') return true;
    if (c == '\n') return true;
    if (c == '\r') return true;
    if (c == '\f') return true;
    if (c == '\v') return true;
    if (c == 5) return true;
    else return false;
}

std::optional<int> is_gap(const yy::parser::location_type& loc, driver& drv, std::string_view text, int start)
{
    int length = text.size();
    assert(start<length);

    if (not (start+1 < length and text[start] == '\\' and is_whitespace_char(text[start+1])))
        return {};

    int i = start+2;

    while(i<length and is_whitespace_char(text[i]))
        i++;

    if (i< length and text[i] == '\\')
    {
        i++; // skip the ending backslash also
    }
    else
    {
        yy::parser::location_type loc2(loc.begin+start, loc.begin+i);
        drv.push_error_message(loc2, Note()<<"invalid string: whitespace gap is missing final '\\'");
    }
    return i;
}

std::pair<char,int> get_char(const yy::parser::location_type& loc, driver& drv, std::string_view text, int length, int i)
{
    assert(i < length);

    char c = text[i];
    if (c == '\\')
    {
        if (i+1 >= length)
        {
            assert(text.size() > 2 and text[text.size()-2] == '\\');
            yy::parser::location_type loc2(loc.begin+i, loc.begin+i+2);
            drv.push_error_message(loc2, Note()<<"invalid escape of ending quote");
            return {'?',i+2};
        }

	c = text[i+1];
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
        {
            yy::parser::location_type loc2(loc.begin+i);
            loc2 += 2;
            auto msg = Note()<<"invalid escape sequence '\\"<<c<<"'";
            drv.push_error_message(loc2, msg);

            c = '?';
        }
        return {c,i+2};
    
    }
    return {c,i+1};
}

yy::parser::symbol_type make_char(std::string_view text, const yy::parser::location_type& loc, driver& drv)
{
    int i=1;
    auto [c,_] = get_char(loc, drv, text, text.size() - 1, i);

    return yy::parser::make_CHAR(c,loc);
}

yy::parser::symbol_type make_string(std::string_view text, const yy::parser::location_type& loc, driver& drv)
{
//    std::cerr<<"analyzing string: '"<<text<<"'\n";
    std::string s;
    s.resize(text.size());
    int j=0;
    for(int i=1;i<text.size()-1;)
    {
//        std::cerr<<"   looking at: '"<<text.substr(i)<<"'\n";
        if (auto after_gap = is_gap(loc, drv, text.substr(0, text.size()-1), i))
            i = *after_gap;
        else
        {
            auto [c, next_pos] = get_char(loc, drv, text, text.size()-1, i);
            s[j++] = c;
            i = next_pos;
        }
    }
    s.resize(j);

//    std::cerr<<"got string: '"<<s<<"'\n";

    return yy::parser::make_STRING(s,loc);
}
