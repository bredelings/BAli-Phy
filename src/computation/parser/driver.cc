#include "driver.hh"
#include "parser.hh"
#include "util/myexception.H"
#include "util/io.H"

using std::string;
using std::set;

void driver::pop_context()
{
    if (contexts.empty())
	throw myexception()<<"Trying to pop empty context!";
    contexts.pop_back();
}

std::optional<LayoutContext> driver::get_context()
{
    if (contexts.empty())
	return {};
    return contexts.back();
}

void driver::push_context(const std::optional<LayoutContext>& lc)
{
    contexts.push_back(lc);
}

void driver::push_context(const LayoutContext& lc)
{
    contexts.push_back(lc);
}

void driver::push_module_context()
{
//    std::cerr<<"running push_module_context\n";
    contexts.push_back(LayoutContext{1,true});
}

void driver::push_context()
{
    contexts.push_back({});
}

LayoutContext driver::get_offside(const yy::parser::location_type& loc)
{
    int offset = loc.end.column;
    if (auto layout_context = get_context())
	return {offset - layout_context->offset, layout_context->gen_semis};
    else
	return {1,false};
}

void driver::push_warning_message(const location_type& loc, const Note& w)
{
//    std::cerr<<"Pushing warning message '"<<e.second<<"' at "<<e.first<<"\n";
    messages.push_back(Message{WarningMsg, loc, {w}});
}

void driver::push_error_message(const location_type& loc, const Note& e)
{
//    std::cerr<<"Pushing error message '"<<e.second<<"' at "<<e.first<<"\n";
    messages.push_back(Message{ErrorMsg, loc, {e}});
}

void driver::pop_error_message()
{
//    std::cerr<<"Popping error message\n";
    if (messages.empty())
	throw myexception()<<"No message to pop!";
    messages.pop_back();
}

driver::driver (const LanguageExtensions& exts)
    : lang_exts(exts), trace_parsing (false), trace_scanning (false)
{
    using namespace yy;

    reserved_words = {
        {"_",{parser::token::TOK_UNDERSCORE,0}},
        {"as",{parser::token::TOK_AS,0}},
        {"bpcall",{parser::token::TOK_BPCALL,0}},
        {"case",{parser::token::TOK_CASE,0}},
        {"class",{parser::token::TOK_CLASS,0}},
        {"data",{parser::token::TOK_DATA,0}},
        {"default",{parser::token::TOK_DEFAULT,0}},
        {"deriving",{parser::token::TOK_DERIVING,0}},
        {"do",{parser::token::TOK_DO,0}},
        {"ecall",{parser::token::TOK_ECALL,0}},
        {"else",{parser::token::TOK_ELSE,0}},
        {"family",{parser::token::TOK_FAMILY,0}},
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
        {"trcall",{parser::token::TOK_TRCALL,0}},
        {"type",{parser::token::TOK_TYPE,0}},
        {"where",{parser::token::TOK_WHERE,0}},
        {"mdo", {parser::token::TOK_MDO,0}},
        {"rec", {parser::token::TOK_REC,0}}
    };

    tight_infix_reserved_symbols = {
        {"@",{parser::token::TOK_TIGHT_INFIX_AT,0}},
        {"-",{parser::token::TOK_MINUS,0}}
    };

    prefix_reserved_symbols = {
        {"~",{parser::token::TOK_PREFIX_TILDE,0}},
        {"!",{parser::token::TOK_PREFIX_BANG,0}},
        {"@",{parser::token::TOK_PREFIX_AT,0}},
        {"-",{parser::token::TOK_PREFIX_MINUS,0}}
    };

    if (lang_exts.has_extension(LangExt::OverloadedRecordDot))
    {
        tight_infix_reserved_symbols.insert({".",{parser::token::TOK_TIGHT_INFIX_DOT,0}});
        prefix_reserved_symbols.insert({".",{parser::token::TOK_PREFIX_DOT,0}});
    }

    reserved_symbols = {
        {"..",{parser::token::TOK_DOTDOT,0}},
        {":",{parser::token::TOK_COLON,0}},
        {"::",{parser::token::TOK_DCOLON,0}},
        {"=",{parser::token::TOK_EQUAL,0}},
        {"\\",{parser::token::TOK_LAM,0}},
        {"|",{parser::token::TOK_VBAR,0}},
        {"<-",{parser::token::TOK_LARROW,0}},
        {"->",{parser::token::TOK_RARROW,0}},
        {"=>",{parser::token::TOK_DARROW,0}},
        {".",{parser::token::TOK_DOT,0}} /* remove this */
    };

    if (not lang_exts.has_extension(LangExt::LexicalNegation))
    {
        reserved_symbols.insert({"-",{parser::token::TOK_MINUS,0}});
    }

    if (not lang_exts.has_extension(LangExt::StarIsType))
    {
        reserved_symbols.insert({"*",{parser::token::TOK_STAR,0}});
    }
}

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

int
driver::parse_file (const std::string &filename)
{
  string file_contents = read_file(filename,"module");
  return parse_string(file_contents, filename);
}

int
driver::parse_string (const string& file_contents, const std::string &input_name)
{
  file = input_name;
  location.initialize (&input_name);
  scan_begin (file_contents);
  yy::parser parser (*this);
  parser.set_debug_level (trace_parsing);
  int res = parser.parse ();
  scan_end ();

  show_messages( {input_name, file_contents}, std::cerr, messages);
  exit_on_error(messages);

  return res;
}

Haskell::Module parse_module_file(const string& content, const std::string& input_name, const LanguageExtensions& lang_exts)
{
    driver D(lang_exts);
    D.parse_string(content, input_name);
    return D.result;
}
