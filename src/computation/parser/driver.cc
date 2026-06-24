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
        {"_", parser::token::TOK_UNDERSCORE},
        {"as", parser::token::TOK_AS},
        {"anyclass", parser::token::TOK_ANYCLASS},
        {"bpcall", parser::token::TOK_BPCALL},
        {"case", parser::token::TOK_CASE},
        {"class", parser::token::TOK_CLASS},
        {"data", parser::token::TOK_DATA},
        {"default", parser::token::TOK_DEFAULT},
        {"deriving", parser::token::TOK_DERIVING},
        {"do", parser::token::TOK_DO},
        {"ecall", parser::token::TOK_ECALL},
        {"else", parser::token::TOK_ELSE},
        {"family", parser::token::TOK_FAMILY},
        {"forall", parser::token::TOK_FORALL},
        {"foreign", parser::token::TOK_FOREIGN},
        {"hiding", parser::token::TOK_HIDING},
        {"if", parser::token::TOK_IF},
        {"import", parser::token::TOK_IMPORT},
        {"in", parser::token::TOK_IN},
        {"infix", parser::token::TOK_INFIX},
        {"infixl", parser::token::TOK_INFIXL},
        {"infixr", parser::token::TOK_INFIXR},
        {"instance", parser::token::TOK_INSTANCE},
        {"let", parser::token::TOK_LET},
        {"module", parser::token::TOK_MODULE},
        {"newtype", parser::token::TOK_NEWTYPE},
        {"of", parser::token::TOK_OF},
        {"qualified", parser::token::TOK_QUALIFIED},
        {"then", parser::token::TOK_THEN},
        {"trcall", parser::token::TOK_TRCALL},
        {"type", parser::token::TOK_TYPE},
        {"mdo", parser::token::TOK_MDO},
        {"rec", parser::token::TOK_REC},
        {"role", parser::token::TOK_ROLE},
        {"stock", parser::token::TOK_STOCK},
        {"via", parser::token::TOK_VIA},
        {"where", parser::token::TOK_WHERE}
    };

    tight_infix_reserved_symbols = {
        {"@", parser::token::TOK_TIGHT_INFIX_AT},
        {"-", parser::token::TOK_MINUS}
    };

    prefix_reserved_symbols = {
        {"~", parser::token::TOK_PREFIX_TILDE},
        {"!", parser::token::TOK_PREFIX_BANG},
        {"@", parser::token::TOK_PREFIX_AT},
        {"-", parser::token::TOK_PREFIX_MINUS}
    };

    if (lang_exts.has_extension(LangExt::OverloadedRecordDot))
    {
        tight_infix_reserved_symbols.insert({".", parser::token::TOK_TIGHT_INFIX_DOT});
        prefix_reserved_symbols.insert({".", parser::token::TOK_PREFIX_DOT});
    }

    reserved_symbols = {
        {"..", parser::token::TOK_DOTDOT},
        {":", parser::token::TOK_COLON},
        {"::", parser::token::TOK_DCOLON},
        {"=", parser::token::TOK_EQUAL},
        {"\\", parser::token::TOK_LAM},
        {"|", parser::token::TOK_VBAR},
        {"<-", parser::token::TOK_LARROW},
        {"->", parser::token::TOK_RARROW},
        {"=>", parser::token::TOK_DARROW},
        {".", parser::token::TOK_DOT} /* remove this */
    };

    if (not lang_exts.has_extension(LangExt::LexicalNegation))
    {
        reserved_symbols.insert({"-", parser::token::TOK_MINUS});
    }

    if (not lang_exts.has_extension(LangExt::StarIsType))
    {
        reserved_symbols.insert({"*", parser::token::TOK_STAR});
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
