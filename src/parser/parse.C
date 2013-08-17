#define BOOST_SPIRIT_DEBUG
//#define BOOST_SPIRIT_LEXERTL_DEBUG
#include "parse.H"

#include <vector>
#include <map>
#include "util.H"

using std::vector;
using std::map;
using std::string;
using std::endl;

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/spirit/include/phoenix_statement.hpp>
#include <boost/spirit/include/phoenix_container.hpp>
#include <boost/spirit/include/phoenix_algorithm.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/phoenix_bind.hpp>
#include <boost/spirit/include/phoenix_function.hpp>
#include <boost/fusion/include/io.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/foreach.hpp>
#include <functional>
#include <sstream>

#include <boost/spirit/include/lex_lexertl.hpp>
#include <boost/spirit/include/lex_lexertl_position_token.hpp>

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace lex = boost::spirit::lex;


namespace phoenix = boost::phoenix;

template <typename BaseIterator, typename Iterator>
struct myerror_handler
{
  template <typename, typename, typename>
  struct result { typedef void type; };

  myerror_handler(BaseIterator first, BaseIterator last)
    : first(first), last(last) {}

  template <typename Message, typename What>
  void operator()(
		  Message const& message,
		  What const& what,
		  Iterator err_pos) const
  {
    // retrieve underlying iterator from current token
    BaseIterator err_pos_base = err_pos->matched().begin();

    int line;
    BaseIterator line_start = get_pos(err_pos_base, line);
    if (err_pos_base != last)
    {
      std::cout << message << what << " line " << line << ':' << std::endl;
      std::cout << get_line(line_start) << std::endl;
      for (; line_start != err_pos_base; ++line_start)
	std::cout << ' ';
      std::cout << '^' << std::endl;
    }
    else
    {
      std::cout << "Unexpected end of file. ";
      std::cout << message << what << " line " << line << std::endl;
    }
  }

  BaseIterator get_pos(BaseIterator err_pos, int& line) const
  {
    line = 1;
    BaseIterator i = first;
    BaseIterator line_start = first;
    while (i != err_pos)
    {
      bool eol = false;
      if (i != err_pos && *i == '\r') // CR
      {
	eol = true;
	line_start = ++i;
      }
      if (i != err_pos && *i == '\n') // LF
      {
	eol = true;
	line_start = ++i;
      }
      if (eol)
	++line;
      else
	++i;
    }
    return line_start;
  }

  std::string get_line(BaseIterator err_pos) const
  {
    BaseIterator i = err_pos;
    // position i to the next EOL
    while (i != last && (*i != '\r' && *i != '\n'))
      ++i;
    return std::string(err_pos, i);
  }

  BaseIterator first;
  BaseIterator last;
  std::vector<Iterator> iters;
};


//-----------------------------------------------------------------------//
// Handle column numbers
//    - http://stackoverflow.com/questions/8100050/boost-spirit-dynamic-lexer-with-column-numbers
//
// Make a more interpretable program structure:
//    - For parsing
//       + Keep track of the list of operators, and their precedence, to use in resolving infix expressions.
//       + Keep track of how polymorphic functions map to instances.
//       + Keep track of each ADT, and the mapping from constructors -> integers
//    - Try to keep the original ASTs around in a representation that maps back to original source.
//       + It should contain an UNTRANSLATED and unsimplified (e.g. readable) representation of the program source.
//       + It should contain (for example) readable function bodies.
//    - For use as a program object
//       + Perhaps it should be a notes collection?
//       + It should contain a list of identifiers and parameters.
//    - Perhaps add a "parameter name :: type" declaration (top level).
//
// Unify mathematical operators -- at least for now.
//   - We should have only 1 function each for +,*,-,/,neg, etc.
//   

/*
program → { lexeme | whitespace }
lexeme → qvarid | qconid | qvarsym | qconsym
          | literal | special | reservedop | reservedid
literal  → integer | float | char | string
special  → (|)|,|;|[|]|`|{|}
whitespace → whitestuff {whitestuff }
whitestuff → whitechar | comment | ncomment
whitechar → newline | vertab | space | tab | uniWhite
newline → return linefeed | return | linefeed | formfeed
return → a carriage return
linefeed → a line feed
vertab → a vertical tab
formfeed → a form feed
space → a space
tab → a horizontal tab
uniWhite → any Unicode character defined as whitespace
comment → dashes [ any symbol {any} ] newline
dashes → -- {-}
opencom → {-
closecom → -}
ncomment → opencom ANYseq {ncomment ANYseq} closecom
ANYseq → {ANY } {ANY } ( opencom | closecom ) {ANY }
ANY → graphic | whitechar
any → graphic | space | tab
graphic → small | large | symbol | digit | special | " | ’
small → ascSmall | uniSmall | _
ascSmall → a | b | ... | z
uniSmall → any Unicode lowercase letter
large → ascLarge | uniLarge
ascLarge → A | B | ... | Z
uniLarge → any uppercase or titlecase Unicode letter
symbol → ascSymbol | uniSymbol special | _ | " | ’
ascSymbol → !|#|$|%|&|*|+|.|/|<|=|>|?|@
            | \|ˆ|||-| ̃|:
uniSymbol → any Unicode symbol or punctuation
digit → ascDigit | uniDigit
ascDigit → 0 | 1 | ... | 9
uniDigit → any Unicode decimal digit
octit → 0 | 1 | ... | 7
hexit → digit | A | . . . | F | a | . . . | f
*/

bool is_reservedid(const string& id)
{
  return (id == "case" or id=="class" or id=="data" or id=="default" or id=="deriving" or id=="do" or id=="else" or id=="foreign" or id=="if" or id=="import" or id=="in" or id=="infix" or id=="infixl" or id=="infixr" or id=="instance" or id=="let" or id=="module" or id=="newtype" or id=="of" or id=="then" or id=="type" or id=="where" or id=="_");
}

bool is_reservedop(const string& op)
{
  return (op == ".." or op == ":" or op == "::" or op == "=" or op == "\\" or op == "|" or op == "<-" or op == "->" or op == "@" or op == "~" or op == "=>");
}

std::string get_unqualified_name(const std::string&);

typedef boost::spirit::istream_iterator StreamIter;

void fail_if_reserved_id(const StreamIter start, const StreamIter end, BOOST_SCOPED_ENUM(lex::pass_flags)& pass)
{
  if (is_reservedid(std::string(start, end)))
    pass = lex::pass_flags::pass_fail;
}

void fail_if_reserved_qid(const StreamIter start, const StreamIter end, BOOST_SCOPED_ENUM(lex::pass_flags)& pass)
{
  string id = get_unqualified_name(string(start,end));
  if (is_reservedid(id))
    pass = lex::pass_flags::pass_fail;
}

void fail_if_reserved_op(const StreamIter start, const StreamIter end, BOOST_SCOPED_ENUM(lex::pass_flags)& pass)
{
  if (is_reservedop(std::string(start, end)))
    pass = lex::pass_flags::pass_fail;
}

void fail_if_reserved_qop(const StreamIter start, const StreamIter end, BOOST_SCOPED_ENUM(lex::pass_flags)& pass)
{
  string op = get_unqualified_name(string(start,end));
  if (is_reservedop(op))
    pass = lex::pass_flags::pass_fail;
}

// http://www.haskell.org/ghc/docs/6.10.2/html/libraries/haskell-src/Language-Haskell-Lexer.html
template <typename Lexer>
struct HTokens : lex::lexer<Lexer>
{
    HTokens()
    {
/*
program → { lexeme | whitespace }
lexeme → qvarid | qconid | qvarsym | qconsym
          | literal | special | reservedop | reservedid
literal  → integer | float | char | string

opencom → {-
closecom → -}
ncomment → opencom ANYseq {ncomment ANYseq} closecom
ANYseq → {ANY } {ANY } ( opencom | closecom ) {ANY }

*/
      // define patterns (lexer macros) to be used during token definition 
      // below
      this->self.add_pattern
	("newline","\r\n|[\r\n\f]")
	("whitechar","[\n\v \t]")
	// uniWhite
	("ascDigit","\\d+")
	("digit","{ascDigit}")
	// uniDigit
	("octit","[0-7]")
	("hexit","{digit}|[A-F]|[a-f]")
	("ascSymbol","[!#$%&*+./<=>?@\\\\^|\\-~:]")
	// uniSymbol
	("symbol","{ascSymbol}") // | uniSymbol except <special,_,",'>
	("ascSmall","[a-z]")
	// uniSmall
	("small","{ascSmall}|_") // | uniSmall
	("ascLarge","[A-Z]")
	// uniLarge
	("large","{ascLarge}") // | uniLarge
	("special","[(),;[\\]`{}]")
	("graphic","{small}|{large}|{symbol}|{digit}|{special}|[\"']")
	("any","{graphic}|[\t ]")
	("ANY","{graphic}|{whitechar}")
	("dashes","--+")
	("graphicnonsymbol","{small}|{large}|{digit}|{special}|[\"']")
	("graphicnonq", "{small}|{large}|{symbol}|{digit}|{special}|[\"]")
	("graphicnonqq","{small}|{large}|{symbol}|{digit}|{special}|'")
	("anynonsymbol","{graphicnonsymbol}|[\t ]")
	("comment","{dashes}({anynonsymbol}{any}*)?{newline}")
	("whitestuff","{whitechar}|{comment}") // | ncomment
	("whitespace","{whitestuff}+")
	("varid","{small}({small}|{large}|{digit}|')*")
	("conid","{large}({small}|{large}|{digit}|')*")
	("varsym","{symbol}+")
	("consym",":{symbol}*")
	("modid","({conid}\\.)+")
	("decimal","\\d+")
	("exponent","[eE][\\+\\-]?{decimal}")
	// The char and string literals are missing some stuff
	("escape","\\[abfnrtv\\\"']");

      QVarId = "{modid}{varid}";
      VarId = "{varid}";
      QConId = "{modid}{conid}";
      ConId = "{conid}";
      QVarSym = "{modid}{varsym}";
      VarSym = "{varsym}";
      QConSym = "{modid}{consym}";
      ConSym = "{consym}";

	  // Literal
      IntTok = "{decimal}";
      FloatTok = "{digit}\\.{digit}{exponent}?|{digit}{exponent}";
      //      Character = "'({graphicnonq}| |{escape})'";
      Character = "'({graphicnonq}| |{escape})'";
      //      StringTok = "\"({graphicnonqq}| |{escape})*\"";
      StringTok = "[\"]({graphicnonqq}| |{escape})*[\"]";

      LeftParen   = "[(]";
      RightParen  = "[)]";
      SemiColon   = ";";
      LeftCurly   = "\\{";
      RightCurly  = "}";
      VRightCurly = "}";
      LeftSquare  = "\\[";
      RightSquare = "]";
      Comma       = ",";
      BackQuote   = "`";

         // underscore - part of reservedid?
      Underscore = "_";

         // reservedop
      DotDot = "\\.\\.";
      Colon = ":";
      DoubleColon = "::";
      Equals = "=";
      Backslash = "\\\\";
      Bar = "\\|";
      LeftArrow = "<-";
      RightArrow = "->";
      At = "@";
      Tilde = "~";
      DoubleArrow = "=>";
      // Minus and Exclamation are "special" varops
      Minus = "-";
      Exclamation = "!";

         // reservedid
      KW_Case = "case";
      KW_Class = "class";
      KW_Data = "data";
      KW_Default = "default";
      KW_Deriving = "deriving";
      KW_Do = "do";
      KW_Else = "else";
      KW_Foreign = "foreign";
      KW_If = "if";
      KW_Import = "import";
      KW_In = "in";
      KW_Infix = "infix";
      KW_InfixL = "infixl";
      KW_InfixR = "infixr";
      KW_Instance = "instance";
      KW_Let = "let";
      KW_Module = "module";
      KW_NewType = "newtype";
      KW_Of = "of";
      KW_Then = "then";
      KW_Type = "type";
      KW_Where = "where";
      KW_Export = "export";
      KW_Hiding = "hiding";
      KW_Qualified = "qualified";
      KW_Safe = "safe";
      KW_Unsafe = "unsafe";

      KW_Builtin = "builtin";
      KW_External= "external";
      KW_Note = "note";
      KW_Parameter = "parameter";
      KW_Submodel = "submodel";
      ColonEqual = ":=";

      // whitespace
      WHITESPACE = "{whitespace}";

      // define tokens and associate them with the lexer
      //        word = "{WORD}";    // reference the pattern 'WORD' as defined above
      
      // this lexer will recognize 3 token types: words, newlines, and 
      // everything else
      this->self  =
	LeftParen
	| RightParen
	| SemiColon
	| LeftCurly
	| RightCurly
	| VRightCurly // added to satisfy demands of parser?
	| LeftSquare
	| RightSquare
	| Comma
	| BackQuote

	| KW_Builtin
	| KW_External
	| KW_Note
	| KW_Parameter
	| KW_Submodel
	| ColonEqual

	// underscore - part of reservedid?
	| Underscore

	// reservedop
	| DotDot
	| Colon
	| DoubleColon
	| Equals
	| Backslash
	| Bar
	| LeftArrow
	| RightArrow
	| At
	| Tilde
	| DoubleArrow
	| Minus         //?
	| Exclamation   //?

	// reservedid
	| KW_Case
	| KW_Class
	| KW_Data
	| KW_Default
	| KW_Deriving
	| KW_Do
	| KW_Else
	| KW_Foreign
	| KW_If
	| KW_Import
	| KW_In
	| KW_Infix
	| KW_InfixL
	| KW_InfixR
	| KW_Instance
	| KW_Let
	| KW_Module
	| KW_NewType
	| KW_Of
	| KW_Then
	| KW_Type
	| KW_Where
	| KW_Export
	| KW_Hiding
	| KW_Qualified
	| KW_Safe
	| KW_Unsafe

	| QVarId [&fail_if_reserved_qid]
	| VarId
	| QConId
	| ConId
	| QConSym [&fail_if_reserved_qop]
	| ConSym
	| QVarSym [&fail_if_reserved_qop]
	| VarSym

	// Literal
	| IntTok
	| FloatTok
	| Character
	| StringTok
	
	// whitespace
	| WHITESPACE [ lex::_pass = lex::pass_flags::pass_ignore ] // how do we skip whitespace in the lexer?
        ;
    }

  lex::token_def<std::string> QVarId;   // (String,STring)
  lex::token_def<std::string> VarId;    // String
  lex::token_def<std::string> QConId;   // (String, String)	
  lex::token_def<std::string> ConId;    // String	
  lex::token_def<std::string> QVarSym;  // (String, String)	
  lex::token_def<std::string> VarSym;   // String	
  lex::token_def<std::string> QConSym;  // (String, String)	
  lex::token_def<std::string> ConSym;   // String 

  lex::token_def<std::string> IntTok;   // Integer
  lex::token_def<std::string> FloatTok; // Rational	
  lex::token_def<std::string> Character;       // Char	
  lex::token_def<std::string> StringTok;// String	

  lex::token_def<> LeftParen;
  lex::token_def<> RightParen;
  lex::token_def<> SemiColon;
  lex::token_def<> LeftCurly;
  lex::token_def<> RightCurly;
  lex::token_def<> VRightCurly;
  lex::token_def<> LeftSquare;
  lex::token_def<> RightSquare;
  lex::token_def<> Comma;
  lex::token_def<> BackQuote;

  lex::token_def<> Underscore;

  lex::token_def<> DotDot;
  lex::token_def<> Colon;
  lex::token_def<> DoubleColon;
  lex::token_def<> Equals;
  lex::token_def<> Backslash;
  lex::token_def<> Bar;
  lex::token_def<> LeftArrow;
  lex::token_def<> RightArrow;
  lex::token_def<> At;
  lex::token_def<> Tilde;
  lex::token_def<> DoubleArrow;

  lex::token_def<> Minus;
  lex::token_def<> Exclamation;
  // Keywords
  lex::token_def<> KW_Case;
  lex::token_def<> KW_Class;
  lex::token_def<> KW_Data;
  lex::token_def<> KW_Default;
  lex::token_def<> KW_Deriving;
  lex::token_def<> KW_Do;
  lex::token_def<> KW_Else;
  lex::token_def<> KW_Foreign;
  lex::token_def<> KW_If;
  lex::token_def<> KW_Import;
  lex::token_def<> KW_In;
  lex::token_def<> KW_Infix;
  lex::token_def<> KW_InfixL;
  lex::token_def<> KW_InfixR;
  lex::token_def<> KW_Instance;
  lex::token_def<> KW_Let;
  lex::token_def<> KW_Module;
  lex::token_def<> KW_NewType;
  lex::token_def<> KW_Of;
  lex::token_def<> KW_Then;
  lex::token_def<> KW_Type;
  lex::token_def<> KW_Where;
  lex::token_def<> KW_Export;
  lex::token_def<> KW_Hiding;
  lex::token_def<> KW_Qualified;
  lex::token_def<> KW_Safe;
  lex::token_def<> KW_Unsafe;

  lex::token_def<> KW_Builtin;
  lex::token_def<> KW_External;
  lex::token_def<> KW_Note;
  lex::token_def<> KW_Parameter;
  lex::token_def<> KW_Submodel;
  lex::token_def<> ColonEqual;

  //  lex::token_def<std::string> WHITESPACE; For multi-stage lexing, we will actually need the matched string
  lex::token_def<lex::omit> WHITESPACE;
  //  lex::token_def<> EOF;
};

template <typename Iterator>
struct HParser : qi::grammar<Iterator, expression_ref()>
{
  typedef myerror_handler<StreamIter, Iterator> error_handler_type;

  template <typename TokenDef>
    HParser(error_handler_type& error_handler, const TokenDef& tok) 
      : HParser::base_type(exp)
    {
        using qi::lit;
        using qi::lexeme;
	using qi::on_error;
	using qi::fail;
        using ascii::char_;
        using qi::double_;
	using qi::eps;
	using qi::eoi;
        using ascii::string;
        using namespace qi::labels;

        using phoenix::at_c;
        using phoenix::push_back;
	using phoenix::begin;
	using phoenix::end;
	using phoenix::insert;
	using phoenix::clear;
	using phoenix::construct;
	using phoenix::new_;
	using phoenix::val;
	using boost::phoenix::function;

	typedef function<error_handler_type> error_handler_function;

	varid %= tok.VarId;
	qvarid %= tok.VarId | tok.QVarId;
	conid %= tok.ConId;
	qconid %= tok.ConId | tok.QConId;

	varsym = tok.VarSym [_val = _1] | tok.Minus[_val = "-"] | tok.Exclamation [_val = "!"];
	qvarsym %= varsym | tok.QVarSym;
	consym %= tok.ConSym;
	qconsym %= tok.ConSym | tok.QConSym;

	tyvar %= varid;
	tycon %= conid;
	tycls %= conid;
	modid %= qconid;

	qtycon %= qconid;
	qtycls %= qconid;

	//	literal2 = tok.FloatTok [ _val  = _1 ];
	literal = tok.FloatTok [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Float"), _a)  ]
	  | tok.IntTok [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Integer"), _a)  ]
	  | tok.Character [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Char"), _a)  ]
	  | tok.StringTok [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("String"), _a)  ];

	/*----- Section 3 ------*/
	exp = 
	  infixexp [ _val = _1 ] >> -(tok.DoubleColon >> -(context >> tok.DoubleArrow) >> type);

	infixexp = 
	  lexp [push_back(_a,_1)] >> qop [push_back(_a,_1)] >> infixexp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("infixexp"), _a)  ]
	  | eps[clear(_a)] >> tok.Minus [push_back(_a, AST_node("neg"))] >> infixexp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("infixexp"), _a)  ]
	  | lexp [ _val = _1 ]
	  ;

	lexp = 
	  tok.Backslash > +apat[push_back(_a,_1)] > tok.RightArrow > exp[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Lambda"), _a)  ]
	  | tok.KW_Let[clear(_a)] > decls[push_back(_a,_1)] > tok.KW_In > exp[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("Let"), _a)  ]
	  | tok.KW_If [clear(_a)] > exp[push_back(_a,_1)] > -tok.SemiColon >> tok.KW_Then > exp[push_back(_a,_1)] > -tok.SemiColon > tok.KW_Else > exp[push_back(_a,_1) ]>> eps [ _val = new_<expression>(AST_node("If"), _a)  ]
	  | tok.KW_Case[clear(_a)] > exp[push_back(_a,_1)] > tok.KW_Of > tok.LeftCurly >> alts[push_back(_a,_1)] >> tok.RightCurly >> eps [ _val = new_<expression>(AST_node("Case"), _a)  ]
	  | tok.KW_Do[clear(_a)] > tok.LeftCurly >> stmts[push_back(_a,_1)] >> tok.RightCurly [ _val = new_<expression>(AST_node("Do"), _a)  ]
	  | fexp [_val = _1]
	  ;

	fexp = +aexp [ push_back(_a,_1) ] >> eps [ _val = new_<expression>(AST_node("Apply"), _a) ]  ; // function application

	// order: con >> conid >> qconid >qcon >> gcon

	aexp = 
	  // variable
	  qvar [_val = construct<AST_node>("id", construct<String>(_1)) ]
	  // general constructor
	  | gcon [ _val = construct<AST_node>("id", construct<String>(_1)) ]
	  // literal
	  | literal [_val = _1 ]
	  // parenthesized expression
	  | eps[clear(_a)] >> tok.LeftParen >> exp [_val = _1] >> tok.RightParen
	  // tuple, k >= 2
	  | eps[clear(_a)] >> tok.LeftParen >> exp [push_back(_a,_1)] >> +(tok.Comma>>exp [push_back(_a,_1)]) >> tok.RightParen >> eps [ _val = new_<expression>(AST_node("Tuple"), _a) ]
	  // left section
	  | eps[clear(_a)] >> tok.LeftParen >> infixexp[push_back(_a,_1)]  >> qop[push_back(_a,_1)] >> tok.RightParen >> eps [ _val = new_<expression>(AST_node("LeftSection"), _a) ]
	  // right section
	  | eps[clear(_a)] >> tok.LeftParen >> qop[push_back(_a,_1)] - tok.Minus >> infixexp[push_back(_a,_1)] >> tok.RightParen >> eps [ _val = new_<expression>(AST_node("RightSection"), _a) ]
	  // list
	  | tok.LeftSquare[clear(_a)] >> (exp[push_back(_a,_1)]%tok.Comma) >> tok.RightSquare >> eps [ _val = new_<expression>(AST_node("List"), _a) ]
	  // arithmetic sequence
	  | tok.LeftSquare[clear(_a)] >> exp[push_back(_a,_1)] >> tok.DotDot >> tok.RightSquare >> eps [ _val = new_<expression>(AST_node("enumFrom"), _a) ]
	  | tok.LeftSquare[clear(_a)] >> exp[push_back(_a,_1)] >> tok.DotDot >> exp[push_back(_a,_1)] >> tok.RightSquare  >> eps [ _val = new_<expression>(AST_node("enumFromTo"), _a) ]
	  | tok.LeftSquare[clear(_a)] >> exp[push_back(_a,_1)] >> tok.Comma>>exp[push_back(_a,_1)] >>tok.DotDot >> tok.RightSquare  >> eps [ _val = new_<expression>(AST_node("enumFromThen"), _a) ]
	  | tok.LeftSquare[clear(_a)] >> exp[push_back(_a,_1)] >> tok.Comma>>exp[push_back(_a,_1)] >>tok.DotDot >> exp[push_back(_a,_1)] >> tok.RightSquare >> eps [ _val = new_<expression>(AST_node("enumFromThenTo"), _a) ]
	  // list comprehension
	  | tok.LeftSquare[clear(_a)] >> exp[push_back(_a,_1)] >>tok.Bar >> (qual[push_back(_a,_1)]%tok.Comma) >> tok.RightSquare >> eps [ _val = new_<expression>(AST_node("ListComprehension"), _a) ]
	  //	  | qcon >> tok.LeftCurly >> *fbind >> tok.RightCurly  // labeled construction (?)
	  //	  | (aexp - qcon) >> tok.LeftCurly>> +fbind >> tok.RightCurly; // labeled update
	  ;
	  
	/*----- Section 3.2 -------*/
	gcon =  tok.LeftParen >> tok.RightParen [_val = "()"]
	  | tok.LeftSquare >> tok.RightSquare [_val = "[]"]
	  | tok.LeftParen >> tok.Comma [_val = "(,"] >> *tok.Comma[_val += ","] >> tok.RightParen[_val += ")"]
	  | qcon [ _val = _1];

	var  %= varid  | tok.LeftParen >> varsym >> tok.RightParen;    // variable
	qvar %= qvarid | tok.LeftParen >> qvarsym >> tok.RightParen;   // qualified variable
	con  %= conid  | tok.LeftParen >> consym >> tok.RightParen;    // constructor
	qcon %= qconid | tok.LeftParen >> gconsym >> tok.RightParen;   // qualified constructor
	varop %= varsym | tok.BackQuote >> varid >> tok.BackQuote;    // variable operator
	qvarop %= qvarsym | tok.BackQuote >> qvarid >> tok.BackQuote; // qualified variable operator
	conop %= consym | tok.BackQuote >> conid >> tok.BackQuote;    // constructor operator
	qconop %= gconsym | tok.BackQuote >> qconid >> tok.BackQuote; // qualified constructor operator
	op %= varop | conop;                      // operator
	qop = qvarop [ _val = construct<AST_node>("id", construct<String>(_1)) ] | qconop [ _val = construct<AST_node>("id",construct<String>(_1)) ];  // qualified operator
	gconsym = qconsym [_val = _1] | tok.Colon [_val = ":"];

	/*----- Section 3.11 -----*/
	qual = pat [push_back(_a,_1)] >> tok.LeftArrow > exp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("PatQual"), _a) ]
	  | tok.KW_Let >> decls[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("LetQual"), _a) ]
	  | eps [clear(_a) ] >> exp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("SimpleQual"), _a) ];

	/*----- Section 3.13 -----*/
	alts = (alt % tok.SemiColon )[_a = _1] >> eps [ _val = new_<expression>(AST_node("alts"), _a) ];
	alt =  eps [clear(_a) ] >> pat[push_back(_a,_1)] >> tok.RightArrow > exp[push_back(_a,_1)] >> -(tok.KW_Where >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("alt"), _a) ]
	  |  eps [clear(_a) ] >> pat[push_back(_a,_1)] >> gdpat[push_back(_a,_1)] >> -(tok.KW_Where >> decls[push_back(_a,_1)])  >> eps [ _val = new_<expression>(AST_node("alt"), _a) ]
	  | eps;

	gdpat = guards [push_back(_a,_1)]>> tok.RightArrow >> exp[push_back(_a,_1)] >> -gdpat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("GdPat"), _a) ];
	guards = tok.Bar >> +guard[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("Guards"), _a) ];
	guard = pat[push_back(_a,_1)] >> tok.LeftArrow >> infixexp[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("PatternGuard"), _a) ]
	  | eps [clear(_a) ] >> tok.KW_Let >> decls[push_back(_a,_1)] >>  eps [ _val = new_<expression>(AST_node("LetGuard"), _a) ]
	  |  eps [clear(_a) ] >> infixexp[push_back(_a,_1)] >>  eps [ _val = new_<expression>(AST_node("BoolGuard"), _a) ];

	/*----- Section 3.14 -----*/
	stmts = *stmt[push_back(_a,_1)] >> exp[push_back(_a,_1)] >> -tok.SemiColon >> eps [ _val = new_<expression>(AST_node("Stmts"), _a) ];
	stmt =  exp[push_back(_a,_1)] >> tok.SemiColon [ _val = new_<expression>(AST_node("SimpleStmt"), _a) ]
	  | eps [clear(_a) ] >> pat[push_back(_a,_1)] >> tok.LeftArrow >> exp[push_back(_a,_1)] >> tok.SemiColon [ _val = new_<expression>(AST_node("PatStmt"), _a) ]
	  | eps [clear(_a) ] >> tok.KW_Let >> decls[push_back(_a,_1)] >> tok.SemiColon [ _val = new_<expression>(AST_node("LetStmt"), _a) ]
	  | eps [clear(_a) ] >> tok.SemiColon [ _val = new_<expression>(AST_node("EmptyStmt"), _a) ];

	/*----- Section 3.15 -----*/
	//	fbind %= qvar >> tok.Equals >> exp;

	/*----- Section 3.17 -----*/
	pat = 
	  lpat [ push_back(_a,_1) ] >> qconop [ push_back(_a,construct<AST_node>("id",construct<String>(_1))) ] >> pat [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("pat"), _a) ]
	  | eps [clear(_a)] >>  lpat [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("pat"), _a) ];

	lpat = 
	  // negative literal float
	  eps [clear(_a)] >> tok.Minus >> tok.FloatTok [ push_back(_a,construct<String>(_1)) ] >> eps [  _val = new_<expression>(AST_node("neg_h_float"), _a) ]
	  // negative literal integer
	  | eps [clear(_a)] >> tok.Minus >> tok.IntTok [ push_back(_a,construct<String>(_1)) ] >> eps [  _val = new_<expression>(AST_node("neg_h_integer"), _a) ]
	  // here the number of apat's must match the constructor arity
	  | eps [clear(_a)] >>  gcon[ push_back(_a,construct<String>(_1)) ] >> +apat[ push_back(_a,_1) ] >> eps [_val = new_<expression>(AST_node("constructor_pattern"), _a) ]
	  // apat
	  | apat [ _val = _1 ]
	  ;                  

	apat = 
	  // as pattern
	  //	  var >> tok.At>>apat 
	  // irrefutable var pattern
	  var [ _val = construct<AST_node>("apat_var", construct<String>(_1)) ]
	  // arity gcon = 0
	  | gcon [ push_back(_a,construct<String>(_1)) ] >> eps [_val = new_<expression>(AST_node("constructor_pattern"), _a) ]
	  // labelled pattern
	  //	  | qcon >> tok.LeftCurly >> *fpat >> tok.RightCurly     
	  | literal [  _val = _1 ]
	  // wildcard
	  | tok.Underscore [ _val = construct<AST_node>("WildcardPattern") ]                       
	  // parenthesized pattern
	  | tok.LeftParen >> pat [ _val = _1 ] >> tok.RightParen          
	  // tuple patten
	  | eps[clear(_a)] >> tok.LeftParen >> pat[ push_back(_a,_1) ] >> +(tok.Comma >> pat[ push_back(_a,_1) ]) >> tok.RightParen [ _val = new_<expression>(AST_node("Tuple"), _a) ]
	  // list pattern
	  | tok.LeftSquare[clear(_a)] >> pat[ push_back(_a,_1) ] % tok.Comma >> tok.RightSquare [ _val = new_<expression>(AST_node("List"), _a) ]
	  // irrefutable pattern
	  //	  | tok.Tilde >> apat                
	  ;
	//	fpat %= qvar >> tok.Equals >> pat;         // field pattern

	/*------ Section 4 -------*/
	module = 
	  -(tok.KW_Module > modid[ push_back(_a,construct<String>(_1)) ] > /*-exports >>*/ tok.KW_Where) > body [ push_back(_a,_1) ] >> eps[ _val = new_<expression>(AST_node("Module"), _a) ];

	body = 
	  tok.LeftCurly >> impdecls[ push_back(_a,_1) ] >> tok.SemiColon > topdecls[ push_back(_a,_1) ] > tok.RightCurly >> eps[ _val = new_<expression>(AST_node("Body"), _a) ]
	  | tok.LeftCurly[clear(_a)] >> impdecls[ push_back(_a,_1) ] > tok.RightCurly>> eps[ _val = new_<expression>(AST_node("Body"), _a) ]
	  // Since body MUST match wherever it occurs, we can force the last rule to match...
	  | tok.LeftCurly > topdecls [ push_back(_a,_1) ] > tok.RightCurly >> eps[ _val = new_<expression>(AST_node("Body"), _a) ];

	topdecls = topdecl [ push_back(_a,_1) ] % tok.SemiColon >> eps[ _val = new_<expression>(AST_node("TopDecls"), _a) ];
	topdecl = 
	  tok.KW_Type > simpletype[ push_back(_a,_1) ] >> tok.Equals >> type[ push_back(_a,_1) ] >> eps [ _val = new_<expression>(AST_node("Decl:type"), _a) ]
	  | tok.KW_Data > /*-(context >> tok.DoubleArrow) >> */ simpletype[ push_back(_a,_1) ] >> tok.Equals > constrs[ push_back(_a,_1) ] /* >> -deriving */ >> eps [ _val = new_<expression>(AST_node("Decl:data"), _a) ]
	  | tok.KW_Data > /*-(context >> tok.DoubleArrow) >> */ simpletype[ push_back(_a,_1) ] /* >> -deriving */ >> eps [ _val = new_<expression>(AST_node("Decl:data"), _a) ]
	  | tok.KW_NewType > /*-(context >> tok.DoubleArrow) >> */ simpletype [ push_back(_a,_1) ] >> tok.Equals >> newconstr[ push_back(_a,_1) ] /*>> -deriving */ >> eps [ _val = new_<expression>(AST_node("Decl:newtype"), _a) ]
	  //	  | tok.KW_Class >> -(scontext >> tok.DoubleArrow) >> tycls >> tyvar >> -(tok.KW_Where >> cdecls)
	  //	  | tok.KW_Instance >> -(scontext >> tok.DoubleArrow) >> qtycls >> inst >> -(tok.KW_Where >> idecls)
	  //	  | tok.KW_Default >> *type
	  //	  | tok.KW_Foreign >> fdecl
	  | tok.KW_Builtin > (var|varop)[ push_back(_a,construct<String>(_1)) ] >> tok.IntTok[ push_back(_a,construct<String>(_1)) ] >> tok.StringTok[ push_back(_a,construct<String>(_1)) ] >> -tok.StringTok[ push_back(_a,construct<String>(_1)) ] >> eps[ _val = new_<expression>(AST_node("Builtin"), _a) ]
	  | tok.KW_Note > bugs_line [_val = _1]
	  | decl [_val = _1]
	  ;

	decls = tok.LeftCurly > (decl % tok.SemiColon)[_val = new_<expression>(AST_node("Decls"), _1)] > tok.RightCurly;
	decl  = 
	  (funlhs | pat)[push_back(_a,_1)] >> rhs[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Decl"), _a)  ]
	| gendecl [_val = _1];

	// class declarations
	cdecls %= tok.LeftCurly >> cdecl % tok.SemiColon > tok.RightCurly;
	//	cdecl  %= gendecl | (funlhs | var) >> rhs;

	// instance declarations
	idecls %= tok.LeftCurly >> idecl % tok.SemiColon > tok.RightCurly;
	//	idecl  %= (funlhs | var) >> rhs | eps;

	gendecl = // vars >> tok.DoubleColon >>  -(context >> tok.DoubleArrow) >> type 
	  fixity[push_back(_a,construct<String>(_1))] > -tok.IntTok[push_back(_a,construct<String>(_1))] > ops[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("FixityDecl"), _a)  ]
	  | eps [ _val = new_<expression>(AST_node("EmptyDecl"), _a)  ];

	ops = op[push_back(_a,construct<String>(_1))]%tok.Comma >> eps [ _val = new_<expression>(AST_node("Ops"), _a)  ];
	vars %= +var;
	fixity = tok.KW_InfixL [_val = "infixl"] | tok.KW_InfixR [_val = "infixr"] | tok.KW_Infix [_val = "infix" ];

	/*----- Section 4.1.2 ------*/

	type = btype[push_back(_a,_1)] >> tok.RightArrow >> type[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("FunctionType"), _a) ]
	  | btype [_val = _1];

	btype = +atype[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("TypeApply"), _a) ];

	atype = gtycon [_val = construct<AST_node>("type_id",construct<String>(_1)) ]
	  // type variable
	  | tyvar [_val = construct<AST_node>("type_id",construct<String>(_1)) ]
	  // tuple type, k >= 2
	  | eps[clear(_a)] >> tok.LeftParen >> type[push_back(_a,_1)] >> +(tok.Comma>>type[push_back(_a,_1)]) >> tok.RightParen >> eps [ _val = new_<expression>(AST_node("TupleType"), _a) ]
	  // list type
	  | eps[clear(_a)] >> tok.LeftSquare >> type [push_back(_a,_1)] >> tok.RightSquare  >> eps [ _val = new_<expression>(AST_node("ListType"), _a) ]
	  // parenthesized type
	  | eps[clear(_a)] >> tok.LeftParen >> type [_val = _1 ] >> tok.RightParen;
	atype2 = atype [_val = _1] | tok.Exclamation >> atype [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("StrictAtype"), _a) ];

	gtycon = tok.LeftParen >> tok.RightParen [_val = "()"]
	  | tok.LeftSquare >> tok.RightSquare [_val = "[]"]
	  | tok.LeftParen >> tok.RightArrow >> tok.RightParen [_val = "->"]
	  | tok.LeftParen >> tok.Comma [_val = "(,"] >> *tok.Comma[_val += ","] >>tok.RightParen[_val += ")"]
	  | qtycon [ _val = _1];

	/*----- Section 4.1.3 ------*/
	//	context %= h_class | tok.LeftParen >> *h_class >> tok.RightParen;
	//	h_class %= qtycls >> tyvar 
	//        | qtycls >> tok.LeftParen >> tyvar >> +atype >> tok.RightParen;

	/*----- Section 4.2.1 ------*/
	newconstr = con[push_back(_a,_1)] >> atype [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("newconstr"), _a) ];
	  // | con >> tok.LeftCurly >> var >> tok.DoubleColon >> type > tok.RightCurly;
	simpletype = tycon[_val = construct<AST_node>("type_id",construct<String>(_1)) ] >> *tyvar[_val = construct<AST_node>("type_id",construct<String>(_1)) ];
	constrs = constr[push_back(_a,_1)]%tok.Bar >> eps [ _val = new_<expression>(AST_node("constrs"), _a) ];

	constr = con[push_back(_a,construct<String>(_1))] >> *atype2[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("constr"), _a) ]
       	  | (btype | atype2)[push_back(_a,_1)] >> conop[push_back(_a,_1)] >> (btype | atype2)[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("constr_op"), _a) ];
	  //	  | con >> tok.LeftCurly >> *fielddecl > tok.RightCurly;

	fielddecl = vars >> tok.DoubleColon >> (type | tok.Exclamation >> atype);
	//	deriving = tok.KW_Deriving >> (dclass | tok.LeftParen >> tok.RightParen | tok.LeftParen >> dclass%tok.Comma >> tok.RightParen);
	dclass = qtycls;

	/*------ Section 4.3.1 -----*/
	//	scontext %= simpleclass | tok.LeftParen >> tok.RightParen | tok.LeftParen >> simpleclass%tok.Comma >> tok.RightParen;
	//	simpleclass %= qtycls >> tyvar;
	
	/*------ Section 4.3.2 -----*/
	/*
	inst %= 
	  gtycon 
	  | tok.LeftParen >> gtycon >> *tyvar >>tok.RightParen 
	  | tok.LeftParen >> tyvar >> tok.Comma >> tyvar %tok.Comma >> tok.RightParen
	  | tok.LeftSquare >> tyvar >> tok.RightSquare
	  | tyvar >> tok.RightArrow >> tyvar
	  ;
	*/

	/*------ Section 4.4.3 -----*/
	funlhs = var [push_back(_a,construct<AST_node>("id", construct<String>(_1)))] >> +apat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs1"), _a)  ]
	  | eps[clear(_a)] >> pat [push_back(_a,_1)] >> varop[push_back(_a,construct<AST_node>("id", construct<String>(_1)))] >> pat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs2"), _a)  ]
	  | eps[clear(_a)] >> tok.LeftParen > funlhs[push_back(_a,_1)] > tok.RightParen > +apat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs3"), _a)  ];

	rhs = tok.Equals >> exp [push_back(_a,_1)] >> -(tok.KW_Where >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("rhs"), _a)  ]
	  | gdrhs[push_back(_a,_1)] >> -(tok.KW_Where >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("rhs"), _a)  ];

	gdrhs = guards[push_back(_a,_1)] >> tok.Equals >> exp [push_back(_a,_1)] >> -gdrhs[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("gdrhs"), _a)  ];

	/*------ Section 5.1 -------*/
	impdecls = impdecl[push_back(_a,_1)] % tok.SemiColon >> eps [ _val = new_<expression>(AST_node("impdecls"), _a)  ];
	
	/*------ Section 5.2 -------*/
	/*
	exports = tok.LeftParen >> tok.RightParen | tok.LeftParen>> h_export % tok.Comma >> -tok.Comma >> tok.RightParen;
	h_export = 
	  qvar
	  | qtycon >> -(tok.LeftParen >> tok.DotDot >> tok.RightParen | tok.LeftParen >> tok.RightParen | tok.LeftParen >> cname %tok.Comma >> tok.RightParen)
	  | qtycls >> -(tok.LeftParen >> tok.DotDot >> tok.RightParen | tok.LeftParen >> tok.RightParen | tok.LeftParen >> var %tok.Comma >> tok.RightParen)
	  | "module" >> modid
	  ;
	*/
	cname = var | con;
	
	/*------ Section 5.3 -------*/
	impdecl = tok.KW_Import > -tok.KW_Qualified[push_back(_a,"qualified")] 
	                   > -tok.KW_Submodel[push_back(_a,"submodel")] 
	                   > modid[push_back(_a,construct<String>(_1))] 
                           >> -(tok.KW_In[push_back(_a,"as")] > modid[push_back(_a,construct<String>(_1))])
			   >> /*-impspec >>*/ eps [ _val = new_<expression>(AST_node("ImpDecl"), _a)  ];

	//	impspec = 
	//	  tok.LeftParen >> tok.RightParen
	//	  | tok.LeftParen >> import%tok.Comma >> tok.RightParen
	//	  | tok.KW_Hiding >> tok.LeftParen >> import%tok.Comma >> tok.RightParen;

	// FIXME! Parsing problems //
	/*
	import = 
	   var
	  | tycon >> -(tok.LeftParen >> tok.DotDot >> tok.RightParen | tok.LeftParen >> tok.RightParen | tok.LeftParen >> cname %"," >> tok.RightParen)
	  | tycls >> -(tok.LeftParen >> tok.DotDot >> tok.RightParen | tok.LeftParen >> tok.RightParen | tok.LeftParen >> var %"," >> tok.RightParen);
	*/

	/*----------- Rules for notes and related processing - not Haskell --------------*/
	bugs_dist = tok.KW_Data >> exp[push_back(_a,_1)] >> tok.Tilde > exp[push_back(_a,_1)] >>eps [ _val = new_<expression>(AST_node("BugsDataDist"), _a)  ] 
	  | eps [clear(_a) ] >> tok.KW_External >> exp[push_back(_a,_1)] >> tok.Tilde > exp[push_back(_a,_1)] >>eps [ _val = new_<expression>(AST_node("BugsExternalDist"), _a)  ]
	  | eps [clear(_a) ] >> exp[push_back(_a,_1)] >> tok.Tilde > exp[push_back(_a,_1)] >>eps [ _val = new_<expression>(AST_node("BugsDist"), _a)  ];
	bugs_default_value = qvar [push_back(_a, construct<AST_node>("id", construct<String>(_1))) ] >> tok.ColonEqual > exp[push_back(_a,_1)] > eps [ _val = new_<expression>(AST_node("BugsDefaultValue"), _a)  ];
	bugs_note = fexp[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("BugsNote"), _a)  ];
	bugs_parameter = tok.KW_Parameter >> varid [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Parameter"), _a)  ];

	bugs_line %= bugs_parameter | bugs_default_value | bugs_dist | bugs_note;

#define add_error_handler(node) on_error<fail>(node,\
	error_handler_function(error_handler)(\
            "Error! Expecting ", _4, _3));\

	add_error_handler(exp);
	add_error_handler(lexp);
	add_error_handler(aexp);
	add_error_handler(funlhs);
	add_error_handler(gendecl);
	add_error_handler(fixity);
	add_error_handler(rhs);
	add_error_handler(impdecls);
	add_error_handler(impdecl);
	add_error_handler(decls);
	add_error_handler(decl);
	add_error_handler(topdecls);
	add_error_handler(topdecl);
	add_error_handler(module);
	add_error_handler(body);
	add_error_handler(pat);
	add_error_handler(lpat);
	add_error_handler(apat);
	add_error_handler(literal);
	add_error_handler(bugs_line);
	add_error_handler(bugs_dist);
	add_error_handler(bugs_default_value);

	BOOST_SPIRIT_DEBUG_NODE(varid);
	BOOST_SPIRIT_DEBUG_NODE(qvarid);
	BOOST_SPIRIT_DEBUG_NODE(conid);
	BOOST_SPIRIT_DEBUG_NODE(qconid);
	BOOST_SPIRIT_DEBUG_NODE(varsym);
	BOOST_SPIRIT_DEBUG_NODE(qvarsym);
	BOOST_SPIRIT_DEBUG_NODE(consym);
	BOOST_SPIRIT_DEBUG_NODE(qconsym);

	BOOST_SPIRIT_DEBUG_NODE(var);
	BOOST_SPIRIT_DEBUG_NODE(qvar);
	BOOST_SPIRIT_DEBUG_NODE(con);
	BOOST_SPIRIT_DEBUG_NODE(qcon);
	BOOST_SPIRIT_DEBUG_NODE(varop);
	BOOST_SPIRIT_DEBUG_NODE(qvarop);
	BOOST_SPIRIT_DEBUG_NODE(conop);
	BOOST_SPIRIT_DEBUG_NODE(qconop);
	BOOST_SPIRIT_DEBUG_NODE(op);
	BOOST_SPIRIT_DEBUG_NODE(qop);
	BOOST_SPIRIT_DEBUG_NODE(gconsym);
	BOOST_SPIRIT_DEBUG_NODE(gcon);

	BOOST_SPIRIT_DEBUG_NODE(modid);
	BOOST_SPIRIT_DEBUG_NODE(module);
	BOOST_SPIRIT_DEBUG_NODE(body);
	BOOST_SPIRIT_DEBUG_NODE(topdecls);
	BOOST_SPIRIT_DEBUG_NODE(topdecl);

	BOOST_SPIRIT_DEBUG_NODE(exp);
	BOOST_SPIRIT_DEBUG_NODE(lexp);
	BOOST_SPIRIT_DEBUG_NODE(aexp);
	BOOST_SPIRIT_DEBUG_NODE(funlhs);
	BOOST_SPIRIT_DEBUG_NODE(rhs);
	BOOST_SPIRIT_DEBUG_NODE(decl);
	BOOST_SPIRIT_DEBUG_NODE(gendecl);
	BOOST_SPIRIT_DEBUG_NODE(decls);
	BOOST_SPIRIT_DEBUG_NODE(pat);
	BOOST_SPIRIT_DEBUG_NODE(lpat);
	BOOST_SPIRIT_DEBUG_NODE(apat);
	BOOST_SPIRIT_DEBUG_NODE(var);
	BOOST_SPIRIT_DEBUG_NODE(varop);
	BOOST_SPIRIT_DEBUG_NODE(constrs);
	BOOST_SPIRIT_DEBUG_NODE(constr);
	BOOST_SPIRIT_DEBUG_NODE(con);

	BOOST_SPIRIT_DEBUG_NODE(type);
	BOOST_SPIRIT_DEBUG_NODE(atype);
	BOOST_SPIRIT_DEBUG_NODE(atype2);
	BOOST_SPIRIT_DEBUG_NODE(btype);
	BOOST_SPIRIT_DEBUG_NODE(gtycon);
	BOOST_SPIRIT_DEBUG_NODE(qtycon);

	modid.name("modid");
	exp.name("exp");
	infixexp.name("infixexp");
	lexp.name("lexp");
	fexp.name("fexp");
	aexp.name("aexp");
	qual.name("qual");
	alts.name("alts");
	alt.name("alt");
	gdpat.name("gdpat");
	guards.name("guards");
	guard.name("guard");
	stmts.name("stmts");
	stmt.name("stmt");
	funlhs.name("funlhs");
	apat.name("apat");
	lpat.name("lpat");
	pat.name("pat");
	module.name("module");
	body.name("body");
	rhs.name("rhs");
	decls.name("decls");
	decl.name("decl");
	topdecls.name("topdecls");
	topdecl.name("topdecl");
	impdecls.name("impdecls");
	impdecl.name("impdecl");
	gendecl.name("gendecl");
	ops.name("ops");
	fixity.name("fixity");
	gcon.name("gcon");
	literal.name("literal");

	bugs_parameter.name("bugs_parameter");
	bugs_default_value.name("bugs_default_value");
	bugs_dist.name("bugs_dist");
	bugs_note.name("bugs_note");
    }
  qi::rule<Iterator, std::string()> varid;
  qi::rule<Iterator, std::string()> qvarid;
  qi::rule<Iterator, std::string()> conid;
  qi::rule<Iterator, std::string()> qconid;

  qi::rule<Iterator, std::string()> varsym;
  qi::rule<Iterator, std::string()> qvarsym;
  qi::rule<Iterator, std::string()> consym;
  qi::rule<Iterator, std::string()> qconsym;

  qi::rule<Iterator, std::string()> tyvar;
  qi::rule<Iterator, std::string()> tycon;
  qi::rule<Iterator, std::string()> tycls;
  qi::rule<Iterator, std::string()> modid; // module id

  qi::rule<Iterator, std::string()> qtycon; // qualified type constructor
  qi::rule<Iterator, std::string()> qtycls; // qualified type class

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> literal;  

  qi::rule<Iterator, expression_ref()> exp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> infixexp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> lexp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> fexp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> aexp;

  /*----- Section 3.2 -------*/
  qi::rule<Iterator, std::string()> gcon;
  qi::rule<Iterator, std::string()> var;
  qi::rule<Iterator, std::string()> qvar;
  qi::rule<Iterator, std::string()> con;
  qi::rule<Iterator, std::string()> qcon;
  qi::rule<Iterator, std::string()> varop;
  qi::rule<Iterator, std::string()> qvarop;
  qi::rule<Iterator, std::string()> conop;
  qi::rule<Iterator, std::string()> qconop;
  qi::rule<Iterator, std::string()> op;
  qi::rule<Iterator, std::string()> gconsym;
  qi::rule<Iterator, expression_ref()> qop;

  /*----- Section 3.11 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> qual;  

  /*----- Section 3.13 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> alts;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> alt;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> gdpat;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> guards;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> guard;

  /*----- Section 3.14 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> stmts;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> stmt;

  /*----- Section 3.15 -----*/
  qi::rule<Iterator, std::string()> fbind;  

  /*----- Section 3.17 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> pat;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> lpat;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> apat;  
  qi::rule<Iterator, std::string()> fpat;  

  /*----- Section 4 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> module;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> body;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> topdecls;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> topdecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> decls;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> decl;

  qi::rule<Iterator, std::string()> cdecls;
  qi::rule<Iterator, std::string()> cdecl;

  qi::rule<Iterator, std::string()> idecls;
  qi::rule<Iterator, std::string()> idecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> gendecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> ops;
  qi::rule<Iterator, std::string()> vars;
  qi::rule<Iterator, std::string()> fixity;

  /*----- Section 4.1.2 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> type;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> btype;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> atype;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> atype2;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> gtype;
  //  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> gtycon;
  qi::rule<Iterator, std::string()> gtycon;

  /*----- Section 4.1.3 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> context;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> h_class;

  /*----- Section 4.2.1 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> newconstr;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> simpletype;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> constrs;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> constr;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> fielddecl;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> deriving;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> dclass;

  /*----- Section 4.3.1 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> scontext;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> simpleclass;

  /*----- Section 4.3.2 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> inst;

  /*----- Section 4.4.3 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> funlhs;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> rhs;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> gdrhs;

  /*----- Section 5.1 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> impdecls;

  /*----- Section 5.2 ------*/
  qi::rule<Iterator, std::string()> exports;
  qi::rule<Iterator, std::string()> h_export;
  qi::rule<Iterator, std::string()> cname;

  /*----- Section 5.3 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> impdecl;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> impspec;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> import;

  /*----------- Rules for notes and related processing - not Haskell --------------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> bugs_dist;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> bugs_default_value;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> bugs_note;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> bugs_parameter;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> bugs_line;

};

//-----------------------------------------------------------------------//
typedef lex::lexertl::position_token<StreamIter,
				     boost::mpl::vector<int,char,std::string>
				     > Token;

typedef lex::lexertl::actor_lexer<Token> Lexer;

HTokens<Lexer> lexer1;          // Our lexer

expression_ref parse_haskell_line(const string& line)
{
  std::stringstream line_stream(line);
  line_stream.unsetf(std::ios::skipws);

  StreamIter beg = StreamIter(line_stream), end;

  HParser<HTokens<Lexer>::iterator_type>::error_handler_type error_handler(beg,end);
  HParser<HTokens<Lexer>::iterator_type> haskell_parser(error_handler,lexer1);

  {
    for(auto i = lexer1.begin(beg, end); i != lexer1.end() and (*i).is_valid(); i++)
    {
      auto& t = *i;
      std::cout<<"'"<<t.value()<<"'\n";;
    }
  }

  /*----------------------------------------------------------------------------*/
  expression_ref cmd;

  StreamIter iter = beg;
  if (not tokenize_and_parse(iter, end, lexer1, haskell_parser, cmd))
    throw myexception()<<"Haskell line parse failed!";

  if (iter != end)
    throw myexception()<<"Haskell line parse only parsed:\n "<<string(beg,iter)<<"\n";

  return cmd;
}

expression_ref parse_haskell_decls(const string& line)
{
  std::stringstream line_stream(line);
  line_stream.unsetf(std::ios::skipws);

  StreamIter beg = StreamIter(line_stream), end;

  HParser<HTokens<Lexer>::iterator_type>::error_handler_type error_handler(beg,end);
  HParser<HTokens<Lexer>::iterator_type> haskell_parser(error_handler,lexer1);

  {
    for(auto i = lexer1.begin(beg, end); i != lexer1.end() and (*i).is_valid(); i++)
    {
      auto& t = *i;
      std::cout<<"'"<<t.value()<<"'\n";;
    }
  }

  /*----------------------------------------------------------------------------*/

  expression_ref cmd;
  StreamIter iter = beg;
  if (not tokenize_and_parse(iter, end, lexer1, haskell_parser.decls, cmd))
    throw myexception()<<"Haskell decls parse failed!";

  if (iter != end)
    throw myexception()<<"Haskell decls parse only parsed:\n "<<string(beg,iter)<<"\n";

  return cmd;
}

expression_ref parse_bugs_line(const string& line)
{
  std::stringstream line_stream(line);
  line_stream.unsetf(std::ios::skipws);

  StreamIter beg = StreamIter(line_stream), end;

  HParser<HTokens<Lexer>::iterator_type>::error_handler_type error_handler(beg,end);
  HParser<HTokens<Lexer>::iterator_type> haskell_parser(error_handler,lexer1);

  {
    for(auto i = lexer1.begin(beg, end); i != lexer1.end() and (*i).is_valid(); i++)
    {
      auto& t = *i;
      std::cout<<"'"<<t.value()<<"'\n";;
    }
  }

  /*----------------------------------------------------------------------------*/

  expression_ref cmd;
  StreamIter iter = beg;
  if (not tokenize_and_parse(iter, end, lexer1, haskell_parser.bugs_line, cmd))
    throw myexception()<<"HBUGS line parse failed!";

  if (iter != end)
    throw myexception()<<"HBUGS line parse only parsed:\n "<<string(beg,iter)<<"\n";

  return cmd;
}

expression_ref parse_bugs_file(const string& lines)
{
  std::stringstream line_stream(lines);
  line_stream.unsetf(std::ios::skipws);

  StreamIter beg = StreamIter(line_stream), end;

  HParser<HTokens<Lexer>::iterator_type>::error_handler_type error_handler(beg,end);
  HParser<HTokens<Lexer>::iterator_type> haskell_parser(error_handler,lexer1);

  /*
  {
    std::stringstream line_stream(lines);
    line_stream.unsetf(std::ios::skipws);

    StreamIter beg = StreamIter(line_stream), end;

    HParser<HTokens<Lexer>::iterator_type>::error_handler_type error_handler(beg,end);
    for(auto i = lexer1.begin(beg,end); i != lexer1.end() and (*i).is_valid(); i++)
    {
      auto& t = *i;
      std::cout<<"'"<<t.value()<<"'\n";;
    }
    // Sharing the same stream might not work.
  }
  */
  /*----------------------------------------------------------------------------*/

  expression_ref cmd;
  StreamIter iter = beg;
  if (not tokenize_and_parse(iter, end, lexer1, haskell_parser.module, cmd))
    throw myexception()<<"Module parse failed!";

  std::cerr<<"cmd = "<<cmd<<std::endl;

  if (iter != end)
    throw myexception()<<"Haskell module parse only parsed:\n "<<string(beg,iter)<<"\n";

  return cmd;
}
