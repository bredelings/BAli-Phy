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
#include <boost/fusion/include/io.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/foreach.hpp>
#include <functional>

#include <boost/spirit/include/lex_lexertl.hpp>

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace lex = boost::spirit::lex;


namespace phoenix = boost::phoenix;
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

int fail_if_reservedid(const string& id)
{
  if (id == "case" or id=="class" or id=="data" or id=="default" or id=="deriving" or id=="do" or id=="else" or id=="foreign" or id=="if" or id=="import" or id=="in" or id=="infix" or id=="infixl" or id=="infixr" or id=="instance" or id=="let" or id=="module" or id=="newtype" or id=="of" or id=="then" or id=="type" or id=="where" or id=="_")
    return 0; // pass_fail
  else
    return 1; // pass__normal
}

int fail_if_reservedop(const string& op)
{
  if (op == ".." or op == ":" or op == "::" or op == "=" or op == "\\" or op == "|" or op == "<-" or op == "->" or op == "@" or op == "~" or op == "=>")
    return 0;
  else
    return 1;  
}

// http://www.haskell.org/ghc/docs/6.10.2/html/libraries/haskell-src/Language-Haskell-Lexer.html
template <typename Lexer>
struct word_count_tokens : lex::lexer<Lexer>
{
    word_count_tokens()
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
      string newline = "\r\n|[\r\n\f]";
      string whitechar = "[\n\v \t]"; // uniWhite
      string uniWhite = "??";
      string ascDigit = "\\d+";
      string uniDigit = "??";
      string digit = ascDigit; // | uniDigit
      string octit = "[0-7]";
      string hexit = digit+"|[A-F]|[a-f]";
      string ascSymbol = "[!#$%&*+./<=>?@\\\\ˆ|\\-~:]";
      string uniSymbol = "??";
      string symbol = ascSymbol; //uniSymbol except <special,_,",'>
      string ascSmall = "[a-z]";
      string uniSmall = "??";
      string small = ascSmall; // | uniSmall
      string ascLarge = "[A-Z]";
      string uniLarge = "??";
      string large = ascLarge; // | uniLarge
      string special = "[(),;[\\]`{}]";
      string graphic = small +"|"+ large +"|"+ symbol +"|"+ digit +"|"+ special + "|[\"']";
      string any = graphic + "|[\t ]";
      string ANY = graphic +"|"+ whitechar;
      string dashes = "--+";
      string comment = dashes + "((?!"+symbol+")"+"("+any+")+)?" + newline;
      string whitestuff = whitechar +"|"+ comment; // | ncomment
      string whitespace = "(" + whitestuff + ")+";
      string varid = "("+small+")" + "("+small+"|"+large+"|"+digit+"|'" + ")*";// - reservedid;
      string conid = "("+large+")" + "("+small+"|"+large+"|"+digit+"|'" + ")*";
      string varsym = "(?!:)("+symbol+")+"; // -reservedop
      string consym = ":("+symbol+")*"; //-reservedop;
      string modid = "(("+conid+")\\.)+";
      string qvarid = "("+modid+")("+varid+")";
      string qconid = "("+modid+")("+conid+")";
      string qvarsym = "("+modid+")("+varsym+")";
      string qconsym = "("+modid+")("+consym+")";
      string int_literal = "\\d+";
      string exponent = "[eE][\\+-]?\\d+";
      string float_literal = "(\\d+\\.\\d+("+exponent+")?)|\\d+"+exponent;
      // The char and string literals could be improved
      string escape = "\\[abfnrtv\\\"']";
      string char_literal = "'(((?!['\\])"+graphic+")| |("+escape+"))'";
      string string_literal = "\"(((?![\"\\])"+graphic+")| |("+escape+"))*\"";
      
        // define patterns (lexer macros) to be used during token definition 
        // below
        this->self.add_pattern
	  ("WORD", "[^ \t\n]+")
	  ("ID",".")
        ;

        // define tokens and associate them with the lexer
        word = "{WORD}";    // reference the pattern 'WORD' as defined above

        // this lexer will recognize 3 token types: words, newlines, and 
        // everything else
        this->self 
	  = QVarId
	  | VarId
	  | QConId
	  | ConId
	  | QVarSym
	  | VarSym
	  | QConSym
	  | ConSym
	  | IntTok
	  | FloatTok
	  | Character
	  | StringTok
	  | WHITESPACE
        ;
    }

  lex::token_def<std::string> QVarId;   // (String,STring)
  lex::token_def<std::string> VarId;    // String
  lex::token_def<std::string> ConId;    // String	
  lex::token_def<std::string> QConId;   // (String, String)	
  lex::token_def<std::string> VarSym;   // String	
  lex::token_def<std::string> ConSym;   // String	
  lex::token_def<std::string> QVarSym;  // (String, String)	
  lex::token_def<std::string> QConSym;  // (String, String)	
  lex::token_def<std::string> IntTok;   // Integer
  lex::token_def<std::string> FloatTok; // Rational	
  lex::token_def<char> Character;       // Char	
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
  lex::token_def<> Underscore;
  lex::token_def<> BackQuote;
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
  lex::token_def<> KW_As;
  lex::token_def<> KW_Export;
  lex::token_def<> KW_Hiding;
  lex::token_def<> KW_Qualified;
  lex::token_def<> KW_Safe;
  lex::token_def<> KW_Unsafe;
  lex::token_def<std::string> WHITESPACE;
  //  lex::token_def<> EOF;
    // the token 'word' exposes the matched string as its parser attribute
  lex::token_def<std::string> word;
  lex::token_def<> id;
};


template <typename Iterator>
struct haskell_grammar : qi::grammar<Iterator, expression_ref(), ascii::space_type>
{
    haskell_grammar() : haskell_grammar::base_type(exp)
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

	small %= char_("a-z") | char_('_');
	large %= char_("A-Z");
	digit %= char_("0-9");
	symbol %= char_("!#$%&*+./<=>?@\\^|~:") | char_('-');
	special %= char_("(),;[]`{}");
	graphic %= small | large | symbol | digit | special | char_('"') | char_('\'');

	dashes %= lit("--")>>*lit("-");

	varid %= (small>>(*(small|large|digit|char_('\'')))) - reservedid;
	conid %= large>>(*(small|large|digit|char_('\'')));
	reservedid_ %= lit("case") | "class" | "data" | "default" | "deriving" | "do" | "else" |	"foreign" | "if" | "import" | "in" | "infix" | "infixl" | 	"infixr" | "instance" | "let" | "module" | "newtype" | "of" | 	"then" | "type" | "where" | "_";
	reservedid %= reservedid_ >> !(small|large|digit|'\'');

	// here, we need to match "==", but not "="
	varsym %= ((symbol-lit(':'))>>*symbol)-reservedop-dashes;
	consym %= (char_(':')>>*symbol)-reservedop;
	reservedop_ %= string("..") | string(":") | string("::") | string("=") | string("\\") | string("|") | string("<-") | string("->") | string("@") | string("~") | string("=>");
	reservedop %= reservedop_ >> !symbol;

	tyvar %= varid;
	tycon %= conid;
	tycls %= conid;
	modid %= conid>>*(string(".")>>conid);

	//	qvarid %= -(modid>>char_('.')) >> varid;
	qvarid %= *(conid>>char_('.'))>>varid;
	//	qconid %= -(modid>>char_('.')) >> conid;
	qconid %= conid>>*(char_('.')>>conid);
	//	qtycon %= -(modid>>char_('.')) >> tycon;
	qtycon %= conid>>*(char_('.')>>conid);
	//	qtycls %= -(modid>>char_('.')) >> tycls;
	qtycls %= conid>>*(char_('.')>>conid);
	//	qvarsym %= -(modid>>char_('.')) >> varsym;
	qvarsym %= *(conid>>char_('.')) >> varsym;
	//	qconsym %= -(modid>>char_('.')) >> consym;
	qconsym %= *(conid>>char_('.')) >> consym;

	decimal %= +char_("0-9");
	h_integer %= decimal;

	h_float %= decimal >> char_('.') >> decimal >> -exponent | decimal >> exponent;
	exponent %= ( char_('e') | char_('E') ) >> -(char_('+')|char_('-')) >> decimal;

	h_char %= lit('\'') >> ((graphic - '\'' - '\\')|char_(' ')|escape) >> lit('\'');
	h_string %= lit('"') >> *((graphic - '"' - '\\')|char_(' ')|escape) >> lit('"');
	escape = lit("\\a") [_val = '\a'] |
	  lit("\\b") [_val = '\b'] |
	  lit("\\f") [_val = '\f'] |
	  lit("\\n") [_val = '\n'] |
	  lit("\\r") [_val = '\r'] |
	  lit("\\t") [_val = '\t'] |
	  lit("\\v") [_val = '\v'] |
	  lit("\\\\") [_val = '\\'] |
	  lit("\\\"") [_val = '"'] |
	  lit("'") [_val = '\''];

	literal = h_float [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Float"), _a)  ]
	  | h_integer [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Integer"), _a)  ]
	  | h_char [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Char"), _a)  ]
	  | h_string [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("String"), _a)  ];

	/*----- Section 3 ------*/
	exp = 
	  infixexp [ _val = _1 ] >> "::" >> -(context >> "=>") >> type 
	  | infixexp [_val = _1 ];

	infixexp = 
	  lexp [push_back(_a,_1)] >> qop [push_back(_a,_1)] >> infixexp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("infixexp"), _a)  ]
	  | eps[clear(_a)] >> lit("-") [push_back(_a, AST_node("neg"))] >> infixexp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("infixexp"), _a)  ]
	  | lexp [ _val = _1 ]
	  ;

	lexp = 
	  lit("\\") > +apat[push_back(_a,_1)] > lit("->") > exp[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Lambda"), _a)  ]
	  | fexp [_val = _1]
	  | lit("let")[clear(_a)] > decls[push_back(_a,_1)] > "in" > exp[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("Let"), _a)  ]
	  | lit("if")[clear(_a)] > exp[push_back(_a,_1)] > -lit(';') >> "then" > exp[push_back(_a,_1)] > -lit(';') > "else" > exp[push_back(_a,_1) ]>> eps [ _val = new_<expression>(AST_node("If"), _a)  ]
	  | lit("case")[clear(_a)] > exp[push_back(_a,_1)] > "of" > "{" >> alts[push_back(_a,_1)] >> "}" >> eps [ _val = new_<expression>(AST_node("Case"), _a)  ]
	  | lit("do")[clear(_a)] > "{" >> stmts[push_back(_a,_1)] >> lit("}") [ _val = new_<expression>(AST_node("Do"), _a)  ]
	  ;

	fexp = +aexp [ push_back(_a,_1) ] >> eps [ _val = new_<expression>(AST_node("Apply"), _a) ]  ; // function application

	// order: con >> conid >> qconid >qcon >> gcon

	aexp = 
	  // variable
	  qvar [_val = phoenix::construct<AST_node>("id", construct<String>(_1)) ]
	  // general constructor
	  | gcon [ _val = phoenix::construct<AST_node>("id", _1) ]
	  // literal
	  | literal [_val = _1 ]
	  // parenthesized expression
	  | "(" >> exp [_val = _1] >> ")"
	  // tuple, k >= 2
	  | "(" >> exp [push_back(_a,_1)] >> +(','>>exp [push_back(_a,_1)]) >> ")" >> eps [ _val = new_<expression>(AST_node("Tuple"), _a) ]
	  // list
	  | lit("[")[clear(_a)] >> (exp[push_back(_a,_1)]%',') >> "]" >> eps [ _val = new_<expression>(AST_node("List"), _a) ]
	  // arithmetic sequence
	  | lit("[")[clear(_a)] >> exp[push_back(_a,_1)] >> ".." >> "]" >> eps [ _val = new_<expression>(AST_node("enumFrom"), _a) ]
	  | lit("[")[clear(_a)] >> exp[push_back(_a,_1)] >> ".." >> exp[push_back(_a,_1)] >> "]"  >> eps [ _val = new_<expression>(AST_node("enumFromTo"), _a) ]
	  | lit("[")[clear(_a)] >> exp[push_back(_a,_1)] >> ','>>exp[push_back(_a,_1)] >>".." >> "]"  >> eps [ _val = new_<expression>(AST_node("enumFromThen"), _a) ]
	  | lit("[")[clear(_a)] >> exp[push_back(_a,_1)] >> ','>>exp[push_back(_a,_1)] >>".." >> exp[push_back(_a,_1)] >> "]" >> eps [ _val = new_<expression>(AST_node("enumFromThenTo"), _a) ]
	  // list comprehension
	  | lit("[")[clear(_a)] >> exp[push_back(_a,_1)] >>"|" >> (qual[push_back(_a,_1)]%',') >> "]" >> eps [ _val = new_<expression>(AST_node("ListComprehension"), _a) ]
	  // left section
	  | lit("(")[clear(_a)] >> infixexp[push_back(_a,_1)]  >> qop[push_back(_a,_1)] >> ")" >> eps [ _val = new_<expression>(AST_node("LeftSection"), _a) ]
	  // right section
	  | lit("(")[clear(_a)] >> ((qop[push_back(_a,_1)] - "-") >> infixexp[push_back(_a,_1)]) >> ")" >> eps [ _val = new_<expression>(AST_node("RightSection"), _a) ]
	  //	  | qcon >> "{" >> *fbind >> "}"  // labeled construction (?)
	  //	  | (aexp - qcon) >> "{">> +fbind >> "}"; // labeled update
	  ;
	  
	/*----- Section 3.2 -------*/
	gcon %= string("()") | string("[]") | string("(,") >> *char_(',')>>string(")") | qcon;

	var  %= varid  | "(" >> varsym >> ")";    // variable
	qvar %= qvarid | "(" >> qvarsym >> ")";   // qualified variable
	con  %= conid  | "(" >> consym >> ")";    // constructor
	qcon %= qconid | "(" >> gconsym >> ")";   // qualified constructor
	varop %= varsym | "`" >> varid >> "`";    // variable operator
	qvarop %= qvarsym | "`" >> qvarid >> "`"; // qualified variable operator
	conop %= consym | "`" >> conid >> "`";    // constructor operator
	qconop %= gconsym | "`" >> qconid >> "`"; // qualified constructor operator
	op %= varop | conop;                      // operator
	qop = qvarop [ _val = construct<AST_node>("id", construct<String>(_1)) ] | qconop [ _val = construct<AST_node>("id",construct<String>(_1)) ];  // qualified operator
	gconsym %= qconsym | string(":");

	/*----- Section 3.11 -----*/
	qual = pat [push_back(_a,_1)] >> "<-" > exp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("PatQual"), _a) ]
	  | lit("let") >> decls[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("LetQual"), _a) ]
	  | eps [clear(_a) ] >> exp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("SimpleQual"), _a) ];

	/*----- Section 3.13 -----*/
	alts = (alt % ';' )[_a = _1] >> eps [ _val = new_<expression>(AST_node("alts"), _a) ];
	alt =  eps [clear(_a) ] >> pat[push_back(_a,_1)] >> "->" > exp[push_back(_a,_1)] >> -("where" >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("alt"), _a) ]
	  |  eps [clear(_a) ] >> pat[push_back(_a,_1)] >> gdpat[push_back(_a,_1)] >> -("where" >> decls[push_back(_a,_1)])  >> eps [ _val = new_<expression>(AST_node("alt"), _a) ]
	  | eps;

	gdpat = guards [push_back(_a,_1)]>> "->" >> exp[push_back(_a,_1)] >> -gdpat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("GdPat"), _a) ];
	guards = "|" >> +guard[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("Guards"), _a) ];
	guard = pat[push_back(_a,_1)] >> "<-" >> infixexp[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("PatternGuard"), _a) ]
	  | eps [clear(_a) ] >> "let" >> decls[push_back(_a,_1)] >>  eps [ _val = new_<expression>(AST_node("LetGuard"), _a) ]
	  |  eps [clear(_a) ] >> infixexp[push_back(_a,_1)] >>  eps [ _val = new_<expression>(AST_node("BoolGuard"), _a) ];

	/*----- Section 3.14 -----*/
	stmts = *stmt[push_back(_a,_1)] >> exp[push_back(_a,_1)] >> -lit(';') >> eps [ _val = new_<expression>(AST_node("Stmts"), _a) ];
	stmt =  exp[push_back(_a,_1)] >> lit(";") [ _val = new_<expression>(AST_node("SimpleStmt"), _a) ]
	  | eps [clear(_a) ] >> pat[push_back(_a,_1)] >> "<-" >> exp[push_back(_a,_1)] >> lit(";") [ _val = new_<expression>(AST_node("PatStmt"), _a) ]
	  | eps [clear(_a) ] >> "let" >> decls[push_back(_a,_1)] >> lit(";") [ _val = new_<expression>(AST_node("LetStmt"), _a) ]
	  | eps [clear(_a) ] >> lit(";") [ _val = new_<expression>(AST_node("EmptyStmt"), _a) ];

	/*----- Section 3.15 -----*/
	//	fbind %= qvar >> "=" >> exp;

	/*----- Section 3.17 -----*/
	pat = 
	  lpat [ push_back(_a,_1) ] >> qconop [ push_back(_a,construct<AST_node>("id",construct<String>(_1))) ] >> pat [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("pat"), _a) ]
	  | eps [clear(_a)] >>  lpat [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("pat"), _a) ];

	lpat = 
	  // negative literal float
	  eps [clear(_a)] >> lit('-') >> h_float [ push_back(_a,construct<String>(_1)) ] >> eps [  _val = new_<expression>(AST_node("neg_h_float"), _a) ]
	  // negative literal integer
	  | eps [clear(_a)] >> lit('-') >> h_integer [ push_back(_a,construct<String>(_1)) ] >> eps [  _val = new_<expression>(AST_node("neg_h_integer"), _a) ]
	  // here the number of apat's must match the constructor arity
	  | eps [clear(_a)] >>  gcon[ push_back(_a,construct<String>(_1)) ] >> +apat[ push_back(_a,_1) ] >> eps [_val = new_<expression>(AST_node("constructor_pattern"), _a) ]
	  // apat
	  | apat [ _val = _1 ]
	  ;                  

	apat = 
	  // as pattern
	  //	  var >> lit('@')>>apat 
	  // irrefutable var pattern
	  var [ qi::_val = phoenix::construct<AST_node>("apat_var", qi::_1) ]        
	  // arity gcon = 0
	  | gcon [ push_back(_a,construct<String>(_1)) ] >> eps [_val = new_<expression>(AST_node("constructor_pattern"), _a) ]
	  // labelled pattern
	  //	  | qcon >> "{" >> *fpat >> "}"     
	  | literal [  _val = _1 ]
	  // wildcard
	  | lit('_') [ qi::_val = phoenix::construct<AST_node>("WildcardPattern") ]                       
	  // parenthesized pattern
	  | lit('(') >> pat [ _val = _1 ] >> ')'          
	  // tuple patten
	  | lit('(')[clear(_a)] >> pat[ push_back(_a,_1) ] >> +(lit(',') >> pat[ push_back(_a,_1) ]) >> lit(')') [ _val = new_<expression>(AST_node("Tuple"), _a) ]
	  // list pattern
	  | lit('[')[clear(_a)] >> pat[ push_back(_a,_1) ] % ',' >> lit(']') [ _val = new_<expression>(AST_node("List"), _a) ]
	  // irrefutable pattern
	  //	  | lit('~') >> apat                
	  ;
	//	fpat %= qvar >> "=" >> pat;         // field pattern

	/*------ Section 4 -------*/
	module = 
	  lit("module") > modid[ push_back(_a,construct<String>(_1)) ] > /*-exports >>*/ "where" > body [ push_back(_a,_1) ] >> eps[ _val = new_<expression>(AST_node("Module"), _a) ]
	  | eps[clear(_a)] >>  body[ push_back(_a,_1) ] >> eps[ _val = new_<expression>(AST_node("Module"), _a) ];

	body = 
	  lit('{') >> impdecls[ push_back(_a,_1) ] >> ';' >> topdecls[ push_back(_a,_1) ] >> '}'>> eps[ _val = new_<expression>(AST_node("Body"), _a) ]
	  | lit('{')[clear(_a)] >> impdecls[ push_back(_a,_1) ] >> '}'>> eps[ _val = new_<expression>(AST_node("Body"), _a) ]
	  | lit('{') >> topdecls [ push_back(_a,_1) ] >> '}' >> eps[ _val = new_<expression>(AST_node("Body"), _a) ];

	topdecls = topdecl [ push_back(_a,_1) ] % ';' >> eps[ _val = new_<expression>(AST_node("TopDecls"), _a) ];
	topdecl = 
	  lit("type") >> simpletype[ push_back(_a,_1) ] >> '=' >> type[ push_back(_a,_1) ] >> eps [ _val = new_<expression>(AST_node("Decl:type"), _a) ]
	  | "data" >> /*-(context >> "=>") >> */ simpletype[ push_back(_a,_1) ] >> '=' >> constrs[ push_back(_a,_1) ] /* >> -deriving */ >> eps [ _val = new_<expression>(AST_node("Decl:data"), _a) ]
	  | "data" >> /*-(context >> "=>") >> */ simpletype[ push_back(_a,_1) ] /* >> -deriving */ >> eps [ _val = new_<expression>(AST_node("Decl:data"), _a) ]
	  | "newtype" >> /*-(context >> "=>") >> */ simpletype [ push_back(_a,_1) ] >> '=' >> newconstr[ push_back(_a,_1) ] /*>> -deriving */ >> eps [ _val = new_<expression>(AST_node("Decl:newtype"), _a) ]
	  //	  | "class" >> -(scontext >> "=>") >> tycls >> tyvar >> -("where" >> cdecls)
	  //	  | "instance" >> -(scontext >> "=>") >> qtycls >> inst >> -("where" >> idecls)
	  //	  | "default" >> *type
	  //	  | "foreign" >> fdecl
	  | lit("builtin") >> (var|varop)[ push_back(_a,construct<String>(_1)) ] >> h_integer[ push_back(_a,construct<String>(_1)) ] >> h_string[ push_back(_a,construct<String>(_1)) ] >> -h_string[ push_back(_a,construct<String>(_1)) ] >> eps[ _val = new_<expression>(AST_node("Builtin"), _a) ]
	  | lit("note") >> bugs_line [_val = _1]
	  | decl [_val = _1]
	  ;

	decls = lit('{') > (decl % ';')[_val = new_<expression>(AST_node("Decls"), _1)] > '}';
	decl  = 
	  (funlhs | pat)[push_back(_a,_1)] >> rhs[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Decl"), _a)  ]
	| gendecl [_val = _1];

	// class declarations
	cdecls %= lit('{') >> cdecl % ';' > '}';
	//	cdecl  %= gendecl | (funlhs | var) >> rhs;

	// instance declarations
	idecls %= lit('{') >> idecl % ';' > '}';
	//	idecl  %= (funlhs | var) >> rhs | eps;

	gendecl = // vars >> "::" >>  -(context >> "=>") >> type 
	  fixity[push_back(_a,construct<String>(_1))] >> -h_integer[push_back(_a,construct<String>(_1))] >> ops[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("FixityDecl"), _a)  ]
	  | eps [ _val = new_<expression>(AST_node("EmptyDecl"), _a)  ];

	ops = op[push_back(_a,construct<String>(_1))]%',' >> eps [ _val = new_<expression>(AST_node("Ops"), _a)  ];
	vars %= +var;
	fixity %= string("infixl") | string("infixr") | string("infix");

	/*----- Section 4.1.2 ------*/

	type = btype[push_back(_a,_1)] >> lit("->") >> type[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("FunctionType"), _a) ]
	  | btype [_val = _1];

	btype = +atype[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("TypeApply"), _a) ];

	atype = gtycon [_val = construct<AST_node>("type_id",construct<String>(_1)) ]
	  // tuple variable
	  | tyvar [_val = construct<AST_node>("type_id",construct<String>(_1)) ]
	  // tuple type, k >= 2
	  | eps[clear(_a)] >> lit('(') >> type[push_back(_a,_1)] >> +(lit(',')>>type[push_back(_a,_1)]) >> ')' >> eps [ _val = new_<expression>(AST_node("TupleType"), _a) ]
	  // list type
	  | eps[clear(_a)] >> lit('[') >> type [push_back(_a,_1)] >> ']'  >> eps [ _val = new_<expression>(AST_node("ListType"), _a) ]
	  // parenthesized type
	  | eps[clear(_a)] >> lit('(') >> type [_val = _1 ] >> ')';
	atype2 = atype [_val = _1] | lit('!') >> atype [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("StrictAtype"), _a) ];

	gtycon %= string("()") | string("[]") | lit("(") >> string("->") >> lit(")") | string("(,") >> *char_(',')>>string(")") | qtycon;

	/*----- Section 4.1.3 ------*/
	//	context %= h_class | lit('(') >> *h_class >> lit(')');
	//	h_class %= qtycls >> tyvar 
	//        | qtycls >> lit('(') >> tyvar >> +atype >> lit(')');

	/*----- Section 4.2.1 ------*/
	newconstr = con[push_back(_a,_1)] >> atype [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("newconstr"), _a) ];
	  // | con >> '{' >> var >> "::" >> type > '}';
	simpletype = tycon[_val = construct<AST_node>("type_id",construct<String>(_1)) ] >> *tyvar[_val = construct<AST_node>("type_id",construct<String>(_1)) ];
	constrs = constr[push_back(_a,_1)]%'|' >> eps [ _val = new_<expression>(AST_node("constrs"), _a) ];

	constr = con[push_back(_a,construct<String>(_1))] >> *atype2[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("constr"), _a) ]
       	  | (btype | atype2)[push_back(_a,_1)] >> conop[push_back(_a,_1)] >> (btype | atype2)[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("constr_op"), _a) ];
	  //	  | con >> '{' >> *fielddecl > '}';

	fielddecl = vars >> "::" >> (type | '!' >> atype);
	//	deriving = lit("deriving") >> (dclass | lit("()") | '(' >> dclass%',' >> ')');
	dclass = qtycls;

	/*------ Section 4.3.1 -----*/
	//	scontext %= simpleclass | "()" | '(' >> simpleclass%',' >> ')';
	//	simpleclass %= qtycls >> tyvar;
	
	/*------ Section 4.3.2 -----*/
	/*
	inst %= 
	  gtycon 
	  | '(' >> gtycon >> *tyvar >>')' 
	  | '(' >> tyvar >> ',' >> tyvar %',' >> ')'
	  | '[' >> tyvar >> ']'
	  | tyvar >> "->" >> tyvar
	  ;
	*/

	/*------ Section 4.4.3 -----*/
	funlhs = var [push_back(_a,phoenix::construct<AST_node>("id", construct<String>(_1)))] >> +apat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs1"), _a)  ]
	  | eps[clear(_a)] >> pat [push_back(_a,_1)] >> varop[push_back(_a,phoenix::construct<AST_node>("id", construct<String>(_1)))] >> pat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs2"), _a)  ]
		  | eps[clear(_a)] >> "(" >> funlhs[push_back(_a,_1)] >> ")" > +apat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs3"), _a)  ];

	rhs = lit('=') >> exp [push_back(_a,_1)] >> -(lit("where") >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("rhs"), _a)  ]
	  | gdrhs[push_back(_a,_1)] >> -(lit("where") >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("rhs"), _a)  ];

	gdrhs = guards[push_back(_a,_1)] >> "=" >> exp [push_back(_a,_1)] >> -gdrhs[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("gdrhs"), _a)  ];

	/*------ Section 5.1 -------*/
	impdecls = impdecl[push_back(_a,_1)] % ';' >> eps [ _val = new_<expression>(AST_node("impdecls"), _a)  ];
	
	/*------ Section 5.2 -------*/
	/*
	exports = lit("()") | '('>> h_export % ',' >> -lit(',') >> ')';
	h_export = 
	  qvar
	  | qtycon >> -("(..)" | lit("()") | "(" >> cname %',' >> ")")
	  | qtycls >> -("(..)" | lit("()") | "(" >> var %',' >> ")")
	  | "module" >> modid
	  ;
	*/
	cname = var | con;
	
	/*------ Section 5.3 -------*/
	impdecl = "import" > -string("qualified")[push_back(_a,construct<String>(_1))] 
	                   > -string("submodel")[push_back(_a,construct<String>(_1))] 
	                   > modid[push_back(_a,construct<String>(_1))] 
			   >> -(string("as")[push_back(_a,construct<String>(_1))] > modid[push_back(_a,construct<String>(_1))]) 
			   >> /*-impspec >>*/ eps [ _val = new_<expression>(AST_node("ImpDecl"), _a)  ];

	//	impspec = 
	//	  lit("()")
	//	  | '(' >> import%',' >> ')'
	//	  | lit("hiding") >> '(' >> import%',' >> ')';

	// FIXME! Parsing problems //
	/*
	import = 
	   var
	  | tycon >> -("(..)" | lit("()") | "(" >> cname %"," >> ")")
	  | tycls >> -("(..)" | lit("()") | "(" >> var %"," >> ")");
	*/

	/*----------- Rules for notes and related processing - not Haskell --------------*/
	bugs_dist = lit("data") >> exp[push_back(_a,_1)] >> '~' > exp[push_back(_a,_1)] >>eps [ _val = new_<expression>(AST_node("BugsDataDist"), _a)  ] 
	  | eps [clear(_a) ] >> lit("external") >> exp[push_back(_a,_1)] >> '~' > exp[push_back(_a,_1)] >>eps [ _val = new_<expression>(AST_node("BugsExternalDist"), _a)  ]
	  | eps [clear(_a) ] >> exp[push_back(_a,_1)] >> '~' > exp[push_back(_a,_1)] >>eps [ _val = new_<expression>(AST_node("BugsDist"), _a)  ];
	bugs_default_value = qvar [push_back(_a, phoenix::construct<AST_node>("id", construct<String>(_1))) ] >> ":=" > exp[push_back(_a,_1)] > eps [ _val = new_<expression>(AST_node("BugsDefaultValue"), _a)  ];
	bugs_note = fexp[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("BugsNote"), _a)  ];
	bugs_parameter = lit("parameter") >> varid [push_back(_a,construct<String>(_1))] >> eps [ _val = new_<expression>(AST_node("Parameter"), _a)  ];

	bugs_line %= bugs_parameter | bugs_default_value | bugs_dist | bugs_note;


	on_error<fail>
	  (
	   exp
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   lexp
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   aexp
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   funlhs
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   gendecl
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   fixity
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   rhs
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   impdecls
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   impdecl
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   decls
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   decl
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   topdecls
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   module
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   body
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   topdecl
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   pat
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   lpat
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   apat
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   h_integer
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   h_string
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   literal
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   bugs_line
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   bugs_dist
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

	on_error<fail>
	  (
	   bugs_default_value
	   , std::cout
	   << val("Error! Expecting ")
	   << _4
	   << val(" here: \"")
	   << construct<std::string>(_3, _2)
	   << val("\"")
	   << std::endl
	   );

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
	h_string.name("h_string");
	reservedid.name("reserved_id");

	bugs_parameter.name("bugs_parameter");
	bugs_default_value.name("bugs_default_value");
	bugs_dist.name("bugs_dist");
	bugs_note.name("bugs_note");
    }

  qi::rule<Iterator, char()> small;
  qi::rule<Iterator, char()> large;
  qi::rule<Iterator, char()> digit;
  qi::rule<Iterator, char()> symbol;
  qi::rule<Iterator, char()> special;
  qi::rule<Iterator, char()> graphic;

  qi::rule<Iterator, std::string()> dashes;

  qi::rule<Iterator, std::string()> varid;
  qi::rule<Iterator, std::string()> conid;
  qi::rule<Iterator, std::string()> reservedid_;
  qi::rule<Iterator, std::string()> reservedid;

  qi::rule<Iterator, std::string()> varsym;
  qi::rule<Iterator, std::string()> consym;
  qi::rule<Iterator, std::string()> reservedop_; // reserved operator
  qi::rule<Iterator, std::string()> reservedop; // reserved operator

  qi::rule<Iterator, std::string()> tyvar;
  qi::rule<Iterator, std::string()> tycon;
  qi::rule<Iterator, std::string()> tycls;
  qi::rule<Iterator, std::string()> modid; // module id

  qi::rule<Iterator, std::string()> qvarid; // qualified variable id
  qi::rule<Iterator, std::string()> qconid; // qualified constructor id
  qi::rule<Iterator, std::string()> qtycon; // qualified type constructor
  qi::rule<Iterator, std::string()> qtycls; // qualified type class
  qi::rule<Iterator, std::string()> qvarsym;
  qi::rule<Iterator, std::string()> qconsym;

  qi::rule<Iterator, std::string()> decimal;
  qi::rule<Iterator, std::string()> h_integer;
  qi::rule<Iterator, std::string()> h_float;
  qi::rule<Iterator, std::string()> exponent;

  qi::rule<Iterator, char()> h_char;
  qi::rule<Iterator, char()> escape;
  qi::rule<Iterator, std::string()> h_string;
  qi::rule<Iterator, std::string()> charesc;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>> literal;  

  qi::rule<Iterator, expression_ref(), ascii::space_type> exp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> infixexp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> lexp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> fexp;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> aexp;

  /*----- Section 3.2 -------*/
  qi::rule<Iterator, std::string(), ascii::space_type> gcon;
  qi::rule<Iterator, std::string(), ascii::space_type> var;
  qi::rule<Iterator, std::string(), ascii::space_type> qvar;
  qi::rule<Iterator, std::string(), ascii::space_type> con;
  qi::rule<Iterator, std::string(), ascii::space_type> qcon;
  qi::rule<Iterator, std::string(), ascii::space_type> varop;
  qi::rule<Iterator, std::string(), ascii::space_type> qvarop;
  qi::rule<Iterator, std::string(), ascii::space_type> conop;
  qi::rule<Iterator, std::string(), ascii::space_type> qconop;
  qi::rule<Iterator, std::string(), ascii::space_type> op;
  qi::rule<Iterator, std::string(), ascii::space_type> gconsym;
  qi::rule<Iterator, expression_ref(), ascii::space_type> qop;

  /*----- Section 3.11 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> qual;  

  /*----- Section 3.13 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> alts;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> alt;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> gdpat;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> guards;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> guard;

  /*----- Section 3.14 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> stmts;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> stmt;

  /*----- Section 3.15 -----*/
  qi::rule<Iterator, std::string(), ascii::space_type> fbind;  

  /*----- Section 3.17 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> pat;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> lpat;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> apat;  
  qi::rule<Iterator, std::string(), ascii::space_type> fpat;  

  /*----- Section 4 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> module;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> body;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> topdecls;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> topdecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> decls;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> decl;

  qi::rule<Iterator, std::string(), ascii::space_type> cdecls;
  qi::rule<Iterator, std::string(), ascii::space_type> cdecl;

  qi::rule<Iterator, std::string(), ascii::space_type> idecls;
  qi::rule<Iterator, std::string(), ascii::space_type> idecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> gendecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> ops;
  qi::rule<Iterator, std::string(), ascii::space_type> vars;
  qi::rule<Iterator, std::string(), ascii::space_type> fixity;

  /*----- Section 4.1.2 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> type;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> btype;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> atype;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> atype2;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> gtype;
  //  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> gtycon;
  qi::rule<Iterator, std::string(), ascii::space_type> gtycon;

  /*----- Section 4.1.3 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> context;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> h_class;

  /*----- Section 4.2.1 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> newconstr;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> simpletype;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> constrs;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> constr;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> fielddecl;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> deriving;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> dclass;

  /*----- Section 4.3.1 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> scontext;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> simpleclass;

  /*----- Section 4.3.2 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> inst;

  /*----- Section 4.4.3 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> funlhs;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> rhs;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> gdrhs;

  /*----- Section 5.1 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>,  ascii::space_type> impdecls;

  /*----- Section 5.2 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> exports;
  qi::rule<Iterator, std::string(), ascii::space_type> h_export;
  qi::rule<Iterator, std::string(), ascii::space_type> cname;

  /*----- Section 5.3 ------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> impdecl;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> impspec;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> import;

  /*----------- Rules for notes and related processing - not Haskell --------------*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_dist;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_default_value;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_note;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_parameter;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_line;
};

//-----------------------------------------------------------------------//

expression_ref parse_haskell_line(const string& line)
{
  using boost::spirit::ascii::space;

  string::const_iterator iter = line.begin();
  haskell_grammar<string::const_iterator> haskell_parser;
  expression_ref E;
  if (phrase_parse(iter, line.end(), haskell_parser, space, E) and iter == line.end())
    return E;

  throw myexception()<<"Haskell pharse parse: only parsed "<<line.substr(0, iter-line.begin());
}

expression_ref parse_haskell_decls(const string& line)
{
  using boost::spirit::ascii::space;

  string::const_iterator iter = line.begin();
  haskell_grammar<string::const_iterator> haskell_parser;
  expression_ref E;
  if (phrase_parse(iter, line.end(), haskell_parser.decls, space, E) and iter == line.end())
    return E;
  throw myexception()<<"Haskell pharse parse: only parsed "<<line.substr(0, iter-line.begin());
}

expression_ref parse_bugs_line(const string& line)
{
  using boost::spirit::ascii::space;

  string::const_iterator iter = line.begin();
  haskell_grammar<string::const_iterator> haskell_parser;
  expression_ref cmd;
  if (phrase_parse(iter, line.end(), haskell_parser.bugs_line, space, cmd) and iter == line.end())
    return cmd;

  throw myexception()<<"BUGS pharse parse: only parsed "<<line.substr(0, iter-line.begin());
}

expression_ref parse_bugs_file(const string& lines)
{
  typedef lex::lexertl::token<
    char const*, boost::mpl::vector<std::string>
    > token_type;

  typedef lex::lexertl::actor_lexer<token_type> lexer_type;

  typedef word_count_tokens<lexer_type>::iterator_type iterator_type;

  word_count_tokens<lexer_type> word_count;          // Our lexer
  /*----------------------------------------------------------------------------*/

  using boost::spirit::ascii::space;

  string::const_iterator iter = lines.begin();
  haskell_grammar<string::const_iterator> haskell_parser;
  expression_ref cmd;
  if (phrase_parse(iter, lines.end(), haskell_parser.module, space, cmd) and iter == lines.end())
    return cmd;

  throw myexception()<<"BUGS pharse parse: only parsed "<<lines.substr(0, iter-lines.begin());
}
