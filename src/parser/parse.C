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

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

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

// A symbol table for parameters and vars.
qi::symbols<char,expression_ref> identifiers;

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
	  lexp [push_back(_a,_1)] >> qop [push_back(_a,_1)] > infixexp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("infixexp"), _a)  ]
	  | eps[clear(_a)] >> lit("-") [push_back(_a, AST_node("neg"))] > infixexp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("infixexp"), _a)  ]
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
	  //	  lit("type") >> simpletype >> '=' >> type
	  //	  | "data" >> -(context >> "=>") >> simpletype >> -('=' >> constrs) >> -deriving
	  //	  | "newtype" >> -(context >> "=>") >> simpletype >> '=' >> newconstr >> -deriving
	  //	  | "class" >> -(scontext >> "=>") >> tycls >> tyvar >> -("where" >> cdecls)
	  //	  | "instance" >> -(scontext >> "=>") >> qtycls >> inst >> -("where" >> idecls)
	  //	  | "default" >> *type
	  //	  | "foreign" >> fdecl
	    lit("builtin") >> var[ push_back(_a,_1) ] >> h_integer[ push_back(_a,_1) ] >> h_string[ push_back(_a,_1) ] >> eps[ _val = new_<expression>(AST_node("Builtin"), _a) ]
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
	type %= btype >> -( lit("->") >> type );
	btype %= -btype >> atype;
	atype %= gtycon
	  | tyvar
	  | lit('(') >> type >> +(lit(',')>>type) >> ')' // tuple type, k >= 2
	  | lit('[') >> type >> ']'                      // list type
	  | lit('(') >> type >> ')';                      // parenthesized constructor
	gtycon %= qtycon 
	  | lit("()")
	  | lit("[]")
	  | lit("(") >> lit("->") >> lit(")")
	  | lit("(,")>>+lit(',') >> lit(')');

	/*----- Section 4.1.3 ------*/
	context %= h_class | lit('(') >> *h_class >> lit(')');
	h_class %= qtycls >> tyvar | qtycls >> lit('(') >> tyvar >> +atype >> lit(')');

	/*----- Section 4.2.1 ------*/
	newconstr = con >> atype | con >> '{' >> var >> "::" >> type > '}';
	simpletype = tycon >> *tyvar;
	constrs = +constr;
	constr = 
	  con >> *(-lit('!') >> atype) 
	  | (btype | '!' >> atype) >> conop >> (btype | '!' >> atype)
	  | con >> '{' >> *fielddecl > '}';

	fielddecl = vars >> "::" >> (type | '!' >> atype);
	//	deriving = lit("deriving") >> (dclass | lit("()") | '(' >> dclass%',' >> ')');
	dclass = qtycls;

	/*------ Section 4.3.1 -----*/
	scontext %= simpleclass | "()" | '(' >> simpleclass%',' >> ')';
	simpleclass %= qtycls >> tyvar;
	
	/*------ Section 4.3.2 -----*/
	inst %= 
	  gtycon 
	  | '(' >> gtycon >> *tyvar >>')' 
	  | '(' >> tyvar >> ',' >> tyvar %',' >> ')'
	  | '[' >> tyvar >> ']'
	  | tyvar >> "->" >> tyvar
	  ;

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
	impdecl = "import" > -string("qualified")[push_back(_a,construct<String>(_1))] > modid[push_back(_a,construct<String>(_1))] 
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
  qi::rule<Iterator, std::string(), ascii::space_type> type;
  qi::rule<Iterator, std::string(), ascii::space_type> btype;
  qi::rule<Iterator, std::string(), ascii::space_type> atype;
  qi::rule<Iterator, std::string(), ascii::space_type> gtype;
  qi::rule<Iterator, std::string(), ascii::space_type> gtycon;

  /*----- Section 4.1.3 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> context;
  qi::rule<Iterator, std::string(), ascii::space_type> h_class;

  /*----- Section 4.2.1 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> newconstr;
  qi::rule<Iterator, std::string(), ascii::space_type> simpletype;
  qi::rule<Iterator, std::string(), ascii::space_type> constrs;
  qi::rule<Iterator, std::string(), ascii::space_type> constr;
  qi::rule<Iterator, std::string(), ascii::space_type> fielddecl;
  qi::rule<Iterator, std::string(), ascii::space_type> deriving;
  qi::rule<Iterator, std::string(), ascii::space_type> dclass;

  /*----- Section 4.3.1 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> scontext;
  qi::rule<Iterator, std::string(), ascii::space_type> simpleclass;

  /*----- Section 4.3.2 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> inst;

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
  using boost::spirit::ascii::space;

  string::const_iterator iter = lines.begin();
  haskell_grammar<string::const_iterator> haskell_parser;
  expression_ref cmd;
  if (phrase_parse(iter, lines.end(), haskell_parser.module, space, cmd) and iter == lines.end())
    return cmd;

  throw myexception()<<"BUGS pharse parse: only parsed "<<lines.substr(0, iter-lines.begin());
}
