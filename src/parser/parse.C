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

	small %= char_("a-z");
	large %= char_("A-Z");
	digit %= char_("0-9");
	symbol %= char_("!#$%&*+./<=>?@\\^|~:") | char_('-');
	special %= char_("(),;[]`{}");
	graphic %= small | large | symbol | digit | special | char_('"') | char_('\'');

	dashes %= lit("--")>>*lit("-");

	varid %= (small>>(*(small|large|digit|'\''))) - reservedid;
	conid %= large>>(*(small|large|digit|'\''));
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

	double (*temp2)(const std::string&);
	temp2 = convertTo<double>;
	int (*temp3)(const std::string&);
	temp3 = convertTo<int>;

	literal = h_float [ _val = phoenix::bind(temp2, _1) ]
	  | h_integer [ _val =  phoenix::bind(temp3, _1) ]
	  | h_char [ _val = _1 ]
	  | h_string [ _val = _1 ];

	/*----- Section 3 ------*/
	exp = 
	  infixexp [ _val = new_<expression>(AST_node("infixexp"),_1) ] >> "::" >> -(context >> "=>") >> type 
	  | infixexp [_val = new_<expression>(AST_node("infixexp"),_1) ];

	infixexp = 
	  lexp [push_back(_val,_1)] >> qop [push_back(_val,_1)] > infixexp [insert(_val,end(_val),begin(_1),end(_1))] 
	  | lit("-") [push_back(_val, AST_node("neg"))] > infixexp [insert(_val,end(_val),begin(_1),end(_1))] 
	  | lexp [ clear(_val), push_back(_val,_1) ]
	  ;

	lexp = 
	  lit("\\") > +apat[push_back(_a,_1)] > lit("->") > exp[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Lambda"), _a)  ]
	  | lit("let")[clear(_a)] > decls[push_back(_a,_1)] > "in" > exp[push_back(_a,_1)]  >> eps [ _val = new_<expression>(AST_node("Let"), _a)  ]
	  | lit("if")[clear(_a)] > exp[push_back(_a,_1)] > -lit(';') >> "then" > exp[push_back(_a,_1)] > -lit(';') > "else" > exp[push_back(_a,_1) ]>> eps [ _val = new_<expression>(AST_node("If"), _a)  ]
	  | lit("case")[clear(_a)] > exp[push_back(_a,_1)] > "of" > "{" >> alts[push_back(_a,_1)] >> "}" >> eps [ _val = new_<expression>(AST_node("Case"), _a)  ]
	  //	  | lit("do") >> "{" >> stmts >> "}"
	  | fexp [_val = _1]
	  ;

	fexp = +aexp [ push_back(_a,_1) ] >> eps [ _val = new_<expression>(AST_node("Apply"), _a) ]  ; // function application

	// order: con >> conid >> qconid >qcon >> gcon

	aexp = 
	  // variable
	  qvar [_val = phoenix::construct<AST_node>("id", _1) ]
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
	  | lit("(")[clear(_a)] >> infixexp[insert(_a,end(_a),begin(_1),end(_1))]  >> qop[push_back(_a,_1)] >> ")" >> eps [ _val = new_<expression>(AST_node("LeftSection"), _a) ]
	  // right section
	  | lit("(")[clear(_a)] >> ((qop[push_back(_a,_1)] - "-") >> infixexp[insert(_a,end(_a),begin(_1),end(_1))]) >> ")" >> eps [ _val = new_<expression>(AST_node("RightSection"), _a) ]
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
	qop = qvarop [ _val = construct<AST_node>("id", _1) ] | qconop [ _val = construct<AST_node>("id",_1) ];  // qualified operator
	gconsym %= string(":") | qconsym;

	/*----- Section 3.11 -----*/
	qual = pat [push_back(_a,_1)] >> "<-" > exp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("PatQual"), _a) ]
	  | lit("let") >> decls[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("LetQual"), _a) ]
	  | eps [clear(_a) ] >> exp [push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("SimpleQual"), _a) ];

	/*----- Section 3.13 -----*/
	alts = (alt % ';' )[_a = _1] >> eps [ _val = new_<expression>(AST_node("alts"), _a) ];
	alt =  eps [clear(_a) ] >> pat[push_back(_a,_1)] >> "->" > exp[push_back(_a,_1)] >> -("where" >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("alt"), _a) ]
	//	  | pat >> gdpat >> -("where" >> decls) 
	  | eps;

	//	gdpat %= guards >> "->" >> exp >> -gdpat;
	//	guards %= "|" >> +guard;
	//	guard %= pat >> "<-" >> infixexp // pattern guard
	//	  | "let" >> decls // local declaration
	//	  | infixexp;      // boolean guard

	/*----- Section 3.14 -----*/
	//	stmts %= *stmt >> exp >> -lit(';');
	//	stmt %= exp >> ";" | pat >> "<-" >> exp >> ";" | "let" >> decls >> ";" | ";";

	/*----- Section 3.15 -----*/
	//	fbind %= qvar >> "=" >> exp;

	/*----- Section 3.17 -----*/
	pat = 
	  lpat [ push_back(_a,_1) ] >> qconop [ push_back(_a,construct<AST_node>("id",_1)) ] >> pat [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("pat"), _a) ]
	  | eps [clear(_a)] >>  lpat [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("pat"), _a) ];

	lpat = 
	  // negative literal float
	  eps [clear(_a)] >> lit('-') >> h_float [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("neg_h_float"), _a) ]
	  // negative literal integer
	  | eps [clear(_a)] >> lit('-') >> h_integer [ push_back(_a,_1) ] >> eps [  _val = new_<expression>(AST_node("neg_h_integer"), _a) ]
	  // here the number of apat's must match the constructor arity
	  | eps [clear(_a)] >>  gcon[ push_back(_a,_1) ] > +apat[ push_back(_a,_1) ] >> eps [_val = new_<expression>(AST_node("constructor_pattern"), _a) ]
	  // apat
	  | apat [ _val = _1 ]
	  ;                  

	apat = 
	  // as pattern
	  //	  var >> lit('@')>>apat 
	  // irrefutable var pattern
	  var [ qi::_val = phoenix::construct<AST_node>("apat_var", qi::_1) ]        
	  // arity gcon = 0
	  | gcon [ push_back(_a,_1) ] >> eps [_val = new_<expression>(AST_node("constructor_pattern"), _a) ]
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
	  lit("module") >> modid >> -exports >> "where" >> body
	  | body;
	body = 
	  lit('{') >> impdecls >> ';' >> topdecls >> '}'
	  | lit('{') >> impdecls >> '}'
	  | lit('{') >> topdecls >> '}';

	topdecls = topdecl % ';';
	topdecl = 
	  lit("type") >> simpletype >> '=' >> type
	  | "data" >> -(context >> "=>") >> simpletype >> -('=' >> constrs) >> -deriving
	  | "newtype" >> -(context >> "=>") >> simpletype >> '=' >> newconstr >> -deriving
	  | "class" >> -(scontext >> "=>") >> tycls >> tyvar >> -("where" >> cdecls)
	  | "instance" >> -(scontext >> "=>") >> qtycls >> inst >> -("where" >> idecls)
	  | "default" >> *type
	  //	  | "foreign" >> fdecl
	  //	  | decl 
	  ;

	decls = lit('{') > (decl % ';')[_val = new_<expression>(AST_node("Decls"), _1)] > '}';
	decl  %= 
	  //	  gendecl |
	  (funlhs | pat)[push_back(_a,_1)] >> rhs[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("Decl"), _a)  ];

	// class declarations
	cdecls %= lit('{') >> cdecl % ';' >> '}';
	//	cdecl  %= gendecl | (funlhs | var) >> rhs;

	// instance declarations
	idecls %= lit('{') >> idecl % ';' >> '}';
	//	idecl  %= (funlhs | var) >> rhs | eps;

	//	gendecl %= vars >> "::" >>  -(context >> "=>") >> type 
	//	  | fixity >> -h_integer >> ops 
	//	  | eps;

	ops %= +op;
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
	newconstr = con >> atype | con >> '{' >> var >> "::" >> type >> '}';
	simpletype = tycon >> *tyvar;
	constrs = +constr;
	constr = 
	  con >> *(-lit('!') >> atype) 
	  | (btype | '!' >> atype) >> conop >> (btype | '!' >> atype)
	  | con >> '{' >> *fielddecl >> '}';

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
	funlhs = var [push_back(_a,phoenix::construct<AST_node>("id", _1))] >> +apat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs1"), _a)  ]
		  | eps[clear(_a)] >> pat [push_back(_a,_1)] >> varop[push_back(_a,_1)] >> pat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs2"), _a)  ]
		  | eps[clear(_a)] >> "(" >> funlhs[push_back(_a,_1)] >> ")" > +apat[push_back(_a,_1)] >> eps [ _val = new_<expression>(AST_node("funlhs3"), _a)  ];

	rhs = lit('=') > exp [push_back(_a,_1)] >> -(lit("where") >> decls[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("rhs"), _a)  ];
	//	  | gdrhs >> -(lit("where") >> decls);

	//	gdrhs %= guards >> "=" >> exp >> -gdrhs;

	/*------ Section 5.1 -------*/
	impdecls = impdecl % ';';
	
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
	impdecl = "import" >> -lit("qualified") >> modid >> -("as" >> modid) >> -impspec;
	impspec = 
	  lit("()")
	  | '(' >> import%',' >> ')'
	  | lit("hiding") >> '(' >> import%',' >> ')';

	// FIXME! Parsing problems //
	/*
	import = 
	   var
	  | tycon >> -("(..)" | lit("()") | "(" >> cname %"," >> ")")
	  | tycls >> -("(..)" | lit("()") | "(" >> var %"," >> ")");
	*/
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

	exp.name("exp");
	infixexp.name("infixexp");
	lexp.name("lexp");
	fexp.name("fexp");
	aexp.name("aexp");
	alts.name("alts");
	alt.name("alt");
	apat.name("funlhs");
	apat.name("apat");
	lpat.name("lpat");
	pat.name("pat");
	decls.name("decls");
	decl.name("decl");
	gcon.name("gcon");
	literal.name("literal");
	h_string.name("h_string");
	reservedid.name("reserved_id");
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

  qi::rule<Iterator, expression_ref()> literal;  

  qi::rule<Iterator, expression_ref(), ascii::space_type> exp;
  qi::rule<Iterator, vector<expression_ref>(), ascii::space_type> infixexp;
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
  qi::rule<Iterator, std::string(), ascii::space_type> gdpat;
  qi::rule<Iterator, std::string(), ascii::space_type> guards;
  qi::rule<Iterator, std::string(), ascii::space_type> guard;

  /*----- Section 3.14 -----*/
  qi::rule<Iterator, std::string(), ascii::space_type> stmts;
  qi::rule<Iterator, std::string(), ascii::space_type> stmt;

  /*----- Section 3.15 -----*/
  qi::rule<Iterator, std::string(), ascii::space_type> fbind;  

  /*----- Section 3.17 -----*/
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> pat;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> lpat;  
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> apat;  
  qi::rule<Iterator, std::string(), ascii::space_type> fpat;  

  /*----- Section 4 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> module;
  qi::rule<Iterator, std::string(), ascii::space_type> body;

  qi::rule<Iterator, std::string(), ascii::space_type> topdecls;
  qi::rule<Iterator, std::string(), ascii::space_type> topdecl;

  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> decls;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> decl;

  qi::rule<Iterator, std::string(), ascii::space_type> cdecls;
  qi::rule<Iterator, std::string(), ascii::space_type> cdecl;

  qi::rule<Iterator, std::string(), ascii::space_type> idecls;
  qi::rule<Iterator, std::string(), ascii::space_type> idecl;

  qi::rule<Iterator, std::string(), ascii::space_type> gendecl;

  qi::rule<Iterator, std::string(), ascii::space_type> ops;
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
  qi::rule<Iterator, std::string(), ascii::space_type> impdecls;

  /*----- Section 5.2 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> exports;
  qi::rule<Iterator, std::string(), ascii::space_type> h_export;
  qi::rule<Iterator, std::string(), ascii::space_type> cname;

  /*----- Section 5.3 ------*/
  qi::rule<Iterator, std::string(), ascii::space_type> impdecl;
  qi::rule<Iterator, std::string(), ascii::space_type> impspec;
  qi::rule<Iterator, std::string(), ascii::space_type> import;
  
};

template <typename Iterator>
struct bugs_grammar : qi::grammar<Iterator, expression_ref(), ascii::space_type>
{
  qi::rule<Iterator, expression_ref(), ascii::space_type> bugs_line;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_lines;
  qi::rule<Iterator, std::string()> text;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_dist;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_default_value;
  qi::rule<Iterator, expression_ref(), qi::locals<vector<expression_ref>>, ascii::space_type> bugs_note;
  haskell_grammar<Iterator> h;

    bugs_grammar() : bugs_grammar::base_type(bugs_line)
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

	text %= +(char_ - ' ' -'(');
	bugs_dist = h.exp[push_back(_a,_1)] >> '~' > text[push_back(_a,_1)] > (lit('(')>>h.exp[push_back(_a,_1)]%','>>lit(')')|lit("()"))>eps [ _val = new_<expression>(AST_node("BugsDist"), _a)  ];
	bugs_default_value = h.exp[push_back(_a,_1)] >> '=' > h.exp[push_back(_a,_1)] > eps [ _val = new_<expression>(AST_node("BugsDefaultValue"), _a)  ];
	bugs_note = h.conid[push_back(_a,_1)] >> *(h.exp[push_back(_a,_1)]) >> eps [ _val = new_<expression>(AST_node("BugsNote"), _a)  ];

	bugs_line %= bugs_dist | bugs_default_value | bugs_note;
	bugs_lines = bugs_line [push_back(_a,_1)] % ';' >> eoi [ _val = new_<expression>(AST_node("BugsLines"), _a)  ];

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
	   text
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

    }
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
  bugs_grammar<string::const_iterator> bugs_parser;
  expression_ref cmd;
  if (phrase_parse(iter, line.end(), bugs_parser, space, cmd) and iter == line.end())
    return cmd;

  throw myexception()<<"BUGS pharse parse: only parsed "<<line.substr(0, iter-line.begin());
}

expression_ref parse_bugs_file(const string& lines)
{
  using boost::spirit::ascii::space;

  string::const_iterator iter = lines.begin();
  bugs_grammar<string::const_iterator> bugs_parser;
  expression_ref cmd;
  if (phrase_parse(iter, lines.end(), bugs_parser.bugs_lines, space, cmd) and iter == lines.end())
    return cmd;

  throw myexception()<<"BUGS pharse parse: only parsed "<<lines.substr(0, iter-lines.begin());
}
