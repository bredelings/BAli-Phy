#include <iostream>
#include <string>
#include <tuple>
#include <boost/spirit/home/x3.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>
#include "../util/ptree.H"

namespace x3 = boost::spirit::x3;

using std::string;
using std::pair;

using x3::int_;
using x3::double_;
using x3::char_;
using x3::lit;
using x3::lexeme;

auto g = double_ >> "," >> double_;
auto ascSmall = char_("a-z");

auto digit = char_("0-9");
auto large = char_("A-Z");
auto special = char_("[(),;[\\]`{}]");
auto small = ascSmall | char_("_");
auto varid = x3::rule<class varid,string>()
    = lexeme[small >> *(small | large | digit | char_("'"))];

auto conid = x3::rule<class conid,string>()
    = lexeme[large >> *(small | large | digit | char_("'"))];
auto modid = x3::rule<class modid,string>()
    = +(conid>>char_("."));

auto op = x3::rule<class op,string>()
    = lit("+");

auto qvarid = x3::rule<class qvarid,string>()
    = modid >> varid;
    
auto string_literal = x3::rule<class string_literal,string>()
    = '"' >> x3::lexeme[ *(~char_('"')|char_('\\')>>char_)] >> '"';

auto char_literal = x3::rule<class char_literal,char>()
    = '\'' >> char_ >> '\'';

auto literal = x3::rule<class literal, ptree::value_t>()
    = double_ | int_ | string_literal | char_literal;

x3::rule<class term_class> term = "term";
    
x3::rule<class infix_class> infix = "infix";
    
x3::rule<class function_> function_ = "function";

auto tuple = "(">>term>>+(",">>term)>>")";

auto list = "[">>term>>+(",">>term)>>"]";

auto arg = varid >> "=" >> term | varid >> "~" >> term | term;

auto variable = (varid | char_("@")>>varid);

auto lambda = char_('\\') >> varid >> "->" >> term;

//auto let = x3::lit("let") >> "{" >> +(varid >>"=">>term) >>"}" >> "in" >> term;
auto let = lexeme["let"] >> "{" >> (varid >> "=" >> term) >> *(";" >> varid >> "=" >> term) >> "}" >> lexeme["in"] >> term;
    
// FIXME: this only allows infix at the top level!
auto const infix_def = term >> op >> infix  | term;
BOOST_SPIRIT_DEFINE(infix);

auto const term_def = let | function_ | literal | "~" >> term | list | tuple | variable | lambda | "(" >> term >>")" ;
BOOST_SPIRIT_DEFINE(term);

auto function__def = (varid|qvarid)
    >> "(" >> -(arg%",") >> ")";
BOOST_SPIRIT_DEFINE(function_);
    
bool parse(const string& s)
{
    using boost::spirit::x3::ascii::space;


//    ptree::value_t x;
    ptree x;

    auto set_name = [](auto& ctx) {_val(ctx).put_value(_attr(ctx));};
    auto push_back = [](auto& ctx) {_val(ctx).push_back(_attr(ctx));};

	
    // term = | submodels       (term + term + term)
    //        | function_call   (QVarId[term,term,term])
    //        | double
    //        | int
    //        | char
    //        | string
    //        | tuple           (term,term,term)
    //        | list            [term,term,term]
    //        | parenthesized   (term)
    //        | let_exp         (let {x = E, y= F,..} in G)
    //        | lambda_exp      ( \varid -> term )
    
    auto iter = s.begin();
    bool ok = x3::phrase_parse(iter, s.end(), infix, space);
//    bool ok = true;
    if (ok and iter == s.end())
    {
//	std::cout<<"read: "<<x.show()<<"\n";
	return true;
    }
    else
    {
//	std::cout<<"failure - read: "<<x.show()<<"\n";
	return false;
    }
}

int main(int argc, char* argv[])
{
    for(int i=1;i<argc;i++)
    {
	if (parse(argv[i]))
	    std::cout<<"success!\n";
	else
	    std::cout<<"fail!\n";
    }
    return 0;
}
