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

bool parse(const string& s)
{
    using x3::int_;
    using x3::double_;
    using x3::char_;
    using boost::spirit::x3::ascii::space;

    auto g = double_ >> "," >> double_;

    auto ascSmall = char_("a-z");
    auto digit = char_("0-9");
    auto large = char_("A-Z");
    auto special = char_("[(),;[\\]`{}]");
    auto small = ascSmall | char_("_");
    auto varid = x3::rule<class varid,string>()
	= small >> *(small | large | digit | char_("'"));

    auto conid = x3::rule<class conid,string>()
	= large >> *(small | large | digit | char_("'"));
    auto modid = x3::rule<class modid,string>()
	= +(conid>>char_("."));

    auto qvarid = x3::rule<class qvarid,string>()
	= modid >> varid;
    
    auto string_literal = x3::rule<class string_literal,string>()
	= '"' >> x3::lexeme[ *(~char_('"')|char_('\\')>>char_)] >> '"';

    auto char_literal = x3::rule<class char_literal,char>()
	= '\'' >> char_ >> '\'';

    auto literal = double_ | int_ | string_literal | char_literal;

    x3::rule<class function_,ptree> function_ = "function";
    
    x3::rule<class term, ptree> term = "term";
    term = literal | function_;
    
    ptree::value_t x;

    auto arg = x3::rule<class arg,pair<string,ptree>>()
	= varid >> "=" >> term;

    auto set_name = [](auto& ctx) {_val(ctx).put_value(_attr(ctx));};
    auto push_back = [](auto& ctx) {_val(ctx).push_back(_attr(ctx));};

//    function_ = (varid|qvarid) [ set_name ] 
//	>> "[" >> arg[push_back]%"," >> "]";
    
	
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
    bool ok = x3::parse(iter, s.end(), literal, x);

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
