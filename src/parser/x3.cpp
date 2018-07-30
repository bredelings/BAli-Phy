#include <iostream>
#include <string>
#include <tuple>
#include <boost/spirit/home/x3.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>

namespace x3 = boost::spirit::x3;
using std::string;
using std::pair;

bool parse(const string& s)
{
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

    auto conid = x3::rule<class varid,string>()
	= large >> *(small | large | digit | char_("'"));
    auto modid = x3::rule<class varid,string>()
	= +(conid>>char_("."));

    auto QVarID = x3::rule<class varid,string>()
	= modid >> varid;
    string x;
    
    auto iter = s.begin();
    bool ok = x3::parse(iter, s.end(), QVarID, x);

    if (ok and iter == s.end())
    {
	std::cout<<"read: "<<x<<"\n";
	return true;
    }
    else
    {
	std::cout<<"failure - read: "<<x<<"\n";
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
