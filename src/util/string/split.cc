#include "util/assert.hh"
#include <regex>
#include "util/string/split.H"

using std::vector;
using std::string;
using std::pair;

/// \brief Divide a string \a s into several strings by break it at the character \a c
///
/// \param s The string to split.
/// \param c The separator.
///
vector<string> split(const string& s, char c) 
{
    vector<string> strings;
    int length=0;
    for(int i=0;i<s.size();i++)
	if (s[i] == c) {
	    strings.push_back(s.substr(i-length,length));
	    length = 0;
	}
	else
	    length++;
  
    strings.push_back(s.substr(s.size()-length,length));
    return strings;
}

/// \brief Divide a string \a s into several strings by break it at the string \a token.
///
/// \param s The string to split.
/// \param token The separator.
///
vector<string> split(const string& s, const string& token)
{
    vector<string> strings;

    int p1 = 0;
    do {
	int p2 = s.find(token,p1);
	if (p2 == -1)
	{
	    strings.push_back(s.substr(p1));
	    break;
	}
	else 
	{
	    assert(p2 < s.size());

	    strings.push_back(s.substr(p1,p2-p1));
	    p1 = p2 + token.size();

	    assert(p1 <= s.size());
	}
    } while (true);
    return strings;
}

vector<string>
resplit(const string& s,const string& rgx_str)
{
    vector<string> elems;

    std::regex rgx (rgx_str);

    std::sregex_token_iterator iter(s.begin(), s.end(), rgx, -1);
    std::sregex_token_iterator end;

    while (iter != end)  {
	elems.push_back(*iter);
	++iter;
    }

    return elems;
}

pair<string,string> split_on_last(char sep, const string& s)
{
    string s1 = s;
    string s2;
    auto pos = s1.rfind(sep);
    if (pos != string::npos)
    {
	s1 = s.substr(0,pos);
	s2 = s.substr(pos+1);
    }
    return {s1,s2};
}

pair<string,string> split_on_first(char sep, const string& s)
{
    string s1 = s;
    string s2;
    auto pos = s1.find(sep);
    if (pos != string::npos)
    {
	s1 = s.substr(0,pos);
	s2 = s.substr(pos+1);
    }
    return {s1,s2};
}
