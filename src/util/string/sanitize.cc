#include "util/string/sanitize.H"
#include "util/string/convert.H"

using std::string;

string sanitize_string(const string& s1)
{
    string s2;
    for(int i=0;i<s1.size();i++) {
	char c = s1[i];
	if (c >=32 and c<= 126)
	    s2 += c;
	else
	    s2 = s2 + "[" + convertToString((int)c) + "]";
    }
    return s2;
}

