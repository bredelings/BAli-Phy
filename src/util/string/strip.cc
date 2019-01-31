#include "strip.H"

using std::string;

/// \brief Remove the character \a c from the string \a s.
///
/// \param s The string to strip.
/// \param c The character to remove.
/// \return the stripped string.
///
string strip(const string& s,char c) {
    string s2;
    for(int i=0;i<s.size();i++)
	if (s[i] != c)
	    s2 += s[i];

    return s2;
}


/// \brief Remove all characters in string \a chars from the string \a s.
///
/// \param s The string to strip.
/// \param chars The string containing characters to remove.
/// \return the stripped string.
///
string strip(const string& s,const string& chars) {
    string s2;
    for(int i=0;i<s.size();i++) {
	bool found = false;
	for(int j=0;j<chars.size() and not found;j++) {
	    if (s[i] == chars[j])
		found = true;
	}
	if (not found)
	    s2 += s[i];
    }

    return s2;
}


string rstrip(const string& line,const string& chars)
{
    int last = line.find_last_not_of(chars);
    if (last == string::npos)
	return {};
    return line.substr(0,last+1);
}

string lstrip(const string& line,const string& chars)
{
    int first = line.find_first_not_of(chars);
    if (first == string::npos)
	return {};
    return line.substr(first);
}
