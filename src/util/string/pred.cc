#include "util/string/pred.H"

using std::string;

bool starts_with(const std::string& s, const std::string& prefix)
{
    if (s.size() < prefix.size()) return false;
    return s.substr(0,prefix.size()) == prefix;
}

bool ends_with(const std::string& s, const std::string& suffix)
{
    if (s.size() < suffix.size()) return false;
    return s.substr(s.size()-suffix.size(), suffix.size()) == suffix;
}

/// \brief Check if a string \a s contains a character \a c.
///
/// \param s The string.
/// \param c The character.
/// \return true if \a s contains \a c.
/// 
bool contains_char(const string& s,char c) {
    for(int i=0;i<s.size();i++)
	if (s[i] == c)
	    return true;
    return false;
}

