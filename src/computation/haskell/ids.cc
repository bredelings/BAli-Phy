#include "ids.H"

#include <regex>
#include "util/myexception.H"
#include "util/string/join.H"

#include "expression/tuple.H"

using std::string;
using std::vector;

bool is_tuple_name(const string& s)
{
    if (s.size() < 3) return false;
    return s == tuple_name(s.size()-1);
}

int tuple_arity(const string& s)
{
    if (s == "()") return 0;
    assert(is_tuple_name(s));
    int n = s.size()-1;
    assert(s == tuple_name(n));
    return n;
}

string tuple_name(int n)
{
    if (n == 0)
	return "()";

    if (n == 1)
	std::abort();

    string s;
    s.resize(n+1);
    s[0] = '(';
    for(int i=1;i<n;i++)
	s[i] = ',';
    s[n] = ')';
    return s;
}

const std::regex rgx( R"(^([A-Z][a-zA-Z0-9_']*)\.)" );

vector<string> haskell_name_path(const std::string& s)
{
    vector<string> path;
    string rest = s;
    std::smatch m;
    while(std::regex_search(rest, m, rgx))
    {
        path.push_back(m[1]);
        rest = m.suffix().str();
    }
    // FIXME if the rest looks like (for example) BAli-Phy.name, but not if it looks like (for example) .|.
    // FIXME move splitting paths into components into the lexer.  That is, split there and store them already split.
    path.push_back(rest);
    return path;
}

bool is_valid_identifier(const string& s)
{
    if (is_haskell_varid(s)) return true;

    if (is_haskell_conid(s)) return true;

    if (is_haskell_varsym(s)) return true;

    if (is_haskell_consym(s)) return true;

    if (is_haskell_builtin_con_name(s)) return true;

    return false;
}

vector<string> get_haskell_identifier_path(const std::string& s)
{
    if (not s.size())
        throw myexception()<<"Empty string is not a legal Haskell identifier!";

    vector<string> path = haskell_name_path(s);

    for(int i=0;i<path.size()-1;i++)
        if (not is_haskell_conid(path[i]))
            throw myexception()<<"Module id component '"<<path[i]<<"' in identifier '"<<s<<"' is not legal!";

    return path;
}

bool haskell_is_lower(char c)
{
    return (islower(c) or c=='_');
}

bool is_haskell_id(const std::string& s)
{
    if (s.empty()) return false;

    if (not (haskell_is_lower(s[0]) or isupper(s[0]))) return false;

    for(int i=1;i<s.size();i++)
    {
        char c = s[i];
        if (not (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\''))
            return false;
    }
    return true;
}

bool is_haskell_varid(const std::string& s)
{
    if (not is_haskell_id(s)) return false;

    return haskell_is_lower(s[0]);
}

bool is_haskell_conid(const std::string& s)
{
    if (not is_haskell_id(s)) return false;

    return isupper(s[0]);
}

bool is_haskell_uqsym(const string& s)
{
    static const string symbols = "!#$%&*+./<=>?@\\^|-~:";

    if (not s.size()) return false;

    for(int i=0;i<s.size();i++)
        if (symbols.find(s[i]) == -1)
            return false;

    return true;
}

bool is_haskell_varsym(const string& s)
{
    if (not is_haskell_uqsym(s)) return false;

    return (s[0] != ':');
}

bool is_haskell_consym(const string& s)
{
    if (not is_haskell_uqsym(s)) return false;

    if (s[0] != ':') return false;
    if (s == ":") return false;

    return true;
}

bool is_haskell_var_name(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    if (path.empty()) return false;
    if (not is_haskell_varid(path.back()) and not is_haskell_varsym(path.back())) return false;
    for(int i=0;i<path.size()-1;i++)
        if (not is_haskell_conid(path[i])) return false;
    return true;
}

bool is_haskell_builtin_con_name(const std::string& s)
{
    if (s == "()" or s == "[]" or s == ":" or is_tuple_name(s))
        return true;
    else
        return false;
}

bool is_haskell_builtin_type_name(const std::string& s)
{
    if (s == "()" or s == "[]" or s == "->" or s == "~" or is_tuple_name(s))
        return true;
    else if (s == "Double" or s == "Int" or s == "Char")
        return true;
    else
        return false;
}

bool valid_path_prefix(const vector<string>& path)
{
    if (path.empty()) return false;
    for(int i=0;i<path.size()-1;i++)
        if (not is_haskell_conid(path[i])) return false;
    return true;
}

bool is_haskell_qsym(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    return valid_path_prefix(path) and is_haskell_uqsym(path.back());
}

bool is_haskell_sym(const std::string& s)
{
    return is_haskell_uqsym(s) or is_haskell_qsym(s);
}

bool is_haskell_qid(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    return valid_path_prefix(path) and is_haskell_id(path.back());
}

bool is_haskell_normal_con_name(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    if (not valid_path_prefix(path)) return false;
    if (not is_haskell_conid(path.back()) and not is_haskell_consym(path.back())) return false;
    return true;
}

bool is_haskell_con_name(const std::string& s)
{
    return (is_haskell_builtin_con_name(s) or is_haskell_normal_con_name(s));
}

bool is_haskell_module_name(const std::string& s)
{
    return is_haskell_normal_con_name(s);
}

bool is_qualified_symbol(const string& s)
{
    return (get_haskell_identifier_path(s).size() >= 2);
}

string get_module_name(const std::string& s)
{
    vector<string> path = get_haskell_identifier_path(s);
    path.pop_back();

    if (not path.size())
        return "";
    else
        return join(path,'.');
}

string get_unqualified_name(const std::string& s)
{
    return get_haskell_identifier_path(s).back();
}


