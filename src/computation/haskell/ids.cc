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

inline bool ok(char c)
{
    if (c >= 'a' and c <= 'z') return true;
    if (c >= 'A' and c <= 'Z') return true;
    if (c >= '0' and c <= '9') return true;
    if (c == '_' or c == '\'') return true;
    return false;
}

std::optional<int> skip_conid_dot(const std::string& s, int i)
{
    if (i < s.size())
    {
        if (s[i] < 'A' or s[i] > 'Z')
            return {};
        else
            i++;

        while(i < s.size() and ok(s[i]))
            i++;

        if (i<s.size() and s[i]== '.')
            return i;
        else
            return {};
    }
    else
        return {};
}

std::optional<int> find_module_separator(const std::string& s)
{
    int i=0;
    while(auto i2 = skip_conid_dot(s,i))
    {
        i=*i2+1;
    }
    if (i > 0)
        return i-1;
    else
        return {};
}

vector<string> haskell_name_path(const std::string& s)
{
    vector<string> path;
    int i=0;
    while(auto i2 = skip_conid_dot(s,i))
    {
        path.push_back(s.substr(i,*i2-i));
        i=*i2+1;
    }
    // FIXME if the rest looks like (for example) BAli-Phy.name, but not if it looks like (for example) .|.
    // FIXME move splitting paths into components into the lexer.  That is, split there and store them already split.
    path.push_back(s.substr(i));
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

    for(char c: s)
    {
        if (not is_haskell_id_char(c))
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

bool is_haskell_id_char(char c)
{
    return (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\'');
}

const string symbol_chars = "!#$%&*+./<=>?@\\^|-~:";

bool is_haskell_symbol_char(char c)
{
    return symbol_chars.find(c) != -1;
}

bool is_haskell_uqsym(const string& s)
{
    if (not s.size()) return false;

    for(int i=0;i<s.size();i++)
        if (not is_haskell_symbol_char(s[i]))
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
    else if (s == "Double" or s == "Int" or s == "Char" or s == "Integer")
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
    auto uqname = get_unqualified_name(s);

    return is_haskell_conid(uqname) or is_haskell_consym(uqname);
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
    return find_module_separator(s).has_value();
//    return (not s.empty() and get_haskell_identifier_path(s).size() >= 2);
}

bool is_qualified_by_module(const std::string& s, const std::string& modid)
{
    if (not is_qualified_symbol(s)) return false;

    return get_module_name(s) == modid;
}

bool is_local_symbol(const std::string& s, const std::string& modid)
{
    if (is_haskell_builtin_con_name(s)) return false;

    if (is_haskell_builtin_type_name(s)) return false;

    if (not is_qualified_symbol(s)) return true;

    return is_qualified_by_module(s, modid);
}

string get_module_name(const std::string& s)
{
    if (auto m = find_module_separator(s))
        return s.substr(0, *m);
    else
        return {};
}

string get_unqualified_name(const std::string& s)
{
    if (auto m = find_module_separator(s))
        return s.substr(*m + 1);
    else
        return s;
}


