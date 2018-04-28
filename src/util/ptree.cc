#include "ptree.H"
#include "util.H"
#include "myexception.H"

using std::string;
using std::vector;
using std::pair;

using boost::optional;

std::ostream& operator<<(std::ostream& o,const monostate&) {o<<"()";return o;}

bool ptree::value_is_empty() const
{
    return value.which() == 0;
}

bool ptree::is_null() const
{
    return value_is_empty() and empty();
}

void ptree::erase(const std::string& key)
{
    vector<pair<string,ptree>> children2;
    for(auto& x: (*this))
	if (x.first != key)
	    children2.push_back(std::move(x));
    std::swap(*this, children2);
}

optional<int> ptree::get_child_index(const std::string& key) const
{
    for(int i=0;i<size();i++)
	if ((*this)[i].first == key)
	    return i;
    return boost::none;
}

int ptree::count(const std::string& key) const
{
    int i=0;
    for(auto& x: (*this))
	if (x.first == key)
	    i++;
    return i;
}

bool ptree::operator==(const ptree& p2) const
{
    return (value == p2.value) and ((vector<pair<string,ptree>>&)(*this) == (vector<pair<string,ptree>>&)p2);
}

bool ptree::operator!=(const ptree& p2) const
{
    return not ((*this) == p2);
}

boost::optional<ptree&>
ptree::get_child_optional(const std::string& key)
{
    if (auto index = get_child_index(key))
	return (*this)[*index].second;
    else
	return boost::none;
}

boost::optional<const ptree&>
ptree::get_child_optional(const std::string& key) const
{
    if (auto index = get_child_index(key))
	return (*this)[*index].second;
    else
	return boost::none;
}

ptree& ptree::get_child(const std::string& key)
{
    auto c = get_child_optional(key);
    if (c)
	return *c;
    else
	throw myexception()<<"No child with key '"<<key<<"'";
}

const ptree& ptree::get_child(const std::string& key) const
{
    auto c = get_child_optional(key);
    if (c)
	return *c;
    else
	throw myexception()<<"No child with key '"<<key<<"'";
}

ptree::operator bool () const
{
    if (not is_a<bool>())
	throw myexception()<<"Trying to convert non-bool to bool";
    else
	return get_value<bool>();
}

ptree::operator int () const
{
    if (not is_a<int>())
	throw myexception()<<"Trying to convert non-int to int";
    else
	return get_value<int>();
}

ptree::operator double () const
{
    if (not is_a<double>())
	throw myexception()<<"Trying to convert non-double to double";
    else
	return get_value<double>();
}

ptree::operator const std::string& () const
{
    if (not is_a<string>())
	throw myexception()<<"Trying to convert non-string to string";
    else
	return get_value<string>();
}

string ptree::show() const
{
    return ::show(*this);
}

string show(const ptree& pt, int depth)
{
    string indent(depth,' ');
    string indent2(depth+2,' ');
    string result = convertToString(pt.value);
    if (pt.has_value<string>())
	result = "'" + result + "'";
    for(auto c: pt)
    {
	result += "\n" + indent2 + c.first + " : ";
	result += show(c.second,depth+4);
    }
    return result;
}

void to_json(json& j, const ptree::value_t& v)
{
    if (v.which() == 0)
	;
    else if (v.which() == 1)
	j = boost::get<std::string>(v);
    else if (v.which() == 2)
	j = boost::get<int>(v);
    else if (v.which() == 3)
	j = boost::get<double>(v);
    else if (v.which() == 4)
	j = boost::get<bool>(v);
    else
	std::abort();
}

void to_json(json& j, const ptree& p)
{
    json value = p.value;

    json args = json::array();
    for(auto& arg: p)
    {
	json p = json::array();
	p[0] = arg.first;
	p[1] = arg.second;
	args.push_back(p);
    }
    j = {{"value",value},{"args",args}};
}
