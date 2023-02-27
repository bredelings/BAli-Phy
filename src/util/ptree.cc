#include "util/ptree.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include "util/myexception.H"

using std::string;
using std::vector;
using std::pair;

using std::optional;

std::ostream& operator<<(std::ostream& o,const monostate&) {o<<"()";return o;}

bool ptree::value_is_empty() const
{
    return value.index() == 0;
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
    return {};
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

ptree*
ptree::get_child_optional(const std::string& key)
{
    if (auto index = get_child_index(key))
	return &(*this)[*index].second;
    else
	return {};
}

const ptree*
ptree::get_child_optional(const std::string& key) const
{
    if (auto index = get_child_index(key))
	return &(*this)[*index].second;
    else
	return {};
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

ptree* ptree::get_path_optional(const vector<string>& path, int i)
{
    assert(i <= path.size());

    // If the path is empty this is a reference to the current object
    if (i == path.size()) return this;

    if (auto child = get_child_optional(path[i]))
	return child->get_path_optional(path, i+1);
    else
	return {};
}


const ptree* ptree::get_path_optional(const vector<string>& path, int i) const
{
    assert(i <= path.size());

    // If the path is empty this is a reference to the current object
    if (i == path.size()) return this;

    if (auto child = get_child_optional(path[i]))
	return child->get_path_optional(path, i+1);
    else
	return {};
}

ptree& ptree::get_path(const vector<string>& path, int i)
{
    auto c = get_path_optional(path, i);
    if (c)
	return *c;
    else
	throw myexception()<<"No path '"<<join(path,'/')<<"' in tree";
}

const ptree& ptree::get_path(const vector<string>& path, int i) const
{
    auto c = get_path_optional(path, i);
    if (c)
	return *c;
    else
	throw myexception()<<"No path '"<<join(path,'/')<<"' in tree";
}

int ptree::make_index(const string& key)
{
    if (auto index = get_child_index(key))
	return *index;
    int index = size();
    push_back({key,{}});
    return index;
}

ptree& ptree::make_child(const string& key)
{
    int index = make_index(key);
    return (*this)[index].second;
}

ptree& ptree::make_path(const vector<string>& path, int i)
{
    assert(i <= path.size());

    if (i == path.size()) return *this;

    auto& child = make_child(path[i]);
    return child.make_path(path, i+1);
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
    if (v.index() == 0)
	;
    else if (v.index() == 1)
	j = std::get<std::string>(v);
    else if (v.index() == 2)
	j = std::get<int>(v);
    else if (v.index() == 3)
	j = std::get<double>(v);
    else if (v.index() == 4)
	j = std::get<bool>(v);
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

std::ostream& operator<<(std::ostream& o, const ptree::value_t& v)
{
    if (std::holds_alternative<monostate>(v))
        o<<"()";
    else if (std::holds_alternative<bool>(v))
        o<<std::get<bool>(v);
    else if (std::holds_alternative<int>(v))
        o<<std::get<int>(v);
    else if (std::holds_alternative<std::string>(v))
        o<<"'"<<std::get<string>(v)<<"'";
    else if (std::holds_alternative<double>(v))
        o<<std::get<double>(v);
    else
        std::abort();
    return o;
}

