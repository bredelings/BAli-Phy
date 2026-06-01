#include "util/ptree.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include "util/myexception.H"

#include <algorithm>
#include <variant>

using std::string;
using std::vector;
using std::pair;

using std::optional;

namespace
{
    std::strong_ordering compare_values(const ptree::value_t& x, const ptree::value_t& y)
    {
        return x <=> y;
    }
}

bool ptree::value_is_empty() const
{
    return value.index() == 0;
}

bool ptree::is_null() const
{
    return value_is_empty() and children().empty();
}

void ptree::erase(const std::string& key)
{
    vector<pair<string,ptree>> children2;
    for(auto& x: children())
	if (x.first != key)
	    children2.push_back(std::move(x));
    children() = std::move(children2);
}

optional<int> ptree::get_child_index(const std::string& key) const
{
    for(int i=0;i<children().size();i++)
	if (children()[i].first == key)
	    return i;
    return {};
}

int ptree::count(const std::string& key) const
{
    int i=0;
    for(auto& x: children())
	if (x.first == key)
	    i++;
    return i;
}

std::strong_ordering ptree::operator<=>(const ptree& p2) const
{
    if (auto cmp = compare_values(value, p2.value); cmp != 0)
        return cmp;

    const auto& children1 = children();
    const auto& children2 = p2.children();

    auto n = std::min(children1.size(), children2.size());
    for(auto i = decltype(n){0}; i < n; i++)
    {
        if (auto cmp = children1[i].first <=> children2[i].first; cmp != 0)
            return cmp;
        if (auto cmp = children1[i].second.operator<=>(children2[i].second); cmp != 0)
            return cmp;
    }

    if (auto cmp = children1.size() <=> children2.size(); cmp != 0)
        return cmp;

    return std::strong_ordering::equivalent;
}

ptree*
ptree::get_child_optional(const std::string& key)
{
    if (auto index = get_child_index(key))
	return &children()[*index].second;
    else
	return {};
}

const ptree*
ptree::get_child_optional(const std::string& key) const
{
    if (auto index = get_child_index(key))
	return &children()[*index].second;
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
    int index = children().size();
    children().push_back({key,{}});
    return index;
}

ptree& ptree::make_child(const string& key)
{
    int index = make_index(key);
    return children()[index].second;
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
	throw myexception()<<"Trying to convert non-bool to bool: '"<<show(false)<<"'";
    else
	return get_value<bool>();
}

ptree::operator int () const
{
    if (not is_a<int>())
	throw myexception()<<"Trying to convert non-int to int: '"<<show(false)<<"'";
    else
	return get_value<int>();
}

ptree::operator double () const
{
    if (not is_a<double>())
	throw myexception()<<"Trying to convert non-double to double: '"<<show(false)<<"'";
    else
	return get_value<double>();
}

ptree::operator const std::string& () const
{
    if (not is_a<string>())
	throw myexception()<<"Trying to convert non-string to string: '"<<show(false)<<"'";
    else
	return get_value<string>();
}

string ptree::show(bool pretty) const
{
    return ::show(*this, pretty);
}

string show(const ptree& pt, std::optional<int> depth)
{
    string result = convertToString(pt.value);

    if (depth)
    {
        string indent2(*depth+2,' ');
        for(auto& [key,value]: pt.children())
        {
            result += "\n" + indent2 + key + " : ";
            result += show(value, *depth+4);
        }
    }
    else
    {
        vector<string> pairs;
        for(auto& [key,value]: pt.children())
        {
            pairs.push_back(key + ": " + show(value));
        }
        if (not pairs.empty())
        {
            if (pt.value_is_empty())
                result = "{"+join(pairs,", ")+"}";
            else
                result += "["+join(pairs,", ")+"]";
        }
    }
    return result;
}

std::ostream& operator<<(std::ostream& o, const ptree::value_t& v)
{
    if (std::holds_alternative<std::monostate>(v))
        o<<"()";
    else if (std::holds_alternative<bool>(v))
        o<<std::get<bool>(v);
    else if (std::holds_alternative<int>(v))
        o<<std::get<int>(v);
    else if (std::holds_alternative<std::string>(v))
        o<<"'"<<std::get<string>(v)<<"'";
    else if (std::holds_alternative<OrderedDouble>(v))
        o<<std::get<OrderedDouble>(v).value;
    else
        std::abort();
    return o;
}
