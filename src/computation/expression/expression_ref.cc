// #define DEBUG_OPTIMIZE

#include "haskell/haskell.H"
#include "util/string/join.H"
#include "expression_ref.H"
#include "computation/module.H"

using std::pair;
using std::vector;
using std::string;

std::string expression_ref::print() const
{
    if (type_ == type_constant::null_type)
        return "[NULL]";
    else
        return ptr()->print();
}

int EPtree::count(const std::string& key) const
{
    int c = 0;
    for(auto& [k,_]: *this)
        if (key == k)
            c++;
    return c;
}

void EPtree::erase(const string& key)
{
    vector<pair<string,expression_ref>> children2;
    for(auto& x: *this)
        if (x.first != key)
            children2.push_back(std::move(x));
    std::swap(*this, children2);
}

std::optional<int> EPtree::get_child_index(const string& key) const
{
    for(int i=0; i<size(); i++)
        if ((*this)[i].first == key)
            return i;
    return {};
}

expression_ref* EPtree::get_child_optional(const string& key)
{
    if (auto index = get_child_index(key))
        return &(*this)[*index].second;
    else
        return nullptr;
}

const expression_ref* EPtree::get_child_optional(const string& key) const
{
    if (auto index = get_child_index(key))
        return &(*this)[*index].second;
    else
        return nullptr;
}

expression_ref& EPtree::get_child(const string& key)
{
    if (auto c = get_child_optional(key))
        return *c;
    else
	throw myexception()<<"No child with key '"<<key<<"'";
}

const expression_ref& EPtree::get_child(const string& key) const
{
    if (auto c = get_child_optional(key))
        return *c;
    else
	throw myexception()<<"No child with key '"<<key<<"'";
}

string EPtree::print() const
{
    string s;
    s = head.print();
    vector<string> args;
    for(auto& [key,val]: (*this))
    {
        string arg = key + ":" + val.print();
        args.push_back(arg);
    }
    return " { " + join(args, ", ") + "}";
}


bool EPtree::operator==(const EPtree& E) const
{
    if (head != E.head) return false;

    for(int i=0;i<size();i++)
        if ((*this)[i] != E[i]) return false;

    return true;
}

bool EPtree::operator==(const Object& o) const
{
    auto E = dynamic_cast<const EPtree*>(&o);
    if (not E)
        return false;

    return operator==(*E);
}

EPtree::EPtree(const expression_ref& H)
    :head(H)
{
}

EPtree::EPtree(const expression_ref& H, const std::initializer_list< std::pair<std::string, expression_ref> > S)
    :EPtree(H, std::vector< std::pair<std::string, expression_ref> >(S))
{
}


EPtree::EPtree(const expression_ref& H, const std::vector< std::pair<std::string, expression_ref> > S)
    :Vector< std::pair< std::string, expression_ref> >(S), head(H)
{
}
