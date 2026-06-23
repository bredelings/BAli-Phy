// #define DEBUG_OPTIMIZE

#include "haskell/haskell.H"
#include "util/string/join.H"
#include "expression_ref.H"
#include "computation/module.H"
#include <set>
#include <iterator>
#include <map>
#include <cctype>

using std::pair;
using std::vector;
using std::string;
using std::set;
using std::multiset;
using std::unique_ptr;

using boost::dynamic_pointer_cast;

/// 16.31281376 -> "16.312813760000001"
string double_to_string(double d)
{
    string s = convertToString(d);
    int i = s.size()-1;
    if (s[i] == '0' and s.find('.') != string::npos)
    {
        while(i>=0 and s[i] == '0')
            i--;
        if (s[i] == '.')
            i++;
        s.resize(i+1);
    }
    return s;
}

std::string expression_ref::print() const
{
    switch(type_)
    {
    case type_constant::null_type:
        return "[NULL]";
    case type_constant::int_type:
        return (i<0)?"("+convertToString(i)+")":convertToString(i);
        break;
    case type_constant::double_type:
        return (d<0)?"("+double_to_string(d)+")":double_to_string(d);
        break;
    case type_constant::log_double_type:
        return "LD"+convertToString(ld);
        break;
    case type_constant::char_type:
        return std::string("'")+c+"'";
        break;
    default:
        return ptr()->print();
    }
}

string expression::print() const 
{
    assert(head);

    vector<string> args;
    args.push_back(head.print());
    for(auto& arg: sub)
    {
        auto arg_text = arg.print();
        if (arg.size())
            arg_text = "(" + arg_text + ")";
        args.push_back(arg_text);
    }

    return join(args, " ");
}

bool expression::operator==(const expression& E) const
{
    if (head != E.head) return false;

    if (size() != E.size()) return false;
    
    for(int i=0;i<size();i++) 
	if (sub[i] != E.sub[i]) return false;

    return true;
}

bool expression::operator==(const Object& o) const 
{
    const expression* E = dynamic_cast<const expression*>(&o);
    if (not E) 
	return false;

    return operator==(*E);
}

expression::expression(const expression_ref& H)
    :head(H)
{ 
    assert(H.is_atomic());
}

expression::expression(const expression_ref& H, const std::initializer_list< expression_ref > S)
    :expression(H,std::vector<expression_ref>(S))
{
    assert(H.is_atomic());
}

expression::expression(const expression_ref& H, const std::vector< expression_ref >& S)
    :head(H),sub(S)
{ 
    assert(H.is_atomic());
}

unique_ptr<expression> operator+(const expression_ref& E1, const expression_ref&E2)
{
    expression* E3 = new expression(E1.head());
    if (not E1.is_atomic())
	E3->sub = E1.sub();
    E3->sub.push_back(E2);
    return unique_ptr<expression>(E3);
}

unique_ptr<expression> operator+(const expression& E1, const expression_ref& E2)
{
    auto E3 = E1.clone();
    E3->sub.push_back(E2);
    return unique_ptr<expression>(E3);
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
    assert(H.is_atomic());
}

EPtree::EPtree(const expression_ref& H, const std::initializer_list< std::pair<std::string, expression_ref> > S)
    :EPtree(H, std::vector< std::pair<std::string, expression_ref> >(S))
{
    assert(H.is_atomic());
}


EPtree::EPtree(const expression_ref& H, const std::vector< std::pair<std::string, expression_ref> > S)
    :Vector< std::pair< std::string, expression_ref> >(S), head(H)
{
    assert(H.is_atomic());
}
