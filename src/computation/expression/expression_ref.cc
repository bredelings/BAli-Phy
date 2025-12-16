// #define DEBUG_OPTIMIZE

#include "haskell/ids.H"
#include "util/string/join.H"
#include "expression_ref.H"
#include "lambda.H"
#include "apply.H"
#include "let.H"
#include "var.H"
#include "case.H"
#include "trim.H"
#include "tuple.H"
#include "list.H" // for char_list
#include "bool.H"
#include "do_block.H"
#include "index_var.H"
#include "computation/operations.H"
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
    case type_constant::index_var_type:
        return std::string("%")+convertToString(i);
        break;
    default:
        return ptr()->print();
    }
}

string print_list(const expression_ref& E)
{
    if (not has_constructor(E,":")) std::abort();

    vector<string> V;
    string S;

    expression_ref E2 = E;
    while(has_constructor(E2,":"))
    {
	assert(E2.size() == 2);
	auto x = E2.sub()[0];
	if (x.is_char())
	    S += x.as_char();
	V.push_back(x.print());
	E2 = E2.sub()[1];
    }
    if (not S.empty() and S.size() == V.size())
	return "\"" + S + "\"";
    else if (has_constructor(E2,"[]"))
	return "["+join(V,", ")+"]";
    else {
	V.push_back(E2.print());
	return join(V,":");
    }

}

bool is_infix_expression(const expression& e)
{
    if (not is_apply(e.head)) return false;

    if (e.size() != 3) return false;

    if (not e.sub[0].is_a<var>()) return false;

    auto id = e.sub[0].as_<var>().name;

    return is_haskell_sym(id);
}

bool is_infix_expression(const expression_ref& e)
{
    if (not is_apply_exp(e)) return false;

    if (e.size() != 3) return false;

    if (not e.sub()[0].is_a<var>()) return false;

    auto id = e.sub()[0].as_<var>().name;

    return is_haskell_sym(id);
}

// How do I make constructor-specific methods of printing data expressions?
// Can I move to defining the print function using an expression?
string expression::print() const 
{
    string result;
    assert(head);

    // The head should not have parts.
    // assert(not is_a<expression>());

    //  if (false)
    {
	if (head.is_a<lambda2>())
	{
	    result = sub[0].print();
	    if (sub[0].head().is_a<lambda2>())
		result = "/\\" + result;
	    else
		result = "/\\." + result;
	    return result;
	}

	if (head.is_a<lambda>())
	{
	    expression_ref body = new expression(*this);
	    vector<string> vars;
	    while (body.head().is_a<lambda>())
	    {
		vars.push_back(body.sub()[0].print());
		// Keep a reference 'body.sub()[1]' here, so it is not destroyed!
		expression_ref tmp = body.sub()[1];
		body = tmp;
	    }
	    result = "\\" + join(vars,' ') + " -> "+ body.print();
	    return result;
	}

	else if (is_let_expression(head))
	{
            auto& L = head.as_<let_exp>();
	    result = "let {";
	    vector<string> parts;
	    for(auto& [x,e] : L.binds)
		parts.push_back(x.print() + " = " + e.print());
	    result += join(parts,"; ");
	    result += "} in " + L.body.print();
	    return result;
	}

	else if (auto L = parse_indexed_let_expression(*this))
	{
	    result = "let {";
	    result += join(L->binds,"; ");
	    result += "} in " + L->body.print();
	    return result;
	}

	else if (head.is_a<Trim>())
	{
	    auto& V = sub[0].as_<Vector<int>>();

	    result = "Trim {"+join(V,",")+"} " + sub[1].print();
	    return result;
	}

	else if (auto C = parse_case_expression(*this))
	{
            auto& [object, alts] = *C;
	    result = "case " + object.print() + " of {";
	    vector<string> parts;
	    for(auto& [pattern, body]: alts)
		parts.push_back( pattern.print() + " -> " + body.print() );
	    result += join(parts,"; ");
	    result += "}";
	    return result;
	}
    }

    // We have to do this BEFORE we compute pargs, otherwise we do everything twice, which leads to exponential growth.
    if (head.is_a<constructor>())
    {
        auto& c = head.as_<constructor>();
        if (c.f_name == ":" and size() == 2)
        {
            return print_list(*this);
        }
    }

    // Print the (unparenthesized) sub-expressions
    vector<string> args(1+size());
    args[0] = head.print();
    for(int i=0;i<size();i++)
	args[1+i] = sub[i].print();

    vector<string> pargs = args;
    for(int i=1;i<pargs.size();i++)
    {
        // Maybe do blocks should have more visible structure?
	if (not sub[i-1].size() and not sub[i-1].is_a<do_block>()) continue;

	if (sub[i-1].head().is_a<constructor>())
	{
	    auto& O = sub[i-1].head().as_<constructor>();

	    // Don't parenthesize tuple arguments.
	    if (is_tuple_name(O.name()) and sub[i-1].size() == O.n_args()) continue;

	    // Don't parenthesize list arguments.
	    if (O.name() == ":") continue;
	}

	pargs[i] = "(" + args[i] + ")";
    }

    if (head.is_a<Apply>())
    {
	// Don't print @ f x y, just print f x y
	pargs.erase(pargs.begin());

	if (is_infix_expression(*this))
	{
	    // Don't parenthesize the operator!
	    pargs[0] = sub[0].as_<var>().name;

	    if (is_apply_exp(sub[1]) and not is_infix_expression(sub[1]))
		pargs[1] = args[2];

	    if (is_apply_exp(sub[2]) and not is_infix_expression(sub[2]))
		pargs[2] = args[3];

	    std::swap(pargs[0],pargs[1]);
	}

	return join(pargs, " ");
    }
    else if (head.is_a<constructor>())
    {
	auto& O = head.as_<constructor>();

	string O_name = O.name();
	if (is_tuple_name(O.name()) and size() == O.n_args())
	{
	    // Should Tuple's parenthesis sub-expressions?
	    vector<string> sub_names;
	    for(int i=0;i<size();i++)
		sub_names.push_back( args[1+i] );
	    return "(" + join(sub_names,", ") + ")";
	}
      
	return join(pargs, " ");
    }

    return join(pargs, " ");
}

bool expression::operator==(const expression& E) const
{
    if (head != E.head) return false;

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

expression_ref::expression_ref(const bool& b)
    :expression_ref(b?bool_true:bool_false)
{}

expression_ref::expression_ref(const std::initializer_list<expression_ref>& es)
{
    for(auto& e: es)
    {
	if (not (*this))
	    (*this) = e;
	else
	    (*this) = apply((*this),e);
    }
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

EVector::operator vector<double>() const
{
    vector<double> v2(size());
    for(int i=0;i<v2.size();i++)
	v2[i] = (*this)[i].as_double();
    return v2;
}

EVector::operator vector<int>() const
{
    vector<int> v2(size());
    for(int i=0;i<v2.size();i++)
	v2[i] = (*this)[i].as_int();
    return v2;
}

EVector::operator vector<char>() const
{
    vector<char> v2(size());
    for(int i=0;i<v2.size();i++)
	v2[i] = (*this)[i].as_char();
    return v2;
}

EVector::EVector(const vector<double>& v1)
{
    resize(v1.size());
    for(int i=0;i<v1.size();i++)
	(*this)[i] = v1[i];
}

EVector::EVector(const vector<int>& v1)
{
    resize(v1.size());
    for(int i=0;i<v1.size();i++)
	(*this)[i] = v1[i];
}

EVector::EVector(const vector<char>& v1)
{
    resize(v1.size());
    for(int i=0;i<v1.size();i++)
	(*this)[i] = v1[i];
}

EPair::EPair(const pair<int,int>& p)
{
    first = p.first;
    second = p.second;
}

EPair::operator std::pair<int,int>() const
{
    return std::pair<int,int>(first.as_int(), second.as_int());
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
