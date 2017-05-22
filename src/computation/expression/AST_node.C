#include "AST_node.H"

using std::string;

bool AST_node::operator==(const Object& o) const 
{
    const AST_node* T = dynamic_cast<const AST_node*>(&o);
    if (not T)
	return false;
    if (T->type != type)
	return false;
    if (T->value != value)
	return false;
    return true;
}

string AST_node::print() const 
{
    string result = "AST[";
    result += type;
    if (value.size())
	result += "," + value;
    result += "]";
    return result;
}

AST_node::AST_node(const string& t)
    :type(t)
{ }

AST_node::AST_node(const string& t, const string& v)
    :type(t), value(v)
{ }

bool is_AST(const expression_ref& E, const string& type)
{
    if (not E) return false;
    if (not E.head().is_a<AST_node>()) return false;
    return E.head().as_<AST_node>().type == type;
}

bool is_AST(const expression_ref& E, const string& type, const string& value)
{
    if (not E) return false;
    if (not E.head().is_a<AST_node>()) return false;
    return (E.head().as_<AST_node>().type == type) and (E.head().as_<AST_node>().value == value);
}

