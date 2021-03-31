#include "haskell.H"
#include "util/string/join.H"

using std::string;
using std::vector;

namespace Haskell
{

string List::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "[" + join(parts,",") +"]";
}

string Tuple::print() const
{
    vector<string> parts;
    for(auto& element: elements)
        parts.push_back(element.print());
    return "(" + join(parts,", ") +")";
}

string LetQual::print() const
{
    return "let " + binds.print();
}

string SimpleQual::print() const
{
    return exp.print();
}

string PatQual::print() const
{
    return bindpat.print() + " <- " + exp.print();
}

string ID::print() const
{
    return name;
}

string WildcardPattern::print() const
{
    return "_";
}

string LazyPattern::print() const
{
    // FIXME -- do we need parentheses around the pattern, and if so, when?
    return "~"+pattern.print();
}

string AsPattern::print() const
{
    // FIXME -- do we need parentheses around the pattern, and if so, when?
    return var.print()+"@"+pattern.print();
}

string TupleType::print() const
{
    vector<string> parts;
    for(auto& element_type: element_types)
        parts.push_back(element_type.print());
    return "(" + join(parts,", ") +")";
}

string ListType::print() const
{
    return "[" + element_type.print() + "]";
}

bool TypeVar::operator==(const TypeVar& v) const
{
    return name == v.name;
}

bool TypeVar::operator==(const Object& o) const
{
    if (this == &o) return true;

    auto tv = dynamic_cast<const TypeVar*>(&o);

    if (not tv) return false;

    return (*this == *tv);
}

string TypeVar::print() const
{
    return name;
}

string parenthesize_type(const expression_ref& t)
{
    if (t.is_a<TypeVar>() or t.is_a<TupleType>() or t.is_a<ListType>())
        return t.print();
    else
        return "(" + t.print() + ")";
}

string TypeApp::print() const
{
    if (head.is_a<TypeApp>())
    {
        auto& H = head.as_<TypeApp>();
        if (H.head.is_a<TypeVar>())
        {
            auto& A = H.head.as_<TypeVar>();
            if (A.name == "->")
                return H.arg.print() + " -> " + arg.print();
        }
    }

    return head.print() + " " + parenthesize_type(arg);
}

string ForallType::print() const
{
    vector<string> binders;
    for(auto& type_var_binder: type_var_binders)
        binders.push_back(type_var_binder.print());
    return "forall "+join(binders," ")+"."+type.print();
}

string ConstrainedType::print() const
{
    return context.print() + " => " + type.print();
}

std::string Context::print() const
{
    vector<string> cs;
    for(auto& constraint: constraints)
        cs.push_back(constraint.print());

    string result = join(cs,", ");
    if (cs.size() == 1)
        return result;
    else
        return "(" + result + ")";
}

std::pair<Type,std::vector<Type>> decompose_type_apps(Type t)
{
    std::vector<Type> args;
    while(t.is_a<TypeApp>())
    {
        auto A = t.as_<TypeApp>();
        args.push_back(A.arg);
        t = A.head;
    }
    std::reverse(args.begin(), args.end());
    return {t,args};
}
    

}
