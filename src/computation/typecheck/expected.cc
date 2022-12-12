#include "expected.H"

using std::vector;
using std::string;
using std::optional;


Infer::Infer(int l)
    : level_(l),
      type_ref( std::make_shared<Type>() )
{ }


int Infer::level() const
{
    return level_;
}

string Check::print() const
{
    return "Check(" + type.print() + ")";
}

string Infer::print() const
{
    return "Infer(" + type_ref->print() + ")";
}

string Expected::print() const
{
    if (check())
        return std::get<Check>(value).print();
    else
        return std::get<Infer>(value).print();
}

optional<Type> Expected::read_type_maybe() const
{
    if (auto C = check())
        return C->type;
    else
        return infer()->type();
}


Type Expected::read_type() const
{
    auto t = read_type_maybe();
    if (not t)
        throw myexception()<<"read_type: empty inferred type!";
    else
        return *t;
}


vector<Type> read_types(const vector<Expected>& exp_types)
{
    vector<Type> types;
    for(auto& exp_type: exp_types)
        types.push_back( exp_type.read_type() );
    return types;
}

vector<Expected> check_types(const vector<Type>& types)
{
    vector<Expected> exp_types;
    for(auto& type: types)
        exp_types.push_back( Check(type) );
    return exp_types;
}

