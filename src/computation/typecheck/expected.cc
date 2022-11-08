#include "expected.H"

using std::vector;
using std::string;
using std::optional;


Infer::Infer(int l)
    : level(l),
      type_ref( std::make_shared<Hs::Type>() )
{ }


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

optional<Hs::Type> Expected::read_type_maybe() const
{
    if (check())
        return check_type();
    else
        return inferred_type();
}


Hs::Type Expected::read_type() const
{
    auto t = read_type_maybe();
    if (not t)
        throw myexception()<<"read_type: empty inferred type!";
    else
        return *t;
}


vector<Hs::Type> read_types(const vector<Expected>& exp_types)
{
    vector<Hs::Type> types;
    for(auto& exp_type: exp_types)
        types.push_back( exp_type.read_type() );
    return types;
}

vector<Expected> check_types(const vector<Hs::Type>& types)
{
    vector<Expected> exp_types;
    for(auto& type: types)
        exp_types.push_back( Check(type) );
    return exp_types;
}

