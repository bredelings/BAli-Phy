#include "let.H"
#include "util/range.H" // for reverse( )
#include "util/string/join.H" // for join( )

using std::vector;
using std::string;

string print_cdecls(const CDecls& cdecls)
{
    vector<string> ds;
    for(auto& [x,E]: cdecls)
        ds.push_back(x.print() + " = " + E.print());

    return "{" + join(ds, "; ") +  "}";
}


bool let_exp::operator==(const Object& o) const 
{
    if (this == &o) return true;

    if (typeid(*this) != typeid(o)) return false;

    auto& lo = static_cast<const let_exp&>(o);
    if (binds.size() != lo.binds.size()) return false;

    for(int i=0; i < binds.size();i++)
        if (binds[i] != lo.binds[i]) return false;

    return body == lo.body;
}

string let_exp::print() const 
{
    return "let " + print_cdecls(binds) + " in " + body.print();
}

expression_ref let_expression(const CDecls& decls, const expression_ref& T)
{
    if (decls.size() == 0) return T;

    return let_exp(decls, T);
}

expression_ref let_expression(const vector<CDecls>& decl_groups, const expression_ref& T)
{
    expression_ref body = T;
    for(auto& decls: reverse(decl_groups))
	body = let_expression(decls,body);
    return body;
}

bool is_let_expression(const expression_ref& E)
{
    return (E.head().type() == type_constant::let_type);
}
