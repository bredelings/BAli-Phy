#include "constructor.H"
#include "computation/runtime/ast.H"
#include "haskell/ids.H"

using std::string;

bool constructor::operator==(const Object& o) const
{
    const constructor* E = dynamic_cast<const constructor*>(&o);
    if (not E) 
	return false;

    // Should we check that the arity matches also?

    return f_name == E->f_name;
}

constructor::constructor(const string& s, int n)
    :f_name(s), n_args_(n), prec(-1)
{
    assert(is_haskell_con_name(s) or (s=="*") or (s=="->"));
}

bool has_constructor(const expression_ref& E, const string& s)
{
    return E.head() == constructor(s,-1);
}

bool has_constructor(const Runtime::Exp& E, const string& s)
{
    if (const auto* C = E.to<Runtime::Constructor>())
        return C->value.name() == s;

    const auto* app = E.to<Runtime::App>();
    if (not app)
        return false;

    const auto* C = std::get_if<Runtime::ConstructorApp>(&app->head);
    return C and C->head.name() == s;
}
