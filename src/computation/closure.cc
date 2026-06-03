#include "computation/object.H"
#include "computation/operation.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/trim.H"
#include "util/string/join.H" // for join( )
#include <utility>

using std::vector;
using std::string;

void closure::set_code(Runtime::Exp c)
{
    code = std::move(c);
    exp_cache = Runtime::to_expression_ref(code);
}

void closure::set_legacy_exp(expression_ref e)
{
    code = {};
    exp_cache = std::move(e);
}

void closure::clear()
{
    exp_cache = {};
    code = {};
    Env.clear();
}

string closure::print() const
{
    string result = exp.print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

closure get_trimmed(const closure& C)
{
    closure C2 = C;
    return get_trimmed(std::move(C2));
}

void do_trim(closure& C)
{
    if (C.exp.head().type() == type_constant::trim_type)
    {
	expression_ref old = C.exp;
	const vector<int>& keep = old.sub()[0].as_<Vector<int>>();

	C.set_legacy_exp(C.exp.sub()[1]);

	// Since environments are indexed backwards
	for(int i=0;i<keep.size();i++)
	{
	    int k = keep[keep.size()-1-i];
	    C.Env[i] = C.lookup_in_env(k);
	}
	C.Env.resize(keep.size());

	// Should this ever happen?
	assert(not C.exp.head().is_a<Trim>());
    }
}

closure get_trimmed(closure&& C)
{
    do_trim(C);

    return std::move(C);
}

expression_ref deindexify(const closure& C)
{
    vector<expression_ref> variables;
    for(int R: C.Env)
	variables.push_back(reg_var(R));
  
    return deindexify(C.exp, variables);
}

closure trim_unnormalize(const closure& C)
{
    closure C2 = C;
    C2.set_legacy_exp(trim_unnormalize(C2.exp));
    return C2;
}
