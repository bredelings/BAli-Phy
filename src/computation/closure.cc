#include "computation/object.H"
#include "computation/closure.H"
#include "computation/operation.H"
#include "computation/preprocess.H"
#include "util/string/join.H" // for join( )
#include <cstdlib>
#include <utility>

using std::vector;
using std::string;

void closure::set_code(Runtime::Exp c)
{
    code = std::move(c);
}

void closure::clear()
{
    code.clear();
    Env.clear();
}

string closure::print() const
{
    string result = deindexify(*this).print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

Runtime::Exp closure::constructor_slot(int i) const
{
    return slot_for_code(constructor_slot_ref(i));
}

Runtime::Exp closure::operation_slot(int i) const
{
    return slot_for_code(operation_slot_ref(i));
}

int closure::reg_for_function_arg(int i) const
{
    if (auto r = reg_for_code(function_arg_ref(i)))
        return *r;
    else
        std::abort();
}

int closure::reg_for_constructor_slot(int i) const
{
    if (auto r = reg_for_code(constructor_slot_ref(i)))
        return *r;
    else
        std::abort();
}

int closure::reg_for_operation_slot(int i) const
{
    if (auto r = reg_for_code(operation_slot_ref(i)))
        return *r;
    else
        std::abort();
}

// Closes a dense expression over the lexical registers selected by its projection.
closure get_trimmed(const Runtime::TrimmedExp& code, const closure::Env_t& Env)
{
    closure::Env_t trimmed_env;
    trimmed_env.resize(code.indices.size());

    // Runtime environments use reverse lexical indexing.
    for(int i = 0; i < code.indices.size(); i++)
    {
        int k = code.indices[code.indices.size() - 1 - i];
        trimmed_env[i] = lookup_in_env(Env, k);
    }

    return closure(code.body, trimmed_env);
}
