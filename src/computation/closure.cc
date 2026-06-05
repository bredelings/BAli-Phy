#include "computation/object.H"
#include "computation/operation.H"
#include "computation/preprocess.H"
#include "computation/runtime/trim.H"
#include "util/string/join.H" // for join( )
#include <cstdlib>
#include <utility>

using std::vector;
using std::string;

namespace
{
    const Runtime::Exp& slot_ref(const Runtime::Exp& E, int i)
    {
        auto app = E.to<Runtime::App>();
        assert(app);
        assert(0 <= i);
        assert(i < app->args.size());
        return app->args[i];
    }
}

void closure::set_code(Runtime::Exp c)
{
    code = std::move(c);
}

void closure::clear()
{
    code = {};
    Env.clear();
}

string closure::print() const
{
    string result = deindexify(*this).print();
    if (Env.size())
	result += " {" + join(Env,", ") + "}";
    return result;
}

int closure::n_slots() const
{
    assert(has_code());

    if (auto app = code.to<Runtime::App>())
        return app->args.size();
    else if (code.to<Runtime::Case>())
        return 1;
    else
        std::abort();
}

Runtime::Exp closure::slot(int i) const
{
    const auto& E = slot_ref(code, i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return Runtime::RegRef(lookup_in_env(index_var->index));
    else
        return E;
}

int closure::reg_for_slot(int i) const
{
    const auto& E = slot_ref(code, i);

    if (auto index_var = E.to<Runtime::IndexVar>())
        return lookup_in_env(index_var->index);
    else if (auto reg_ref = E.to<Runtime::RegRef>())
        return reg_ref->target;
    else
        std::abort();
}

closure get_trimmed(const closure& C)
{
    closure C2 = C;
    return get_trimmed(std::move(C2));
}

void do_trim(closure& C)
{
    assert(C.has_code());

    vector<int> keep;
    if (const auto* trim = C.get_code().to<Runtime::Trim>())
    {
        keep = trim->indices;
        C.set_code(trim->body);

        assert(not C.get_code().to<Runtime::Trim>());
    }
    else
        return;

    // Since environments are indexed backwards
    for(int i=0;i<keep.size();i++)
    {
        int k = keep[keep.size()-1-i];
        C.Env[i] = C.lookup_in_env(k);
    }
    C.Env.resize(keep.size());


}

closure get_trimmed(closure&& C)
{
    do_trim(C);

    return std::move(C);
}

closure trim_unnormalize(const closure& C)
{
    assert(C.has_code());

    closure C2 = C;
    C2.set_code(Runtime::trim_unnormalize(C2.get_code()));
    return C2;
}
