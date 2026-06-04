#include "computation/param.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/var.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/list.H"
#include "computation/context.H"
#include "computation/machine/graph_register.H"
#include "computation/operations.H"

#include "util/log-level.H"
#include "computation/machine/gcobject.H"

#include <type_traits>

using std::optional;
using std::vector;

namespace
{
Runtime::Exp runtime_head_code(const Runtime::Exp& E)
{
    auto app = E.to<Runtime::App>();
    if (not app)
        return E;

    return std::visit([](const auto& head) -> Runtime::Exp
    {
        using T = std::decay_t<decltype(head)>;

        if constexpr (std::is_same_v<T, Runtime::FunctionApply>)
            return Apply();
        else if constexpr (std::is_same_v<T, Runtime::ConstructorApp>)
            return head.head;
        else if constexpr (std::is_same_v<T, Runtime::OperationApp>)
            return *head.head;
    }, app->head);
}

int runtime_size(const Runtime::Exp& E)
{
    if (auto app = E.to<Runtime::App>())
        return app->args.size();
    else
        return 0;
}
}

expression_ref param::ref(const context_ref& C) const
{
    if (reg)
        return reg_var(*reg);
    else if (head)
        return C.get_expression(*head);
    else
        return Runtime::to_expression_ref(*value);
}

optional<int> param::is_modifiable(const context_ref& C) const
{
    if (reg)
    {
        if (C.find_modifiable_reg(*reg))
            return *reg;
        else
            return {};
    }
    // FIXME - this doesn't search for the modifiable.
    else if (head)
        return C.compute_expression_is_modifiable_reg(*head);
    else
        return {};
}

Runtime::Exp param::get_code(const context_ref& C) const
{
    if (reg)
    {
        int r = *reg;
	return C.lazy_evaluate_reg(r).get_code();
    }
    else if (head)
    {
	return C.lazy_evaluate_head(*head).get_code();
    }
    else
        return *value;
}

void param::set_code(context_ref& C, Runtime::Exp code) const
{
    if (value)
    {
        if (code != *value)
            throw myexception()<<"param::set_code: trying to set constant '"<<*value<<"' to '"<<code<<"'";
    }
    else if (auto r = is_modifiable(C))
        C.set_modifiable_value(*r,std::move(code));
    else
        throw myexception()<<"param::set_code: can't set non-modifiable head to '"<<code<<"'";
}

context_ptr context_ptr::operator[](int i) const
{
    auto [_, r] = C.incremental_evaluate(reg);
    assert(r>0);
    auto& c = C.memory()->closure_at(r);
    if (runtime_size(c.get_code()) == 0)
    {
        if (auto im = c.get_code().to<IntMap>())
            r = (*im)[i];
        else
            std::abort();
    }
    else
        r = c.reg_for_slot(i);
    return {C, r};
}

context_ptr context_ptr::list_element(int index) const
{
    context_ptr L = result();
    int i=0;
    for(;L.size() > 0 and i < index;i++)
    {
        assert(L.size() == 2);
        L = L[1];
    }
    if (i < index)
        throw myexception()<<"Trying to get list element "<<index<<" for a list of size "<<i<<"!";

    assert(L.size() == 2);

    // If we return the result here, then we can't find a modifiable!
    return L[0];
}

expression_ref context_ptr::value() const
{
    return result().head();
}

Runtime::Exp context_ptr::value_code() const
{
    return result().head_code();
}

EVector context_ptr::list_to_vector() const
{
    object_ptr<EVector> vec(new EVector);

    context_ptr L = result();
    while(L.size() > 0)
    {
        assert(L.size() == 2);

        vec->push_back(L[0].value());

        L = L[1];
    }

    return std::move(*vec);
}

Runtime::RVector context_ptr::list_to_vector_code() const
{
    Runtime::RVector vec;

    context_ptr L = result();
    while(L.size() > 0)
    {
        assert(L.size() == 2);

        vec.push_back(L[0].value_code());

        L = L[1];
    }

    return vec;
}

vector<context_ptr> context_ptr::list_elements() const
{
    vector<context_ptr> elements;

    context_ptr L = result();
    while(L.size() > 0)
    {
        assert(L.size() == 2);

        elements.push_back(L[0]);

        L = L[1];
    }

    return elements;
}

context_ptr context_ptr::result() const
{
    auto cp2 = *this;
    cp2.move_to_result();
    return cp2;
}

optional<context_ptr> context_ptr::modifiable() const
{
    auto cp2 = *this;
    if (cp2.move_to_modifiable())
        return cp2;
    else
        return {};
}

void context_ptr::set_value(const expression_ref& v)
{
    auto m = modifiable();
    if (not m)
        throw myexception()<<"Trying to set the value of non-modifiable reg "<<get_reg();
    int r = m->get_reg();
    const_cast<context_ref&>(C).set_reg_value(r,Runtime::e_op_value(v));
}

void context_ptr::set_code(Runtime::Exp v)
{
    auto m = modifiable();
    if (not m)
        throw myexception()<<"Trying to set the value of non-modifiable reg "<<get_reg();
    int r = m->get_reg();
    const_cast<context_ref&>(C).set_reg_value(r,std::move(v));
}

void context_ptr::move_to_result()
{
    auto [_,result] = C.incremental_evaluate(reg);
    reg = result;
}

bool context_ptr::move_to_modifiable()
{
    auto mod_reg = C.find_modifiable_reg(reg);
    if (mod_reg)
    {
        reg = *mod_reg;
        return true;
    }
    else
        return false;
}

int context_ptr::size() const
{
    auto [_, r] = C.incremental_evaluate(reg);
    assert(r>0);
    return runtime_size(C.memory()->closure_at(r).get_code());
}

expression_ref context_ptr::head() const
{
    return Runtime::to_expression_ref(head_code());
}

Runtime::Exp context_ptr::head_code() const
{
    auto [_, r] = C.incremental_evaluate(reg);
    assert(r>0);
    return runtime_head_code(C.memory()->closure_at(r).get_code());
}

context_ptr::context_ptr(const context_ref& c, int r1)
    :C(c)
{
    // If <i> is a modifiable that calls an unevaluated expression E
    // (e.g. E = flip alignment <reg>), then we don't need to evaluate E to point at <i>.

    // Refusing to evaluate such modifiables allows us to find them and set them without evaluating them.

    if (C.memory()->reg_is_changeable(r1))
	reg = r1;
    else
    {
	auto [r2,_] = C.incremental_evaluate(r1);
	reg = r2;
    }

    // Apparently this does not look through index-var-with-force, but incremental_evaluate(i) does:
    // int r2 = C.incremental_evaluate_unchangeable(r1);
}
