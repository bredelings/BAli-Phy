//#define DEBUG_OPTIMIZE

#include <memory>
#include "computation/context.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/effects.H"
#include "computation/program.H"
#include "loader.H"
#include "module.H"
#include "parser/desugar.H"
#include "expression/lambda.H"
#include "expression/AST_node.H"
#include "expression/var.H"
#include "expression/modifiable.H"
#include "expression/reg_var.H"
#include "util/rng.H"
#include "util/log-level.H"
#include "util/permute.H"

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/member.hpp>

using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;
using std::unordered_set;
using std::unique_ptr;
using boost::dynamic_pointer_cast;
using std::optional;

using std::cerr;
using std::endl;

template <typename T>
using Bounds = Box<bounds<T>>;

object_ptr<reg_heap>& context_ref::memory() const {return memory_;}

const std::vector<int>& context_ref::heads() const {return memory()->get_heads();}

const closure& context_ref::operator[](int i) const {return (*memory())[i];}

closure context_ref::preprocess(const closure& C) const
{
    return memory()->preprocess(C);
}

const closure* context_ref::precomputed_value_for_reg(int r) const
{
    auto& M = *memory();

    if (auto r2 = M.precomputed_value_in_context(r, context_index))
    {
        return &M[*r2];
    }
    else
        return nullptr;
}

const closure* context_ref::precomputed_value_for_head(int index) const
{
    int H = heads()[index];
    return precomputed_value_for_reg(H);
}

/// Return the value of a particular index, computing it if necessary
const closure& context_ref::lazy_evaluate_head(int index) const
{
    return memory()->lazy_evaluate_head(index, context_index);
}

/// Return the value of a particular index, computing it if necessary
const closure& context_ref::lazy_evaluate_reg(int& r) const
{
    return memory()->lazy_evaluate(r, context_index);
}

/// Return the value of a particular index, computing it if necessary
std::pair<int,int> context_ref::incremental_evaluate(int r) const
{
    return memory()->incremental_evaluate_in_context(r, context_index);
}

/// Return the value of a particular index, computing it if necessary
const expression_ref& context_ref::evaluate_head(int index) const
{
    return lazy_evaluate_head(index).exp;
}

/// Return the value of a particular index, computing it if necessary
const expression_ref& context_ref::evaluate_head_unchangeable(int index) const
{
    int R = heads()[index];

    return memory()->lazy_evaluate_unchangeable(R).exp;
}

/// Return the value of a particular index, computing it if necessary
const expression_ref& context_ref::perform_head(int index, bool ec) const
{
    int R = heads()[index];

    return perform_expression(reg_var(R), ec);
}

const closure& context_ref::lazy_evaluate_expression_(closure&& C, bool ec) const
{
    try {
	int R = memory()->push_temp_head( std::move(C) );

	const closure& result = ec?memory()->lazy_evaluate(R, context_index) : memory()->lazy_evaluate_unchangeable(R);
    
	memory()->pop_temp_head();
	return result;
    }
    catch (myexception& e)
    {
	memory()->pop_temp_head();
	throw;
    }
}

const expression_ref& context_ref::evaluate_expression_(closure&& C,bool ec) const
{
    const expression_ref& result = lazy_evaluate_expression_(std::move(C),ec).exp;
#ifndef NDEBUG
    if (result.head().is_a<lambda2>())
	throw myexception()<<"Evaluating lambda as object: "<<result.print();
#endif
    return result;
}

const closure& context_ref::lazy_evaluate_expression(const expression_ref& E, bool ec) const
{
    return lazy_evaluate_expression_( preprocess(E), ec);
}

const expression_ref& context_ref::evaluate_expression(const expression_ref& E,bool ec) const
{
    return evaluate_expression_( preprocess(E), ec);
}

const expression_ref& context_ref::perform_expression(const expression_ref& E,bool ec) const
{
    expression_ref perform_io = get_expression(*(memory()->perform_io_head));
    expression_ref E2 = {perform_io, E};
    return evaluate_expression_( preprocess(E2), ec);
}

const expression_ref& context_ref::evaluate_reg(int r) const
{
    auto [r1, result] = memory()->incremental_evaluate_in_context(r, context_index);

    return memory()->expression_at(result);
}

expression_ref context_ref::recursive_evaluate_reg(int r) const
{
    closure C1 = memory()->lazy_evaluate(r, context_index);
    expression_ref E1 = deindexify(trim_unnormalize(C1));

    if (E1.is_atomic())
	return E1;

    unique_ptr<expression> E (new expression(E1.as_expression()));

    // Having finished with C, it is now safe to do evaluation.
    for(auto& e: E->sub)
    {
	if (e.is_index_var())
	{
	    int r = e.as_index_var();
	    e = recursive_evaluate_reg(r);
	}
	else if (e.is_a<reg_var>())
	{
	    int r = e.as_<reg_var>().target;
	    e = recursive_evaluate_reg(r);
	}
    }
    return expression_ref(std::move(E));
}

expression_ref context_ref::recursive_evaluate_head(int i) const
{
    return recursive_evaluate_reg(get_compute_expression_reg(i));
}

int get_reps(double x)
{
    assert(x >= 0.0);

    if (x <= 0) return 0;

    int x_int = (int)x;
    double x_frac = x - x_int;
    return x_int + poisson(x_frac);
}

double first_occurrence_time(double start, double end, int n)
{
    assert(n > 0);

    double u = uniform();
    double x = -expm1(log(1.0-u)/double(n));
    return start + (end-start)*x;
}

struct tk_group
{
    int step;
    double t0;
    int n;
};

using namespace boost::multi_index;

typedef multi_index_container<
            tk_group,
            indexed_by<
                ordered_non_unique<member<tk_group,double,&tk_group::t0>>,
                hashed_unique<member<tk_group,int,&tk_group::step>>
            >
        > set_tk_group;

void add_transition_kernel(const closure& e, int s, double t, set_tk_group& tk_groups)
{
    double rate = e.exp.sub()[0].as_double();

    // how many total
    int n_total = get_reps(rate);
    // how many in the remaining interval
    int n_remaining = binomial(n_total, 1.0-t);

    if (n_remaining > 0)
    {
        auto t0 = first_occurrence_time(t, 1.0, n_remaining);

        tk_groups.insert({s, t0, n_remaining});
    }
}

tk_group get_next_transition_kernel(set_tk_group& tk_groups)
{
    // 1. Remove the first transition kernels
    auto tk1 = *tk_groups.begin();
    tk_groups.erase(tk_groups.begin());

    // 2. Decrement the remaining instances
    auto tk2 = tk1;
    tk2.n--;

    // 3. If there are no more occurrences, skip reinsertion.
    if (tk2.n > 0)
    {
        // 4. Get a new first occurrence time.
        tk2.t0 = first_occurrence_time(tk1.t0, 1.0, tk2.n);

        // 5. Reinsert
        tk_groups.insert(tk2);
    }

    return tk1;
}

void context_ref::run_transition_kernels()
{
    // 1.unmap any transition kernels that are not currently used.
    evaluate_program();

    // 2. Compute initial set of TK groups.
    set_tk_group tk_groups;

    for(int s: memory()->transition_kernels())
    {
        auto& e = memory()->get_effect(s);
        add_transition_kernel(e, s, 0, tk_groups);
    }

    std::set<int> tk_steps_removed;
    std::set<int> tk_steps_added;

    std::function<void(int, int)> register_tk_handler = [&](int, int s)
    {
        auto it = tk_steps_removed.find(s);
        if (it != tk_steps_removed.end())
            tk_steps_removed.erase(it);
        else
        {
            assert(not tk_steps_added.count(s));
            tk_steps_added.insert(s);
        }
    };

    std::function<void(int, int)> unregister_tk_handler = [&](int, int s)
    {
        auto it = tk_steps_added.find(s);
        if (it != tk_steps_added.end())
            tk_steps_added.erase(it);
        else
        {
            assert(not tk_steps_removed.count(s));
            tk_steps_removed.insert(s);
        }
    };

    memory()->register_tk_handlers.push_back(register_tk_handler);
    memory()->unregister_tk_handlers.push_back(unregister_tk_handler);

    auto& tk_groups_by_step = tk_groups.get<1>();

    while(not tk_groups.empty())
    {
        auto [s, t, _] = get_next_transition_kernel(tk_groups);

        // Run the T.K.
        perform_transition_kernel(s);

        // Handle differences here.
        evaluate_program();

        // The transition kernel may not unregister itself.
        assert(not tk_steps_removed.count(s));

        // process tk_steps_removed
        for(auto tk_step: tk_steps_removed)
            tk_groups_by_step.erase(tk_step);

        // process tk_steps_added
        for(int tk_step: tk_steps_added)
        {
            auto& e = memory()->get_effect(tk_step);
            add_transition_kernel(e, tk_step, t, tk_groups);
        }

        tk_steps_added.clear();
        tk_steps_removed.clear();
    }

    memory()->register_tk_handlers.pop_back();
    memory()->unregister_tk_handlers.pop_back();
}

void context_ref::perform_transition_kernel(int s)
{
    auto& e = memory()->get_effect(s);

    int r = e.reg_for_slot(1);
    assert(memory()->reg_is_constant(r));
    expression_ref E = {reg_var(r), get_context_index()};
    perform_expression(E);
}

int context_ref::n_transition_kernels() const
{
    return memory()->transition_kernels().size();
}

optional<int> context_ref::compute_expression_is_modifiable_reg(int index) const
{
    return memory()->compute_expression_is_modifiable_reg(index);
}

bool context_ref::reg_is_modifiable(int r) const
{
    return is_modifiable(memory()->expression_at(r));
}

EVector context_ref::get_modifiable_values(const std::vector<int>& indices) const
{
    EVector values(indices.size());

    for(int i=0;i<values.size();i++)
	values[i] = get_modifiable_value(indices[i]);

    return values;
}

expression_ref context_ref::maybe_modifiable_structure(int r)
{
    get_reg_value(r);
    return memory()->maybe_modifiable_structure(r);
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
const expression_ref& context_ref::get_reg_value(int R) const
{
    return memory()->get_reg_value_in_context(R, context_index);
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
const expression_ref& context_ref::get_modifiable_value(int R) const
{
    return get_reg_value(*find_modifiable_reg(R));
}

void context_ref::set_modifiable_value_(int R, closure&& C)
{
    set_reg_value(*find_modifiable_reg(R), std::move(C) );
}

void context_ref::set_modifiable_value(int R, const expression_ref& E)
{
    assert(is_literal_type(E.type()));
    set_modifiable_value_(R, E);
}

void context_ref::set_reg_value(int P, closure&& C)
{
    memory()->set_reg_value_in_context(P, std::move(C), context_index);
}

/// Add an expression that may be replaced by its reduced form
int context_ref::add_compute_expression(const expression_ref& E)
{
    return add_compute_expression_( preprocess(E) );
}

/// Add an expression that may be replaced by its reduced form
int context_ref::add_compute_expression_(closure&& C)
{
    memory()->allocate_head(std::move(C));

    return heads().size() - 1;
}

int context_ref::n_expressions() const
{
    return heads().size();
}

expression_ref context_ref::get_expression(int i) const
{
    int H = heads()[i];
    return reg_var(H);
}

std::optional<int> context_ref::out_edges_from_dist(int r) const
{
    auto& from_dist = memory()->out_edges_from_dist;
    auto it = from_dist.find(r);
    if (it == from_dist.end())
        return {};
    else
        return it->second;
}

const std::set<int>* context_ref::out_edges_to_var(int r) const
{
    auto& to_var = memory()->out_edges_to_var;
    auto it = to_var.find(r);
    if (it == to_var.end())
        return nullptr;
    else
        return &(it->second);
}

const std::map<std::string,int>* context_ref::in_edges_to_dist(int r)  const
{
    auto& to_dist = memory()->in_edges_to_dist;
    auto it = to_dist.find(r);
    if (it == to_dist.end())
        return nullptr;
    else
        return &(it->second);
}

const std::map<int,std::set<std::string>>* context_ref::in_edges_from_var(int r)  const
{
    auto& from_var = memory()->in_edges_from_var;
    auto it = from_var.find(r);
    if (it == from_var.end())
        return nullptr;
    else
        return &(it->second);
}

const std::map<std::string,int>* context_ref::dist_properties(int s) const
{
    auto& dist_properties = memory()->dist_properties;
    auto it = dist_properties.find(s);
    if (it == dist_properties.end())
        return nullptr;
    else
        return &(it->second);
}

void context_ref::compile()
{
}

log_double_t context_ref::prior() const
{
    return memory()->prior_for_context(context_index);
}

log_double_t context_ref::likelihood() const
{
    return memory()->likelihood_for_context(context_index);
}

log_double_t context_ref::probability() const
{
    return memory()->probability_for_context(context_index);
}

prob_ratios_t context_ref::probability_ratios(const context_ref& C1) const
{
    return memory()->probability_ratios(C1.context_index, context_index);
}

prob_ratios_t context_ref::heated_probability_ratios(const context_ref& C1) const
{
    auto ratio = probability_ratios(C1);
    ratio.heat(get_beta());
    return ratio;
}

double context_ref::get_beta() const
{
    return 1.0;
}

void context_ref::set_beta(double)
{
    //FIXME: We need to * make a context into a pair (token,heat), or
    //                  * make a beta reg in the machine that won't be forgotten when unused.
    throw myexception()<<"Setting heat/beta for a context is not implemented.  See  context.cc";
}
log_double_t context_ref::heated_likelihood() const
{
    // Don't waste time calculating likelihood if we're sampling from the prior.
    if (get_beta() == 0)
	return 1;
    else
	return pow(likelihood(),get_beta());
}

log_double_t context_ref::heated_probability() const
{
    return prior() * heated_likelihood();
}

log_double_t context_ref::heated_probability_ratio(const context& C1) const
{
    return heated_probability_ratios(C1).total_ratio();
}

void context_ref::collect_garbage() const
{
    memory()->collect_garbage();
}

void context_ref::write_factor_graph() const
{
    evaluate_program();
    collect_garbage();
    ::write_factor_graph(*memory());
}

void context_ref::show_graph() const
{
    evaluate_program();
    collect_garbage();
    write_dot_graph(*memory());
}

void context_ref::show_graph_for_root_token() const
{
    write_dot_graph(*memory());
}

expression_ref context_ref::evaluate_program() const
{
    return memory()->evaluate_program(context_index);
}


json context_ref::get_logged_parameters() const
{
    // 1. Check if there is a logging head.
    if (not memory()->logging_head)
	throw myexception()<<"No logging head has been set!";

    // 2. Evaluate the program before doing non-program evaluation.
    //    Otherwise any invalid regs are going to get unset instead of reshared.
    evaluate_program();

    // 3. Evaluate the logging head
    auto L = evaluate_head(*memory()->logging_head);

    // 4. Check that the result is a JSON object.
    return L.as_checked<Box<json>>().value();
}

int context_ref::get_compute_expression_reg(int index) const
{
    assert(index >= 0 and index < heads().size());

    return heads()[index];
}

optional<int> context_ref::find_modifiable_reg(int r) const
{
    return memory()->find_modifiable_reg_in_context(r, context_index);
}

std::ostream& operator<<(std::ostream& o, const context_ref& C)
{
    for(int index = 0;index < C.n_expressions(); index++)
    {
	o<<index<<" "<<C.get_expression(index);
	o<<"\n";
    }
    return o;
}

void context_ref::clear_program()
{
    memory()->program.reset();
}

void context_ref::clear_identifiers()
{
    memory()->identifiers.clear();
}

const vector<string>& context_ref::get_args() const
{
    return memory()->args;
}

void context_ref::set_args(const vector<string>& a)
{
    memory()->args = a;
}

// This probably should not really be an equals operator, since it
// can't be implemented by copy-construction, which just makes another
// reference to the same thing.

// Make the context point to the same token as the other context
context_ref& context_ref::operator=(const context_ref& C)
{
    assert(&get_memory() == &C.get_memory());

    memory_->switch_to_context(context_index, C.get_context_index());

    return *this;
}

/*
Delete this, since it would be confusing to have

   context_ref C1 = C2;

and

   context_ref C1(M,c1);
   C1 = C2;

mean different things.

context_ref::context_ref(const context_ref& C)
    :memory_(C.memory_),
     context_index(C.get_context_index())
{
}
*/

context_ref::context_ref(reg_heap& M)
    :context_ref(M,-1)
{
}

context_ref::context_ref(reg_heap& M, int c)
    :memory_(&M),
     context_index(c)
{
}

/*----------------------*/

context::context(const context_ref& C)
    :context_ref(*C.memory_,
                 C.memory_->copy_context(C.get_context_index()))
{
}

context::context(const context& C)
    :context_ref(*C.memory_,
                 C.memory_->copy_context(C.get_context_index()))
{
}

context& context::operator=(context&& C)
{
    std::swap(context_index, C.context_index);
    return *this;
}

context::context(context&& C)
    :context_ref(*C.memory_, -1)
{
    std::swap(context_index, C.context_index);
}

context::context(const Program& P)
    :context_ref(*new reg_heap(P))
{
    context_index = memory_->get_first_context();
}

context::~context()
{
    if (context_index != -1)
        memory_->release_context(context_index);
}

bool accept_MH(const context_ref& C1,const context_ref& C2,log_double_t rho)
{
    if (log_verbose >= 4)
    {
        std::cerr<<"accept_MH: rho = "<<rho<<endl;

        show_parameters(std::cerr,C1);
        std::cerr<<C1.probability()<<" = "<<C1.likelihood()<<" [likelihood] + "<<C1.prior()<<" [prior]"<<endl;
        std::cerr<<endl;

        show_parameters(std::cerr,C2);
        std::cerr<<C2.probability()<<" = "<<C2.likelihood()<<" [likelihood] + "<<C2.prior()<<" [prior]"<<endl;
        std::cerr<<endl<<endl;
    }

    log_double_t ratio = rho*C2.heated_probability_ratio(C1);

    bool accept = (ratio >= 1.0 or uniform() < ratio);

    if (log_verbose >=3) std::cerr<<"accept_MH: accept = "<<accept<<endl;

    return accept;
}

bool perform_MH(context_ref& C1,const context_ref& C2,log_double_t rho)
{
    bool accept = accept_MH(C1, C2, rho);
    if (accept)
        C1 = C2;
    return accept;
}


void simplify(json& j);
json flatten_me(const json& j);

void show_parameters(std::ostream& o,const context_ref& C)
{
    auto j = C.get_logged_parameters();
    simplify(j);
    j = flatten_me(j);
    for(auto& [key,j2]: j.items())
        o<<"   "<<key<<" = "<<j2;
    o<<"\n";
    o<<"\n";
}

std::string show_parameters(const context_ref& C)
{
    std::ostringstream oss;
    show_parameters(oss,C);
    return oss.str();
}

