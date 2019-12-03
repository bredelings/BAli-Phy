//#define DEBUG_OPTIMIZE

#include <memory>
#include "computation/context.H"
#include "computation/machine/graph_register.H"
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

using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;
using std::unique_ptr;
using boost::dynamic_pointer_cast;
using std::optional;

using std::cerr;
using std::endl;

template <typename T>
using Bounds = Box<bounds<T>>;

object_ptr<reg_heap>& context_ref::memory() const {return memory_;}

const std::vector<int>& context_ref::heads() const {return memory()->get_heads();}

std::vector<std::pair<std::string,int>>& context_ref::parameters() const {return memory()->get_parameters();}

std::map<std::string, int>& context_ref::identifiers() const {return memory()->get_identifiers();}

const closure& context_ref::operator[](int i) const {return (*memory())[i];}

closure context_ref::preprocess(const closure& C) const
{
    return memory()->preprocess(C);
}

string context_ref::parameter_name(int i) const
{
    return parameters()[i].first;
}

void context_ref::rename_parameter(int i, const string& new_name)
{
    parameters()[i].first = new_name;
}

/// Return the value of a particular index, computing it if necessary
const closure& context_ref::lazy_evaluate(int index) const
{
    return memory()->lazy_evaluate_head(index, context_index);
}

/// Return the value of a particular index, computing it if necessary
const expression_ref& context_ref::evaluate(int index) const
{
    return lazy_evaluate(index).exp;
}

/// Return the value of a particular index, computing it if necessary
const expression_ref& context_ref::perform(int index, bool ec) const
{
    int H = heads()[index];

    return perform_expression(reg_var(H), ec);
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

expression_ref context_ref::recursive_evaluate_parameter(int i) const
{
    return recursive_evaluate_reg(get_parameter_reg(i));
}

expression_ref context_ref::recursive_evaluate(int i) const
{
    return recursive_evaluate_reg(get_compute_expression_reg(i));
}

int get_reps(double x)
{
    int x_int = (int)x;
    double x_frac = x - x_int;
    return x_int + poisson(x_frac);
}

void context_ref::run_transition_kernels()
{
    vector<pair<double,int>> weighted_tks;
    // Don't use a range-for, since the number of transition kernels could change
    for(int i=0; i< memory()->transition_kernels().size(); ++i)
    {
        auto& [r_rate, r_kernel] = memory()->transition_kernels()[i];

        // We could force new random variables here, which would be weird.
        double rate = get_reg_value(r_rate).as_double();
        if (rate > 0)
            weighted_tks.push_back({rate, r_kernel});
    }

    vector<int> order;

    for(auto& [w,r]: weighted_tks)
    {
        memory()->mark_transition_kernel_active(r);

        int n = get_reps(w);
        for(int j=0;j<n;j++)
            order.push_back(r);
    }

    random_shuffle(order);

    for(int move: order)
        if (memory()->transition_kernel_is_active(move))
            perform_transition_kernel(move);

    for(auto& [w,r]: weighted_tks)
        memory()->clear_transition_kernel_active(r);
}

void context_ref::perform_transition_kernel(int r)
{
    expression_ref E = {reg_var(r), get_context_index()};
    perform_expression(E);
}

int context_ref::n_transition_kernels() const
{
    return memory()->transition_kernels().size();
}

optional<int> context_ref::parameter_is_modifiable_reg(int index) const
{
    return memory()->parameter_is_modifiable_reg(index);
}

optional<int> context_ref::compute_expression_is_modifiable_reg(int index) const
{
    return memory()->compute_expression_is_modifiable_reg(index);
}

optional<int> context_ref::compute_expression_is_random_variable(int index) const
{
    return memory()->compute_expression_is_random_variable(index);
}

bool context_ref::compute_expression_has_bounds(int index) const
{
    auto R = compute_expression_is_random_variable(index);
    if (not R) return false;
    auto e = get_range_for_random_variable(*R);

    return (e and e.is_a<Bounds<double>>());
}

bounds<double> context_ref::get_bounds_for_compute_expression(int index) const
{
    auto R = compute_expression_is_random_variable(index);
    auto e = get_range_for_random_variable(*R);

    assert(e);

    if (not e.is_a<Bounds<double>>())
	throw myexception()<<"compute expression "<<index<<" doesn't have bounds<double>.";

    return e.as_<Bounds<double>>();
}

EVector context_ref::get_parameter_values(const std::vector<int>& indices) const
{
    EVector values(indices.size());

    for(int i=0;i<values.size();i++)
	values[i] = get_parameter_value(indices[i]);

    return values;
}

bool context_ref::parameter_has_bounds(int i) const
{
    auto e = get_parameter_range(i);

    return (e and e.is_a<Bounds<double>>());
}

const bounds<double>& context_ref::get_parameter_bounds(int i) const
{
    auto e = get_parameter_range(i);

    assert(e);

    if (not e.is_a<Bounds<double>>())
	throw myexception()<<"parameter '"<<parameter_name(i)<<"' doesn't have bounds<double>.";

    return e.as_<Bounds<double>>();
}

EVector context_ref::get_modifiable_values(const std::vector<int>& indices) const
{
    EVector values(indices.size());

    for(int i=0;i<values.size();i++)
	values[i] = get_modifiable_value(indices[i]);

    return values;
}

void context_ref::set_parameter_values(const vector<int>& indices,const vector<expression_ref>& p)
{
    assert(indices.size() == p.size());

    for(int i=0;i<indices.size();i++)
	set_parameter_value(indices[i], p[i]);
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
const expression_ref& context_ref::get_reg_value(int R) const
{
    return memory()->get_reg_value_in_context(R, context_index);
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
const expression_ref& context_ref::get_modifiable_value(int R) const
{
    return get_reg_value(*get_modifiable_reg(R));
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
const expression_ref& context_ref::get_parameter_value(int index) const
{
    return memory()->get_parameter_value_in_context(index, context_index);
}

/// Get the value of a non-constant, non-computed index
const expression_ref& context_ref::get_parameter_value(const std::string& name) const
{
    auto index = find_parameter(name);

    return get_parameter_value(index);
}

void context_ref::set_modifiable_value_(int R, closure&& C)
{
    set_reg_value(*get_modifiable_reg(R), std::move(C) );
}

void context_ref::set_modifiable_value(int R, const expression_ref& E)
{
    assert(not E.size());
    assert(not E.is_index_var());
    assert(not E.is_a<reg_var>());
    assert(not E.is_a<var>());
    set_modifiable_value_(R, E);
}

void context_ref::set_parameter_value(int index, const expression_ref& E)
{
    assert(not E.size());
    assert(not E.is_index_var());
    assert(not E.is_a<reg_var>());
    assert(not E.is_a<var>());
    set_parameter_value_(index, E);
}

void context_ref::set_parameter_value_(int index, closure&& C)
{
    assert(index >= 0);

    auto P = parameter_is_modifiable_reg(index);

    set_reg_value(*P, std::move(C) );
}

void context_ref::set_reg_value(int P, closure&& C)
{
    memory()->set_reg_value_in_context(P, std::move(C), context_index);
}

/// Update the value of a non-constant, non-computed index
void context_ref::set_parameter_value(const std::string& var, const expression_ref& O)
{
    int i = find_parameter(var);
    
    set_parameter_value(i, O);
}

int context_ref::n_parameters() const
{
    return parameters().size();
}

optional<int> context_ref::maybe_find_parameter(const string& s) const
{
    return memory()->maybe_find_parameter(s);
}

int context_ref::find_parameter(const string& s) const
{
    return memory()->find_parameter(s);
}

param context_ref::new_modifiable(const expression_ref& value)
{
    expression_ref M{var("Parameters.modifiable"), value};
    return add_compute_expression(M);
}

int context_ref::add_modifiable_parameter(const string& full_name, const expression_ref& value)
{
    expression_ref M(modifiable(),{value});
    int p = n_parameters();
    memory()->add_parameter(full_name, M);
    return p;
}

const vector<int>& context_ref::random_variables() const
{
    return memory()->random_variables();
}

const expression_ref context_ref::get_range_for_random_variable(int r) const
{
    return memory()->get_range_for_random_variable(context_index, r);
}

double context_ref::get_rate_for_random_variable(int r) const
{
    return memory()->get_rate_for_random_variable(context_index, r);
}

const expression_ref context_ref::get_parameter_range(int p) const
{
    return memory()->get_parameter_range(context_index, p);
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

int context_ref::add_likelihood_factor(const expression_ref& E)
{
    return memory()->register_likelihood(preprocess(E));
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

void context_ref::show_graph() const
{
    prior();
    collect_garbage();
    int t = memory()->token_for_context(context_index);
    dot_graph_for_token(*memory(), t);
}

expression_ref context_ref::evaluate_program() const
{
    return memory()->evaluate_program(context_index);
}


json context_ref::get_logged_parameters() const
{
    if (not memory()->logging_head)
	throw myexception()<<"No logging head has been set!";

    auto L = evaluate(*memory()->logging_head);
    return L.as_checked<Box<json>>().value();
}

int context_ref::get_parameter_reg(int index) const
{
    assert(index >= 0 and index < n_parameters());

    return parameters()[index].second;
}

int context_ref::get_compute_expression_reg(int index) const
{
    assert(index >= 0 and index < heads().size());

    return heads()[index];
}

optional<int> context_ref::get_modifiable_reg(int r) const
{
    return memory()->find_modifiable_reg(r);
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

Program& context_ref::get_Program()
{
    if (not memory()->P)
	throw myexception()<<"Program used after being cleared!";

    return *(memory()->P);
}

const Program& context_ref::get_Program() const
{
    if (not memory()->P)
	throw myexception()<<"Program used after being cleared!";

    return *(memory()->P);
}

void context_ref::clear_program()
{
    memory()->P.reset();
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

int context::add_program(const expression_ref& E)
{
    return memory()->add_program(E, context_index);
}

int context::add_identifier(const string& name) const
{
    if (maybe_find_parameter(name))
	throw myexception()<<"Cannot add identifier '"<<name<<"': there is already a parameter with that name.";

    return memory()->add_identifier(name);
}

void context::allocate_identifiers_for_modules(const vector<string>& module_names)
{
    // 2. Give each identifier a pointer to an unused location; define parameter bodies.
    for(const auto& name: module_names)
    {
	const Module& M = get_Program().get_module(name);

	for(const auto& [name,_]: M.code_defs())
	    add_identifier(name);
    }
      
    // 3. Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
    for(const auto& module_name: module_names)
    {
	const Module& M = get_Program().get_module(module_name);

	for(const auto& [name,body]: M.code_defs())
	{
	    // get the root for each identifier
	    auto loc = identifiers().find(name);
	    assert(loc != identifiers().end());
	    int R = loc->second;

#ifdef DEBUG_OPTIMIZE
	    std::cerr<<name<<" := "<<body<<"\n\n";
	    std::cerr<<name<<" := "<<preprocess(body).exp<<"\n\n\n\n";
#endif

	    // load the body into the machine
	    assert(R != -1);
	    memory()->set_C(R, preprocess(body) );
	}
    }
}

context& context::operator+=(const string& module_name)
{
    if (not get_Program().contains_module(module_name))
	(*this) += get_Program().get_module_loader()->load_module(module_name);

    return *this;
}

context& context::operator+=(const vector<string>& module_names)
{
    for(const auto& name: module_names)
	(*this) += name;

    return *this;
}

// \todo FIXME:cleanup If we can make this only happen once, we can assume old_module_names is empty.
context& context::operator+=(const Module& M)
{
    Program& PP = get_Program();

    // Get module_names, but in a set<string>
    set<string> old_module_names = PP.module_names_set();

    // 1. Add the new modules to the program, add notes, perform imports, and resolve symbols.
    get_Program().add(M);

    // 2. Give each identifier a pointer to an unused location; define parameter bodies.
    vector<string> new_module_names;
    for(auto& module: PP)
	if (not old_module_names.count(module.name))
	    new_module_names.push_back(module.name);

    allocate_identifiers_for_modules(new_module_names);

    return *this;
}

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

context::context(const Program& P)
    :context_ref(*new reg_heap(P.get_module_loader()))
{
    context_index = memory_->get_unused_context();

    for(auto& M: P.modules())
	(*this) += M;

    // For Prelude.unsafePerformIO
    if (not P.size())
	(*this) += "Prelude";

    memory_->add_perform_io_head();
}

context::~context()
{
    memory_->release_context(context_index);
}

bool accept_MH(const context_ref& C1,const context_ref& C2,log_double_t rho)
{
    if (log_verbose >= 3)
    {
        std::cerr<<"accept_MH: rho = "<<rho<<endl;

        show_parameters(std::cerr,C1);
        std::cerr<<C1.probability()<<" = "<<C1.likelihood()<<" + "<<C1.prior()<<endl;
        std::cerr<<endl;

        show_parameters(std::cerr,C2);
        std::cerr<<C2.probability()<<" = "<<C2.likelihood()<<" + "<<C2.prior();
        std::cerr<<endl<<endl;
    }

    log_double_t ratio = rho*C2.heated_probability_ratio(C1);

    bool accept = (ratio >= 1.0 or uniform() < ratio);

    if (log_verbose >=3) std::cerr<<"accept_MH: accept = "<<accept<<endl;

    return accept;
}

void simplify(json& j);
json flatten_me(const json& j);

void show_parameters(std::ostream& o,const context_ref& C, bool show_hidden) {
    for(int i=0;i<C.n_parameters();i++) {
	string name = C.parameter_name(i);
	if ((not show_hidden) and name.size() > 1 and name[0] == '*') continue;

	o<<"    "<<name<<" = ";

	string output = C.recursive_evaluate_parameter(i).print();
	if (output.find(10) != string::npos or output.find(13) != string::npos)
	    output = "[multiline]";
	o<<output;
    }
    auto j = C.get_logged_parameters();
    simplify(j);
    j = flatten_me(j);
    for(auto& [key,j2]: j.items())
        o<<"   "<<key<<" = "<<j2;
    o<<"\n";
    o<<"\n";
}

std::string show_parameters(const context_ref& C, bool show_hidden)
{
    std::ostringstream oss;
    show_parameters(oss,C, show_hidden);
    return oss.str();
}

/// \brief Check if the model C has a parameter called name
///
/// \param C      The model
/// \param name   A parameter name
///
bool has_parameter(const context_ref& C, const string& name)
{
    for(int i=0;i<C.n_parameters();i++)
	if (C.parameter_name(i) == name)
	    return true;
    return false;
}

