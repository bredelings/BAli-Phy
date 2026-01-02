#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include "computation/operations.H"
#include "util/string/convert.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/gcobject.H"
#include "computation/haskell/Integer.H"

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

//*****************************************************//
extern "C" expression_ref simple_function_truncate(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return trunc(x);
}

extern "C" expression_ref simple_function_ceiling(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return ceil(x);
}

extern "C" expression_ref simple_function_floor(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();
    assert(x > 0.0);

    return floor(x);
}


extern "C" expression_ref simple_function_round(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();
    assert(x > 0.0);

    return round(x);
}

extern "C" expression_ref simple_function_doubleToInt(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();
    int xi = (int)x;
    return xi;
}

extern "C" expression_ref simple_function_divide_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return x / y;
}

extern "C" expression_ref simple_function_divide_logdouble(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return x.as_log_double() / y.as_log_double();
}

extern "C" expression_ref simple_function_recip_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();

    return 1.0/x;
}

extern "C" expression_ref simple_function_recip_logdouble(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_log_double();
    x.log() = -x.log();

    return x;
}

// Also see function `div_t div(int,int)` in stdlib.h

// mod is always positive
template <typename T>
T mod(T x, T y)
{
    T z = x % y;
    if (z < 0)
	z += y;
    return z;
}

// Since div subtracts the mod, it rounds down
template <typename T>
T div(T x, T y)
{
    return (x - mod(x,y))/y;
}

// I think actually Char is a unicode character, not a 1-byte object.


// x `quot` y should round towards -infinity
// Also, supposedly (x `quot` y)*y + (x `mod` y) == x
// Therefore, (x `quot` y) = (x - (x `mod` y))/y
extern "C" expression_ref simple_function_div_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return div<int>(x,y);
}

extern "C" expression_ref simple_function_mod_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return mod(x,y);
}

// x `quot` y should round towards zero.
// Therefore we use C integer division, which rounds towards 0.
extern "C" expression_ref simple_function_quot_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return x / y;
}

extern "C" expression_ref simple_function_rem_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return x % y;
}


// x `quot` y should round towards -infinity
// Also, supposedly (x `quot` y)*y + (x `mod` y) == x
// Therefore, (x `quot` y) = (x - (x `mod` y))/y
extern "C" expression_ref simple_function_div_integer(vector<expression_ref>& args)
{
    using boost::dynamic_pointer_cast;

    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();
  
    return Integer(div(x,y));
}

extern "C" expression_ref simple_function_mod_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();
  
    return Integer(mod(x,y));
}

// x `quot` y should round towards zero.
// Therefore we use C integer division, which rounds towards 0.
extern "C" expression_ref simple_function_quot_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();

    return Integer(x / y);
}

extern "C" expression_ref simple_function_rem_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();
  
    return Integer(x % y);
}

extern "C" expression_ref simple_function_increment_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();

    return x + 1;
}


extern "C" expression_ref simple_function_equals_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();

    return x == y;
}

extern "C" expression_ref simple_function_equals_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();

    return x == y;
}

extern "C" expression_ref simple_function_equals_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return x == y;
}

extern "C" expression_ref simple_function_equals_log_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_log_double();
    auto y = get_arg(args).as_log_double();

    return x == y;
}

extern "C" expression_ref simple_function_equals_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return x == y;
}

enum Ordering {LT=4,GT=5,EQ=6,DontKnow=7};

template <typename T>
Ordering compare(const T& x, const T& y)
{
    if (x < y) return Ordering::LT;
    if (x > y) return Ordering::GT;
    return Ordering::EQ;
}

extern "C" expression_ref simple_function_compare_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();

    if (x < y)
        return expression_ref(0); // avoid constructing from (Object*)0
    else if (x == y)
        return 1;
    else if (x > y)
        return 2;
    else
        std::abort();
}

extern "C" expression_ref simple_function_compare_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    if (x < y)
        return expression_ref(0); // avoid constructing from (Object*)0
    else if (x == y)
        return 1;
    else if (x > y)
        return 2;
    else
        std::abort();
}

extern "C" expression_ref simple_function_compare_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    if (x < y)
        return expression_ref(0); // avoid constructing from (Object*)0
    else if (x == y)
        return 1;
    else if (x > y)
        return 2;
    else
        std::abort();
}

extern "C" expression_ref simple_function_lessthan_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return x < y;
}

extern "C" expression_ref simple_function_lessthan_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();

    return x < y;
}

extern "C" expression_ref simple_function_lessthan_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();

    return x < y;
}

extern "C" expression_ref simple_function_lessthan_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return x < y;
}

extern "C" expression_ref simple_function_lessthan_log_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_log_double();
    auto y = get_arg(args).as_log_double();

    return x < y;
}

extern "C" expression_ref simple_function_expToLogDouble(vector<expression_ref>& args)
{
    double d = get_arg(args).as_double();
    return exp_to<log_double_t>(d);
}

extern "C" expression_ref simple_function_doubleToLogDouble(vector<expression_ref>& args)
{
    double d = get_arg(args).as_double();
    return log_double_t(d);
}

extern "C" expression_ref simple_function_show_int(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();

    object_ptr<String> v (new String);
    *v = std::to_string(x);
    return v;
}

extern "C" expression_ref simple_function_show_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    object_ptr<String> v(new String);
    *v = x.str();
    return v;
}

//defined in expression_ref.cc
string double_to_string(double d);

extern "C" expression_ref simple_function_show_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();

    object_ptr<String> v (new String(double_to_string(x)));

    return v;
}

extern "C" expression_ref simple_function_show(vector<expression_ref>& args)
{
    auto x = get_arg(args);
  
    object_ptr<String> v (new String);
    *v = x.print();
    return v;
}

#include "computation/machine/error_exception.H"

extern "C" closure builtin_function_error(OperationArgs& Args)
{
    string message = Args.evaluate(0).as_<String>();
  
    throw error_exception(message);
}

extern "C" expression_ref simple_function_read_int(vector<expression_ref>& args)
{
    string s = get_arg(args).as_<String>();

    if (auto i = can_be_converted_to<int>(s))
	return *i;
    else
	throw myexception()<<"Cannot convert string '"<<s<<"' to int!";
}

extern "C" expression_ref simple_function_read_double(vector<expression_ref>& args)
{
    string s = get_arg(args).as_<String>();

    if (auto d = can_be_converted_to<double>(s))
	return *d;
    else
	throw myexception()<<"Cannot convert string '"<<s<<"' to double!";
}

extern "C" expression_ref simple_function_cNothing(OperationArgs&)
{
    return EMaybe();
}

extern "C" expression_ref simple_function_cJust(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    return EMaybe(x);
}

extern "C" expression_ref simple_function_cIsJust(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_<EMaybe>();
    return bool(x);
}

extern "C" expression_ref simple_function_cFromJust(vector<expression_ref>& args)
{
    auto maybe = get_arg(args).as_<EMaybe>();
    return *maybe;
}

struct HaskellException
{
    int r;
};

extern "C" closure builtin_function_throw(OperationArgs& Args)
{
    int r = Args.reg_for_slot(0);

    throw HaskellException{r};

    return constructor("()",0);
}

int get_n_lambdas(const expression_ref& E);
expression_ref peel_n_lambdas(const expression_ref& E, int n);

extern "C" closure builtin_function_catchRaw(OperationArgs& Args)
{
    try
    {
        Args.evaluate(0);
        int r = Args.reg_for_slot(0);
        return {index_var(0),{r}};
    }
    catch (HaskellException& H)
    {
        closure C = Args.evaluate_slot_to_closure(1);

        C.exp = peel_n_lambdas(C.exp, 1);
        C.Env.push_back(H.r);

        // If we have only 1 arg, we can always apply it.
        return C;
    }
}

//*****************************************************//
extern "C" closure builtin_function_newIORef(OperationArgs& Args)
{
    // 1. Initial value
    int r = Args.reg_for_slot(0);

    expression_ref E(constructor("Data.IORef.IORef",1),{index_var(0)});

    int r_ioref = Args.allocate(closure{E,{r}});

    return {index_var(0), {r_ioref}};
}

extern "C" closure builtin_function_readIORef(OperationArgs& Args)
{
    // 1. IORef
    auto C = Args.evaluate_slot_to_closure(0);
    assert(has_constructor(C.exp,"Data.IORef.IORef"));
    assert(C.Env.size() == 1);

    return {index_var(0), {C.Env[0]}};
}

int copy_out_of_machine(reg_heap& M, OperationArgs& Args, int r)
{
    // DO: make an object that pins a reg in memory until it is destroyed.
    // push_temp head KIND of works, but the stack-based approach is problematic.
    // We should use a hash.
    // So, we would have a hash map of heads -> counts inside reg_heap.

    // 1. If the reg r is already non-contingent, use it direcctly.
    if (not M.reg_is_contingent(r)) return r;

    // 2. Evaluate the reg to a constant.
    auto C = Args.evaluate_reg_to_closure(r);

    // 3. Complain if there the object isn't atomic
    if (is_gcable_type(C.exp.type()))
    {
	vector<int> tmp;
	auto gco = convert<GCObject>(C.exp.ptr());
	gco->get_regs(tmp);
	for(int& r: tmp)
	    r = copy_out_of_machine(M, Args, r);

	if (tmp.size())
	    throw myexception()<<"copy_out_of_machine: currently we don't handle GCObjects";
    }
    else
    {
	for(int& r: C.Env)
	    r = copy_out_of_machine(M, Args, r);
    }

    return Args.allocate_non_contingent(std::move(C));
}


extern "C" closure builtin_function_writeIORef(OperationArgs& Args)
{
    // 1. IORef
    int r_ioref = Args.evaluate_slot_unchangeable(0);
    auto C = Args.evaluate_slot_to_closure(0);
    assert(has_constructor(C.exp,"Data.IORef.IORef"));
    assert(C.Env.size() == 1);

    // 2. New value
    int r_value = Args.reg_for_slot(1);

    // 3. Write the IORef
    auto& M = Args.memory();
    assert(has_constructor(M.expression_at(r_ioref),"Data.IORef.IORef"));
    if (not M.reg_is_contingent(r_ioref))
    {
	// The IORef is non-contingent, but the value is contingent!

	r_value = copy_out_of_machine(M, Args, r_value);
    }
    C.Env[0] = r_value;
    M.set_C(r_ioref, std::move(C));

    return constructor("()",0);
}

extern "C" closure builtin_function_modifyIORef(OperationArgs& Args)
{
    // 1. IORef
    int r_ioref = Args.evaluate_slot_unchangeable(0);
    auto C = Args.evaluate_slot_to_closure(0);
    assert(has_constructor(C.exp,"Data.IORef.IORef"));
    assert(C.Env.size() == 1);
    int r_value = C.Env[0];

    // 2. Function
    int r_func = Args.reg_for_slot(1);

    // 3. Allocate new value
    expression_ref fE = index_var(1);
    expression_ref argE = index_var(0);
    expression_ref apply_exp = {fE, argE};

    int r_apply = Args.allocate({apply_exp,{r_func, r_value}});

    // 4. Write the IORef
    auto& M = Args.memory();
    assert(has_constructor(M.expression_at(r_ioref),"Data.IORef.IORef"));
    C.Env[0] = r_apply;
    M.set_C(r_ioref, std::move(C));

    return constructor("()",0);
}


extern "C" closure builtin_function_modifyIORefStrict(OperationArgs& Args)
{
    // 1. IORef
    int r_ioref = Args.evaluate_slot_unchangeable(0);
    auto C = Args.evaluate_slot_to_closure(0);
    assert(has_constructor(C.exp,"Data.IORef.IORef"));
    assert(C.Env.size() == 1);
    int r_value = C.Env[0];

    // 2. Function
    int r_func = Args.reg_for_slot(1);

    // 3. Allocate new value
    expression_ref fE = index_var(1);
    expression_ref argE = index_var(0);
    expression_ref apply_exp = {fE, argE};

    int r_apply = Args.allocate({apply_exp,{r_func, r_value}});

    // 4. Write the IORef
    auto& M = Args.memory();
    assert(has_constructor(M.expression_at(r_ioref),"Data.IORef.IORef"));
    C.Env[0] = r_apply;
    M.set_C(r_ioref, std::move(C));

    // Force
    r_apply = Args.evaluate_reg_force(r_apply);

    return constructor("()",0);
}


extern "C" expression_ref simple_function_isWindows(vector<expression_ref>&)
{
#ifdef _WIN32
    return bool_true;
#else
    return bool_false;
#endif
}

