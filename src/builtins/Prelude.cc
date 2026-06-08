#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/operations.H"
#include "util/string/convert.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/gcobject.H"
#include "computation/haskell/Integer.H"
#include "computation/runtime/ast.H"

#include <cerrno>
#include <cstdlib>
#include <limits>

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

//*****************************************************//
extern "C" R::Exp simple_function_truncate(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return trunc(x);
}

extern "C" R::Exp simple_function_ceiling(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return ceil(x);
}

extern "C" R::Exp simple_function_floor(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();
    assert(x > 0.0);

    return floor(x);
}


extern "C" R::Exp simple_function_round(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();
    assert(x > 0.0);

    return round(x);
}

extern "C" R::Exp simple_function_doubleToInt(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();
    int xi = (int)x;
    return xi;
}

extern "C" R::Exp simple_function_divide_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return x / y;
}

extern "C" R::Exp simple_function_divide_logdouble(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return x.as_log_double() / y.as_log_double();
}

extern "C" R::Exp simple_function_recip_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();

    return 1.0/x;
}

extern "C" R::Exp simple_function_recip_logdouble(vector<R::Exp>& args)
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
extern "C" R::Exp simple_function_div_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return div<int>(x,y);
}

extern "C" R::Exp simple_function_mod_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return mod(x,y);
}

// x `quot` y should round towards zero.
// Therefore we use C integer division, which rounds towards 0.
extern "C" R::Exp simple_function_quot_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return x / y;
}

extern "C" R::Exp simple_function_rem_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();
  
    return x % y;
}


// x `quot` y should round towards -infinity
// Also, supposedly (x `quot` y)*y + (x `mod` y) == x
// Therefore, (x `quot` y) = (x - (x `mod` y))/y
extern "C" R::Exp simple_function_div_integer(vector<R::Exp>& args)
{
    using boost::dynamic_pointer_cast;

    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();
  
    return div(x,y);
}

extern "C" R::Exp simple_function_mod_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();
  
    return mod(x,y);
}

// x `quot` y should round towards zero.
// Therefore we use C integer division, which rounds towards 0.
extern "C" R::Exp simple_function_quot_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();

    return integer(x / y);
}

extern "C" R::Exp simple_function_rem_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();
  
    return integer(x % y);
}

extern "C" R::Exp simple_function_increment_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();

    return x + 1;
}


extern "C" R::Exp simple_function_equals_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();

    return x == y;
}

extern "C" R::Exp simple_function_equals_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();

    return x == y;
}

extern "C" R::Exp simple_function_equals_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return x == y;
}

extern "C" R::Exp simple_function_equals_log_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_log_double();
    auto y = get_arg(args).as_log_double();

    return x == y;
}

extern "C" R::Exp simple_function_equals_char(vector<R::Exp>& args)
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

extern "C" R::Exp simple_function_compare_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();

    if (x < y)
        return 0;
    else if (x == y)
        return 1;
    else if (x > y)
        return 2;
    else
        std::abort();
}

extern "C" R::Exp simple_function_compare_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    if (x < y)
        return 0;
    else if (x == y)
        return 1;
    else if (x > y)
        return 2;
    else
        std::abort();
}

extern "C" R::Exp simple_function_compare_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    if (x < y)
        return 0;
    else if (x == y)
        return 1;
    else if (x > y)
        return 2;
    else
        std::abort();
}

extern "C" R::Exp simple_function_lessthan_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return x < y;
}

extern "C" R::Exp simple_function_lessthan_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();
    auto y = get_arg(args).as_int();

    return x < y;
}

extern "C" R::Exp simple_function_lessthan_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();

    return x < y;
}

extern "C" R::Exp simple_function_lessthan_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return x < y;
}

extern "C" R::Exp simple_function_lessthan_log_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_log_double();
    auto y = get_arg(args).as_log_double();

    return x < y;
}

extern "C" R::Exp simple_function_expToLogDouble(vector<R::Exp>& args)
{
    double d = get_arg(args).as_double();
    return exp_to_log_space(d);
}

extern "C" R::Exp simple_function_doubleToLogDouble(vector<R::Exp>& args)
{
    double d = get_arg(args).as_double();
    return log_double_t(d);
}

extern "C" R::Exp simple_function_show_int(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();

    return std::to_string(x);
}

extern "C" R::Exp simple_function_show_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    return x.str();
}

//defined in expression_ref.cc
string double_to_string(double d);

extern "C" R::Exp simple_function_show_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();

    return double_to_string(x);
}

extern "C" R::Exp simple_function_show(vector<R::Exp>& args)
{
    auto x = get_arg(args);
  
    return x.print();
}

#include "computation/machine/error_exception.H"

extern "C" closure builtin_function_error(OperationArgs& Args)
{
    string message = Args.evaluate_slot_to_value(0).as_string();
  
    throw error_exception(message);
}

extern "C" R::Exp simple_function_read_int(vector<R::Exp>& args)
{
    string s = get_arg(args).as_string();

    if (auto i = can_be_converted_to<int>(s))
	return *i;
    else
	throw myexception()<<"Cannot convert string '"<<s<<"' to int!";
}

extern "C" R::Exp simple_function_read_double(vector<R::Exp>& args)
{
    string s = get_arg(args).as_string();

    if (auto d = can_be_converted_to<double>(s))
	return *d;
    else
	throw myexception()<<"Cannot convert string '"<<s<<"' to double!";
}

extern "C" R::Exp simple_function_read_int_at(vector<R::Exp>& args)
{
    string s = get_arg(args).as_string();
    int offset = get_arg(args).as_int();
    if (offset < 0 or offset > s.size())
        return R::RPair(0, -1);

    const char* start = s.c_str() + offset;
    char* end = nullptr;
    errno = 0;
    long value = std::strtol(start, &end, 10);
    if (end == start or errno == ERANGE or value < std::numeric_limits<int>::min() or value > std::numeric_limits<int>::max())
        return R::RPair(0, -1);

    return R::RPair(int(value), offset + int(end - start));
}

extern "C" R::Exp simple_function_read_integer_at(vector<R::Exp>& args)
{
    string s = get_arg(args).as_string();
    int offset = get_arg(args).as_int();
    if (offset < 0 or offset > s.size())
        return R::RPair(R::Integer(integer(0)), -1);

    int pos = offset;
    bool negative = false;
    if (pos < s.size() and (s[pos] == '-' or s[pos] == '+'))
    {
        negative = s[pos] == '-';
        pos++;
    }

    if (pos == s.size() or s[pos] < '0' or s[pos] > '9')
        return R::RPair(R::Integer(integer(0)), -1);

    integer value = 0;
    while(pos < s.size() and s[pos] >= '0' and s[pos] <= '9')
    {
        value *= 10;
        value += s[pos] - '0';
        pos++;
    }

    if (negative)
        value = -value;

    return R::RPair(R::Integer(value), pos);
}

extern "C" R::Exp simple_function_read_double_at(vector<R::Exp>& args)
{
    string s = get_arg(args).as_string();
    int offset = get_arg(args).as_int();
    if (offset < 0 or offset > s.size())
        return R::RPair(0.0, -1);

    const char* start = s.c_str() + offset;
    char* end = nullptr;
    errno = 0;
    double value = std::strtod(start, &end);
    if (end == start or errno == ERANGE)
        return R::RPair(0.0, -1);

    return R::RPair(value, offset + int(end - start));
}

extern "C" R::Exp simple_function_cNothing(vector<R::Exp>&)
{
    return R::RMaybe();
}

extern "C" R::Exp simple_function_cJust(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    return R::RMaybe(x);
}

extern "C" R::Exp simple_function_cIsJust(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& x = arg.as_<R::RMaybe>();
    return bool(x);
}

extern "C" R::Exp simple_function_cFromJust(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& maybe = arg.as_<R::RMaybe>();
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

    return closure(Runtime::ConstructorApp("()", 0, {}));
}

static void peel_closure_lambdas(closure& C, int n)
{
    assert(C.has_code());
    C.set_code(Runtime::peel_lambdas(C.get_code(), n));
}

extern "C" closure builtin_function_catchRaw(OperationArgs& Args)
{
    try
    {
        Args.evaluate_slot_to_closure(0);
        int r = Args.reg_for_slot(0);
        return closure(Runtime::IndexVar(0), {r});
    }
    catch (HaskellException& H)
    {
        closure C = Args.evaluate_slot_to_closure(1);

        peel_closure_lambdas(C, 1);
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

    Runtime::Exp E = Runtime::ConstructorApp("Data.IORef.IORef", 1,
                                             {Runtime::IndexVar(0)});

    int r_ioref = Args.allocate(closure(std::move(E), {r}));

    return closure(Runtime::IndexVar(0), {r_ioref});
}

extern "C" closure builtin_function_readIORef(OperationArgs& Args)
{
    // 1. IORef
    auto C = Args.evaluate_slot_to_closure(0);
    assert(Runtime::has_constructor(C.get_code(),"Data.IORef.IORef"));
    assert(C.Env.size() == 1);

    return closure(Runtime::IndexVar(0), {C.Env[0]});
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
    if (auto object_value = C.get_code().to<Runtime::ObjectValue>(); C.get_code().is_gcable_object_value())
    {
	vector<int> tmp;
	auto gco = convert<GCObject>(object_value->value);
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
    assert(Runtime::has_constructor(C.get_code(),"Data.IORef.IORef"));
    assert(C.Env.size() == 1);

    // 2. New value
    int r_value = Args.reg_for_slot(1);

    // 3. Write the IORef
    auto& M = Args.memory();
    assert(Runtime::has_constructor(M.closure_at(r_ioref).get_code(),"Data.IORef.IORef"));
    if (not M.reg_is_contingent(r_ioref))
    {
	// The IORef is non-contingent, but the value is contingent!

	r_value = copy_out_of_machine(M, Args, r_value);
    }
    C.Env[0] = r_value;
    M.set_C(r_ioref, std::move(C));

    return closure(Runtime::ConstructorApp("()", 0, {}));
}

extern "C" closure builtin_function_modifyIORef(OperationArgs& Args)
{
    // 1. IORef
    int r_ioref = Args.evaluate_slot_unchangeable(0);
    auto C = Args.evaluate_slot_to_closure(0);
    assert(Runtime::has_constructor(C.get_code(),"Data.IORef.IORef"));
    assert(C.Env.size() == 1);
    int r_value = C.Env[0];

    // 2. Function
    int r_func = Args.reg_for_slot(1);

    // 3. Allocate new value
    int r_apply = Args.allocate(closure(Runtime::apply(Runtime::IndexVar(1), {Runtime::IndexVar(0)}),
                                        {r_func, r_value}));

    // 4. Write the IORef
    auto& M = Args.memory();
    assert(Runtime::has_constructor(M.closure_at(r_ioref).get_code(),"Data.IORef.IORef"));
    C.Env[0] = r_apply;
    M.set_C(r_ioref, std::move(C));

    return closure(Runtime::ConstructorApp("()", 0, {}));
}


extern "C" closure builtin_function_modifyIORefStrict(OperationArgs& Args)
{
    // 1. IORef
    int r_ioref = Args.evaluate_slot_unchangeable(0);
    auto C = Args.evaluate_slot_to_closure(0);
    assert(Runtime::has_constructor(C.get_code(),"Data.IORef.IORef"));
    assert(C.Env.size() == 1);
    int r_value = C.Env[0];

    // 2. Function
    int r_func = Args.reg_for_slot(1);

    // 3. Allocate new value
    int r_apply = Args.allocate(closure(Runtime::apply(Runtime::IndexVar(1), {Runtime::IndexVar(0)}),
                                        {r_func, r_value}));

    // 4. Write the IORef
    auto& M = Args.memory();
    assert(Runtime::has_constructor(M.closure_at(r_ioref).get_code(),"Data.IORef.IORef"));
    C.Env[0] = r_apply;
    M.set_C(r_ioref, std::move(C));

    // Force
    r_apply = Args.evaluate_reg_force(r_apply);

    return closure(Runtime::ConstructorApp("()", 0, {}));
}


extern "C" R::Exp simple_function_isWindows(vector<R::Exp>&)
{
#ifdef _WIN32
    return true;
#else
    return false;
#endif
}

/* (ST m) >>= k
    = ST (\s ->
      case (m s) of (# new_s, r #) ->
      case (k r) of (ST k2 ->
      (k2 new_s)))
      
    = ST (andThenST m k s)
*/
extern "C" closure builtin_function_andThenST(OperationArgs& Args)
{
    closure m = Args.evaluate_slot_to_closure(0);

    int s_reg = Args.current_closure().reg_for_slot(2);

    peel_closure_lambdas(m, 1);
    m.Env.push_back(s_reg);

    // ms_result <- perform m as an operation with step S
    // ms_result SHOULD be in WHNF and have for the form (new_s_reg , r_reg)
    int new_s_reg = -1;
    int r_reg = -1;

    
    closure k = Args.evaluate_slot_to_closure(1);
    peel_closure_lambdas(k, 1);
    k.Env.push_back(r_reg);

    // kr_result <- perform k as an operation with step S
    // kr_result SHOULD be in WHNF and have the form (ST k2_reg)

    int k2_reg = -1;

    closure k2 = Args.evaluate_slot_to_closure(k2_reg);
    peel_closure_lambdas(k2, 1);
    k2.Env.push_back(new_s_reg);

    // k2_new_result <- perform k2 as an operation with step S
    // k2_new_result should have the form (new_s2, x)

    // PROBLEM: The uses/forces of a reg should be FIXED and not depend on values of used regs,
    //            since those can change.
    //          So its ok to evaluate m and k, since those depend on the regs passed in.
    //          But its NOT ok to evaluate (m s), (k r), and (k2 new_s), since those depend
    //             on the VALUES passed in!

    // Normally we allocate a new expression and call it when the next step of evaluation depends
    // on the inputs.  However, our current architecture means that we should RE-PERFORM the next
    // action on the output of the current action, whereas we actually want to invalidate the current
    // action (m s) if the results of the next action (k2 new_s) change.
    std::exit(1);
}
