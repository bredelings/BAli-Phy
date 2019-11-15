#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include "computation/operations.H"
#include "util/string/convert.H"
#include "computation/machine/graph_register.H"

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

extern "C" closure builtin_function_is_int(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);

    if (arg.is_int())
	return {bool_true};
    else
	return {bool_false};
}

extern "C" closure builtin_function_is_double(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);

    if (arg.is_double())
	return {bool_true};
    else
	return {bool_false};
}

extern "C" closure builtin_function_is_char(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);

    if (arg.is_char())
	return {bool_true};
    else
	return {bool_false};
}

extern "C" closure builtin_function_truncate(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {trunc(x)};
}

extern "C" closure builtin_function_ceiling(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {ceil(x)};
}

extern "C" closure builtin_function_floor(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();
    assert(x > 0.0);

    return {floor(x)};
}


extern "C" closure builtin_function_round(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();
    assert(x > 0.0);

    return {round(x)};
}

extern "C" closure builtin_function_doubleToInt(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();
    int xi = (int)x;
    return {xi};
}

extern "C" closure builtin_function_add(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() + y.as_double()};
    else if (x.is_int())
	return {x.as_int() + y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() + y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() + y.as_char()};
    else
	throw myexception()<<"Add: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_multiply(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() * y.as_double()};
    else if (x.is_int())
	return {x.as_int() * y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() * y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() * y.as_char()};
    else
	throw myexception()<<"Multiply: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_divide(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() / y.as_double()};
    else if (x.is_int())
	return {double(x.as_int()) / double(y.as_int())};
    else if (x.is_log_double())
	return {x.as_log_double() / y.as_log_double()};
    else if (x.is_char())
	return {double(x.as_char()) / double(y.as_char())};
    else
	throw myexception()<<"Divide: object '"<<x.print()<<"' is not double, int, log_double, or char'";
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

// x `quot` y should round towards -infinity
// Also, supposedly (x `quot` y)*y + (x `mod` y) == x
// Therefore, (x `quot` y) = (x - (x `mod` y))/y
extern "C" closure builtin_function_div(OperationArgs& Args)
{
    using boost::dynamic_pointer_cast;

    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_int())
	return { div<int>(x.as_int(),y.as_int()) };
    else if (x.is_char())
	return { div<char>(x.as_char(),y.as_char())};
    else
	throw myexception()<<"div: object '"<<x.print()<<"' is not int, or char'";
}

extern "C" closure builtin_function_mod(OperationArgs& Args)
{
    using boost::dynamic_pointer_cast;

    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_int())
	return {mod(x.as_int(),y.as_int())};
    else if (x.is_char())
	return {mod(x.as_char(),y.as_char())};
    else
	throw myexception()<<"mod: object '"<<x.print()<<"' is not int, or char'";
}

// x `quot` y should round towards zero.
// Therefore we use C integer division, which rounds towards 0.
extern "C" closure builtin_function_quot(OperationArgs& Args)
{
    using boost::dynamic_pointer_cast;

    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_int())
	return { x.as_int() / y.as_int() };
    else if (x.is_char())
	return { x.as_char() / y.as_char() };
    else
	throw myexception()<<"quot: object '"<<x.print()<<"' is not int, or char'";
}

extern "C" closure builtin_function_rem(OperationArgs& Args)
{
    using boost::dynamic_pointer_cast;

    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_int())
	return { x.as_int() % y.as_int() };
    else if (x.is_char())
	return { x.as_char() % y.as_char() };
    else
	throw myexception()<<"rem: object '"<<x.print()<<"' is not int, or char'";
}

extern "C" closure builtin_function_subtract(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() - y.as_double()};
    else if (x.is_int())
	return {x.as_int() - y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() - y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() - y.as_char()};
    else
	throw myexception()<<"Minus: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_negate(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
  
    if (x.is_double())
	return {-x.as_double()};
    else if (x.is_int())
	return {-x.as_int()};
    else if (x.is_char())
	return {-x.as_char()};
    else
	throw myexception()<<"Negate: object '"<<x.print()<<"' is not double, int, or char'";
}

extern "C" closure builtin_function_get_n_args(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    return {expression_ref(int(x.size()))};
}

extern "C" closure builtin_function_get_arg(OperationArgs& Args)
{
    auto& x = Args.evaluate_slot_to_closure(0);

    int i = Args.evaluate(1).as_int();

    if (i < 0 or i >= x.exp.size())
        throw myexception()<<"Prelude:get_arg: Can't access argument "<<i<<" of '"<<x.print()<<"'";

    int r = x.reg_for_slot(i);

    return closure{index_var(0),{r}};
}

extern "C" closure builtin_function_equals_top(OperationArgs& Args)
{
    constexpr int False = 1;
    constexpr int True =  2;
    constexpr int DontKnow = 3;

    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);

    // 1. If the heads aren't equal, then x /= y.
    if (x.head() != y.head()) return {expression_ref(False)};

    // 2. If the number of argumets isn't equal, then x /= y
    if (x.size() != y.size()) return {expression_ref(False)};

    // 3. If x and y have no arguments, then x == y
    if (x.size() == 0) return {expression_ref(True)};

    // 4. Otherwise we dont know.
    return {expression_ref(DontKnow)};
}

template <typename T>
int compare(const T& x, const T& y)
{
    if (x < y) return -1;
    if (x > y) return 1;
    return 0;
}

int recursive_compare(OperationArgs& Args, int r1, int r2)
{
    Args.stack_push(r1);
    Args.stack_push(r2);

    auto do_return = [&](int i){
                         Args.stack_pop(r2);
                         Args.stack_pop(r1);
                         return i;
                     };

    // 0. We aren't going to record any uses or forces.  We just want to extract information.
    reg_heap& M = Args.memory();

    // 1. First evaluate the regs.  This will yield not any index_vars.
    int r1e = Args.evaluate_reg_to_reg(r1);
    int value1 = M.value_for_reg(r1e);

    int r2e = Args.evaluate_reg_to_reg(r2);
    int value2 = M.value_for_reg(r2e);

    // 2. Then check if the two expressions have a different head.
    auto& x = M.expression_at(value1);
    auto& y = M.expression_at(value2);

    std::optional<int> cmp;
    if (x.is_int())
        cmp = compare(x.as_int(), y.as_int());
    else if (x.is_double())
        cmp = compare(x.as_double(), y.as_double());
    else if (x.is_log_double())
        cmp = compare(x.as_log_double(), y.as_log_double());
    else if (x.is_char())
        cmp = compare(x.as_char(), y.as_char());
    else if (x.is_a<String>())
        cmp = compare<string>(x.as_<String>(), y.as_<String>());
    if (cmp)
        return do_return(*cmp);

    if (not x.head().is_a<constructor>())
        throw myexception()<<"builtin_compare: `"<<M[value1].print()<<"` and `"<<M[value2].print()<<"` can't be compared!";

    // Constructors with fewer arguments are less
    auto xc = x.head().as_<constructor>();
    auto yc = y.head().as_<constructor>();

    cmp = compare( xc.n_args(), yc.n_args() );
    if (*cmp != 0)
        return do_return(*cmp);

    // For constructors with the same numbers of arguments, compare the name string.
    cmp = compare<string>(xc.f_name, yc.f_name);
    if (*cmp != 0)
        return do_return(*cmp);

    const int arity = M.expression_at(value1).size();
    assert(M.expression_at(value2).size() == arity);
    assert(xc.n_args() == arity);
    assert(yc.n_args() == arity);

    // 3. Then compare the arguments
    for(int i=0; i<arity; i++)
    {
        assert(i >0 or is_constructor_exp(M.expression_at(value1)));

        int arg1 = M[value1].reg_for_slot(i);
        int arg2 = M[value2].reg_for_slot(i);
        int cmp_arg = recursive_compare(Args, arg1, arg2);
        if (cmp_arg != 0)
            return do_return(cmp_arg);
    }

    return do_return(0);
}

extern "C" closure builtin_function_recursive_compare(OperationArgs& Args)
{
    auto r1 = Args.evaluate_slot_to_reg(0);
    auto r2 = Args.evaluate_slot_to_reg(1);

    int result = recursive_compare(Args, r1, r2);

    return {expression_ref(result)};
}

extern "C" closure builtin_function_greaterthan(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() > y.as_double()};
    else if (x.is_int())
	return {x.as_int() > y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() > y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() > y.as_char()};
    else
	throw myexception()<<">: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_greaterthanorequal(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() >= y.as_double()};
    else if (x.is_int())
	return {x.as_int() >= y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() >= y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() >= y.as_char()};
    else
	throw myexception()<<">=: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_lessthan(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() < y.as_double()};
    else if (x.is_int())
	return {x.as_int() < y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() < y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() < y.as_char()};
    else
	throw myexception()<<"<: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_lessthanorequal(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() <= y.as_double()};
    else if (x.is_int())
	return {x.as_int() <= y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() <= y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() <= y.as_char()};
    else
	throw myexception()<<"<=: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_doubleToLogDouble(OperationArgs& Args)
{
    double d = Args.evaluate(0).as_double();
    return {log_double_t(d)};
}

extern "C" closure builtin_function_intToDouble(OperationArgs& Args)
{
    int i = Args.evaluate(0).as_int();
    return {double(i)};
}

extern "C" closure builtin_function_seq(OperationArgs& Args)
{
    int x = Args.reg_for_slot(0);
    int y = Args.reg_for_slot(1);

    expression_ref E(Seq(),{index_var(1),index_var(0)});

    return closure{E,{x,y}};
}

extern "C" closure builtin_function_join(OperationArgs& Args)
{
    int x = Args.reg_for_slot(0);
    int y = Args.reg_for_slot(1);

    expression_ref E(Join(),{index_var(1),index_var(0)});

    return closure{E,{x,y}};
}

extern "C" closure builtin_function_show(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
  
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

extern "C" closure builtin_function_putStrLn(OperationArgs& Args)
{
    string message = Args.evaluate(0).as_<String>();

    int state = Args.evaluate(1).as_int();

    std::cout<<message<<std::endl;

    return EPair(state+1, constructor("()",0));
}

extern "C" closure builtin_function_read_int(OperationArgs& Args)
{
    string s = Args.evaluate(0).as_<String>();

    if (auto i = can_be_converted_to<int>(s))
	return {*i};
    else
	throw myexception()<<"Cannot convert string '"<<s<<"' to int!";
}

extern "C" closure builtin_function_read_double(OperationArgs& Args)
{
    string s = Args.evaluate(0).as_<String>();

    if (auto d = can_be_converted_to<double>(s))
	return {*d};
    else
	throw myexception()<<"Cannot convert string '"<<s<<"' to double!";
}

