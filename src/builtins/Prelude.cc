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

    // 1. If the number of arguments isn't equal, then x /= y
    if (x.size() != y.size()) return {expression_ref(False)};

    // 2. If the heads aren't equal, then x /= y.
    if (x.head() != y.head()) return {expression_ref(False)};

    // 3. If x and y have no arguments, then x == y
    if (x.size() == 0) return {expression_ref(True)};

    // 4. Otherwise we dont know.
    return {expression_ref(DontKnow)};
}

enum Ordering {LT=4,GT=5,EQ=6,DontKnow=7};

template <typename T>
Ordering compare(const T& x, const T& y)
{
    if (x < y) return Ordering::LT;
    if (x > y) return Ordering::GT;
    return Ordering::EQ;
}

extern "C" closure builtin_function_compare_top(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);

    // 1. Compare number of arguments
    auto cmp = compare( x.size(), y.size() );
    if (cmp != Ordering::EQ)
        return expression_ref(int(cmp));

    // 2. Compare the heads
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
    else if (x.head().is_a<constructor>())
    {
        auto xc = x.head().as_<constructor>();
        auto yc = y.head().as_<constructor>();

        assert(x.size() == xc.n_args());
        assert(y.size() == yc.n_args());

        cmp = compare<string>(xc.f_name, xc.f_name);
    }
    else
        throw myexception()<<"Prelude:compare_top: `"<<x<<"` and `"<<y<<"` can't be compared!";

    if (cmp != Ordering::EQ)
        return expression_ref(int(cmp));

    // 3. If x and y have no arguments, then x == y
    if (x.size() == 0) return {expression_ref(int(Ordering::EQ))};

    // 4. Otherwise we don't know
    return {expression_ref(int(Ordering::DontKnow))};
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

extern "C" closure builtin_function_struct_seq(OperationArgs& Args)
{
    int r0 = Args.reg_for_slot(0);
    auto c = Args.evaluate_reg_to_closure_(r0);

    auto& M = Args.memory();
    r0 = M.follow_index_var_no_force(r0);
    if (M.reg_is_to_changeable(r0))
	throw myexception()<<"struct_seq: structure must be constant at reg "<<r0<<"!";

    for(int i=0;i<c.exp.size();i++)
        Args.evaluate_reg_force(c.reg_for_slot(i));

    int r1 = Args.current_closure().reg_for_slot(1);

    return {index_var(0),{r1}};
}

