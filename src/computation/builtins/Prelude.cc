#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"
#include "computation/operations.H"
#include "vector_from_list.H"
#include "util/string/convert.H"

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

extern "C" closure builtin_function_vector_from_list(OperationArgs& Args)
{
    object_ptr<EVector> v (new EVector);

    const closure* top = &Args.evaluate_slot_to_closure(0);
    while(top->exp.size())
    {
	assert(has_constructor(top->exp,":"));
	assert(top->exp.size() == 2);

	int element_reg = top->reg_for_slot(0);

	int next_reg = top->reg_for_slot(1);

	// Add the element to the list.
	v->push_back( Args.evaluate_reg_to_object(element_reg) );
	// Move to the next element or end
	top = &Args.evaluate_reg_to_closure(next_reg);
    }
    assert(has_constructor(top->exp,"[]"));

    return v;
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

extern "C" closure builtin_function_equals(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() == y.as_double()};
    else if (x.is_int())
	return {x.as_int() == y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() == y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() == y.as_char()};
    else
	throw myexception()<<"==: object '"<<x.print()<<"' is not double, int, log_double, or char'";
}

extern "C" closure builtin_function_notequals(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);
  
    if (x.is_double())
	return {x.as_double() != y.as_double()};
    else if (x.is_int())
	return {x.as_int() != y.as_int()};
    else if (x.is_log_double())
	return {x.as_log_double() != y.as_log_double()};
    else if (x.is_char())
	return {x.as_char() != y.as_char()};
    else
	throw myexception()<<"/=: object '"<<x.print()<<"' is not double, int, log_double, or char'";
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

#include "iota.H"

extern "C" closure builtin_function_iotaUnsigned(OperationArgs& Args)
{
    return iota_function<unsigned>(Args);
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
    auto v = get_vector_from_list(Args,0);

    string message;
    for(auto ch: v)
	message += ch.as_char();
  
    throw error_exception(message);
}

extern "C" closure builtin_function_putStrLn(OperationArgs& Args)
{
    string message = Args.evaluate(0).as_<String>();

    std::cout<<message<<std::endl;

    return constructor("()",0);
}

extern "C" closure builtin_function_reapply(OperationArgs& Args)
{
    int R1 = Args.current_closure().reg_for_slot(0);

    int R2 = Args.current_closure().reg_for_slot(1);

    expression_ref apply_E;
    {
	expression_ref fE = index_var(1);
	expression_ref argE = index_var(0);
	apply_E = {fE, argE};
    }

    // %1 %0 {R1,R2}
    int apply_reg = Args.allocate({apply_E,{R1, R2}});

    // FIXME - aren't we trying to eliminate general evaluation of regs that aren't children?  See below:

    // Evaluate the newly create application reg - and depend upon it!
    if (Args.evaluate_changeables())
	Args.evaluate_reg_to_object(apply_reg);

    return {index_var(0),{apply_reg}};
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

