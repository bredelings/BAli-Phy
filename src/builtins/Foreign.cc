#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/bool.H"
#include "computation/expression/expression_ref.H"
#include "util/myexception.H"
#include <algorithm>
#include "util/json.hh"
#include <string_view>
#include "computation/machine/graph_register.H"

using std::string;
using std::string_view;



json c_json(const expression_ref& E)
{
    int type = E.as_<EPair>().first.as_int();

    auto& J = E.as_<EPair>().second;
    
    // Null
    if (type == 5)
        return {};
    // String
    else if (type == 4)
        return J.as_<String>();
    // Bool
    else if (type == 3)
    {
        if (is_bool_true(J))
            return true;
        else if (is_bool_false(J))
            return false;
        else
            throw myexception()<<"Foreign:cjson: I don't understand bool '"<<J<<"'";
    }
    // Number
    else if (type == 2)
    {
	if (J.is_double())
	    return J.as_double();
	else if (J.is_int())
	    return J.as_int();
	else if (J.is_log_double())
	    return (double)J.as_log_double();
        else
            throw myexception()<<"Foreign:cjson: I don't understand number '"<<J<<"'";
    }
    // Object
    else if (type == 1)
    {
	json object;
	for(auto& j: J.as_<EVector>())
	{
            auto& K = j.as_<EPair>().first;
            auto& V = j.as_<EPair>().second;
	    const string& key  = K.as_<String>();
	    object[key] = c_json(V);
	}
	return object;
    }
    else if (type == 0)
    {
	json array;
	for(auto& j: J.as_<EVector>())
	    array.push_back(c_json(j));
	return array;
    }
    else
        throw myexception()<<"Foreign:c_json: Can't translate "<<E<<" into JSON!";
}


extern "C" closure builtin_function_c_json(OperationArgs& Args)
{
    auto j = Args.evaluate(0);

    Box<json> J = c_json(j);

    return (const Object&)J;
}

extern "C" closure builtin_function_ejson_array(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_<EVector>();
    return { EPair(0, j) };
}

extern "C" closure builtin_function_ejson_object(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_<EVector>();
    return { EPair(1, j) };
}

extern "C" closure builtin_function_ejson_number(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_double();
    return { EPair(2, j) };
}

extern "C" closure builtin_function_ejson_bool(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_double();
    return { EPair(3, j) };
}

extern "C" closure builtin_function_ejson_string(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_<String>();
    return { EPair(4, j) };
}

extern "C" closure builtin_function_ejson_null(OperationArgs& Args)
{
    auto j = Args.evaluate(0);
    return { EPair(5, 0) };
}
