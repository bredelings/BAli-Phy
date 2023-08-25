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
#include "mcon/mcon.H"

using std::string;
using std::string_view;
using std::vector;
using std::set;
using std::map;


json c_json(const expression_ref& E)
{
    int type = E.as_<EPair>().first.as_int();

    auto& J = E.as_<EPair>().second;
    
    // Null
    if (type == 6)
        return {};
    // String
    else if (type == 5)
        return (string)J.as_<String>();
    // Bool
    else if (type == 4)
    {
        if (is_bool_true(J))
            return true;
        else if (is_bool_false(J))
            return false;
        else
            throw myexception()<<"Foreign:cjson: I don't understand bool '"<<J<<"'";
    }
    // Integer
    else if (type == 2)
    {
        return J.as_int();
    }
    // Double
    else if (type == 3)
    {
        // I think we'd like to serialize Inf as 1e999999
        // and -Inf as -1e999999. But here we return JSON,
        // not a string, so that doesn't work.
        return J.as_double();
    }
    // Object
    else if (type == 1)
    {
	json object = json::object();
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
	json array = json::array();
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

extern "C" closure builtin_function_ejson_inumber(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_int();
    return { EPair(2, j) };
}

extern "C" closure builtin_function_ejson_fnumber(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_double();
    return { EPair(3, j) };
}

extern "C" closure builtin_function_ejson_bool(OperationArgs& Args)
{
    auto j = Args.evaluate(0);
    return { EPair(4, j) };
}

extern "C" closure builtin_function_ejson_string(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_<String>();
    return { EPair(5, j) };
}

extern "C" closure builtin_function_ejson_null(OperationArgs& Args)
{
    auto j = Args.evaluate(0);
    return { EPair(6, 0) };
}

extern "C" closure builtin_function_cjson_to_bytestring(OperationArgs& Args)
{
    auto j = Args.evaluate(0).as_<Box<json>>();
    String s = j.dump();
    return s;
}

extern "C" closure builtin_function_tsvHeaderAndMapping(OperationArgs& Args)
{
    auto arg0 =  Args.evaluate(0);
    vector<string> firstFields;
    for(auto& e: arg0.as_<EVector>())
	firstFields.push_back(e.as_<String>());

    auto arg1 =  Args.evaluate(1);
    auto& sample = arg1.as_<Box<json>>();

    vector<string> out_fields = firstFields;

    set<string> fields1;
    for(auto& field: out_fields)
	fields1.insert(field);

    auto all_fields = MCON::get_keys_nested(MCON::atomize(sample,true));

    for(auto& field: fields1)
	if (not all_fields.count(field))
	{
	    std::cerr<<"Error: Header: field '"<<field<<"' does not exist!\n";
	    exit(1);
	}

    vector<string> fields2;
    for(auto& field: all_fields)
	if (not fields1.contains(field))
	    fields2.push_back(field);
    std::sort(fields2.begin(), fields2.end());
    out_fields.insert(out_fields.end(), fields2.begin(), fields2.end());
    assert(out_fields.size() == all_fields.size());

    auto printed_fields = MCON::short_fields(out_fields);

    expression_ref tsv_header = new String(MCON::tsv_line(printed_fields));;

    object_ptr<Box<map<string,int>>> mapping = new Box<map<string,int>>();
    for(int i=0;i<out_fields.size();i++)
	mapping->insert({out_fields[i],i});

    expression_ref m2 = mapping;

    return { EPair(tsv_header, m2) };
}

extern "C" closure builtin_function_getTsvLine(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& mapping = arg0.as_<Box<std::map<string,int>>>();

    auto arg1 = Args.evaluate(1);
    auto& sample = arg1.as_<Box<json>>();

    auto sample2 = MCON::atomize(MCON::unnest(sample), true);

    object_ptr<String> result = new String(MCON::tsv_line(MCON::get_row(mapping, sample2)));
    return result;
}
