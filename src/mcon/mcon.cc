#include "mcon/mcon.H"
#include <tuple>
#include <optional>
#include <sstream>
#include <variant>
#include <boost/convert.hpp>
#include <boost/convert/lexical_cast.hpp>
#include <boost/algorithm/string/split.hpp>
using std::string;
using std::string_view;
using std::set;
using std::map;
using std::vector;
using std::tuple;
using std::variant;

namespace fs = std::filesystem;

namespace MCON
{

void update(json::object& j1, const json::object& j2)
{
    for(auto& [key,value]: j2)
	j1[key] = value;
}

void update(json::object& j1, json::object&& j2)
{
    for(auto& [key,value]: j2)
	j1[key] = std::move(value);
}

vector<string> mcon_path(const string& name)
{
    vector<string> path;
    boost::split(path, name, [](char c) {return c == '/';});

    for(int i=0;i+1<path.size();i++)
	path[i].push_back('/');
    return path;
}

void drop_path(json::object& j, const vector<string>& path)
{
    json::object* jj = &j;
    for(int i=0;i+1<path.size();i++)
    {
	jj = & jj->at(path[i]).as_object();
    }
    jj->erase(path.back());
}

bool is_nested_key(string_view key)
{
    return key.ends_with('/');
}

set<string> get_keys_non_nested(const json::object& j)
{
    set<string> keys;
    for(auto& [key,value]: j)
	keys.insert(key);
    return keys;
}

set<string> get_keys_nested(const json::object& j)
{
    set<string> keys;
    for(auto& [key,value]: j)
    {
	if (is_nested_key(key))
	{
	    // This duplicates work, versus computing a prefix.
	    // Could be quadratic in length of string.
	    // Linear in depth of key.
	    // But simple.
	    for(auto& key2: get_keys_nested(value.as_object()))
		keys.insert(string(key)+string(key2));
	}
	else
	    keys.insert(key);
    }
    return keys;
}

json::object get_long_names_and_values_data(const std::string& prefix, const json::value& j)
{
    json::object j2;
    if (j.is_object())
    {
	for(auto& [key,value]: j.as_object())
	    update( j2, get_long_names_and_values_data(prefix+'['+string(key)+']', value) );
    }
    else if (j.is_array())
    {
	auto& array = j.as_array();
	for(int i=0; i< array.size(); i++)
	    update( j2, get_long_names_and_values_data(prefix+'['+std::to_string(i+1)+']', array[i]) );
    }
    else
	j2[prefix] = j;
    return j2;
}

json::object atomize(const json::object& j, bool nested)
{
    json::object j2;
    for(auto& [key,value]: j)
    {
	if (nested and is_nested_key(key))
	{
	    json::object j3;
	    auto j4 = atomize(value.as_object(), true);
	    for(auto& [key2, value2]: j4)
		j3[key2] = value2;
	    j2[key] = j3;
	}
	else
	    update( j2, get_long_names_and_values_data(key,value) );
    }
    return j2;
}

json::object unnest(const json::object& j, const string& prefix)
{
    json::object j2;
    for(auto& [key,value]: j)
    {
	if (is_nested_key(key))
	    update( j2, unnest(value.as_object(), prefix+string(key)) );
	else
	    j2[prefix+string(key)] = value;
    }
    return j2;
}

json::object nest(const json::object& j)
{
    json::object j2;

    map<string,json::object> subgroups;
    for(auto& [key,value]: j)
    {
	if (auto pos = key.find('/'); pos != string::npos)
	{
	    auto prefix = key.substr(0,pos+1);
	    auto rest = key.substr(pos+1);
	    subgroups[prefix][rest] = value;
	}
	else
	    j2[key] = value;
    }

    for(auto& [key,value]: subgroups)
    {
	j2[key] = nest(value);
    }

    return j2;
}

string chop_nested(const string& key)
{
    if (is_nested_key(key))
	return key.substr(0,key.size()-1);
    else
	return key;
}

json::object simplify(const json::object& j)
{
    // 1. Recursively simplify children.
    json::object j2;
    for(auto& [k,v]: j)
	j2[k] = is_nested_key(k) ? simplify(v.as_object()) : v;

    // 2. Check which keys are present or could be promoted.
    map<string,int> seen;
    for(auto& [key,value]: j2)
    {
	seen[chop_nested(key)] += 1;
	if (is_nested_key(key))
	    for(auto& [key2,value2]: value.as_object())
		seen[chop_nested(key2)] += 1;
    }

    // 3. Promote nested keys if there is no ambiguity.
    json::object j3;
    for(auto& [key,value]: j2)
    {
	if (is_nested_key(key))
	{
	    bool ok = true;
	    for(auto& [k2,v2]: value.as_object())
		if (seen.at(chop_nested(k2)) > 1)
		    ok = false;
	    if (ok)
		update(j3, value.as_object());
	    else
		j3[key] = value;
	}
	else
	    j3[key] = value;
    }

    return j3;
}

vector<string> short_fields(const vector<string>& fields)
{
    json::object j;
    for(int i=0;i<fields.size();i++)
	j[fields[i]] = i;

    j = unnest(simplify(nest(j)));

    vector<string> fields2(fields.size());
    for(auto& [key,index]: j)
	fields2[index.as_int64()] = string(key);

    return fields2;
}

json::kind collapse_kind(json::kind k)
{
    if (k == json::kind::int64 or k == json::kind::uint64)
	k = json::kind::double_;
    return k;
}

void findConstantFields(json::value& j1, const json::value& j2)
{
    if (j1.is_null())
	return;

    else if (collapse_kind(j1.kind()) != collapse_kind(j2.kind()))
    {
	j1.emplace_null();
	return;
    }
    
    else if (j1.is_object())
    {
	auto& o1 = j1.as_object();
	auto& o2 = j2.as_object();
	if (o1.size() != o2.size())
	{
	    j1.emplace_null();
	    return;
	}
	for(auto& [key,v1]: o1)
	{
	    if (not o2.count(key))
	    {
		j1.emplace_null();
		return;
	    }
	    findConstantFields(v1, o2.at(key));
	}
    }

    else if (j1.is_array())
    {
	auto& a1 = j1.as_array();
	auto& a2 = j2.as_array();
	if (a1.size() != a2.size())
	{
	    j1.emplace_null();
	    return;
	}
	for(int i=0;i<a1.size();i++)
	{
	    findConstantFields(a1[i], a2[i]);
	}
    }
}

tuple<set<string>, set<string>> classifyFields(const vector<json::object>& samples)
{
    if (samples.empty()) return {{},{}};

    auto fields = samples[0];
    for(auto& sample: samples)
    {
	json::object fields2;
	for(auto& [key, v1]: fields)
	{
	    if (v1.is_null()) continue;

	    if (not sample.count(key))
		v1.emplace_null();
	    else
		findConstantFields(v1, sample.at(key));
	}
    }

    // Maybe the top level, we should keep all the common fields, instead of dropping
    // everything if something changes?
    auto fields2 = atomize(unnest(fields), false);

    set<string> constant_fields;
    set<string> varying_fields;

    for(auto& [key,_]: fields2)
    {
	if (fields2.at(key).is_null())
	    varying_fields.insert(key);
	else
	    constant_fields.insert(key);
    }

    return {constant_fields, varying_fields};
}

std::ostream& Log::dump_MCON(std::ostream& o) const
{
    json::object header;
    if (fields)
    {
	json::array jfields;
	for(auto& field: *fields)
	    jfields.push_back(json::string(field));
	header["fields"] = jfields;
    }

    header["format"] = "MCON";
    header["version"] = "0.1";
    header["nested"] = is_nested();
    header["atomic"] = is_atomic();
    o<<header<<"\n";
    for(auto& sample: samples)
	o<<sample<<"\n";

    return o;
}

set<string> Log::get_keys() const
{
    assert(not samples.empty());
    if (is_nested())
	return get_keys_nested(samples[0]);
    else
	return get_keys_non_nested(samples[0]);
}

void Log::unnest()
{
    if (is_nested())
    {
	for(auto& sample: samples)
	    sample = MCON::unnest(sample);
	nested = false;
    }
}

void Log::atomize()
{
    if (not is_atomic())
    {
	for(auto& sample: samples)
	    sample = MCON::atomize(sample,true);
	atomic = true;
    }
}

void Log::drop(const string& name)
{
    auto path = mcon_path(name);
    for(auto& sample: samples)
	drop_path(sample, path);
}

std::ostream& write_tsv_line(std::ostream& o, const vector<string>& v)
{
    for(int i=0;i<v.size();i++)
    {
	o<<v[i];
	if (i+1<v.size())
	    o<<'\t';
    }
    return o;
}

std::string tsv_line(const vector<string>& v)
{
    std::ostringstream o;
    write_tsv_line(o,v);
    return o.str();
}

vector<string> parse_tsv_line(const string& s)
{
    vector<string> strings;
    int length=0;
    for(int i=0;i<s.size();i++)
	if (s[i] == '\t') {
	    strings.push_back(s.substr(i-length,length));
	    length = 0;
	}
	else
	    length++;
  
    strings.push_back(s.substr(s.size()-length,length));
    return strings;
}

vector<string> get_row(const map<string,int>& all_fields, const json::object& sample, std::optional<int> sample_index)
{
    vector<string> row(all_fields.size());
    for(auto& [field,index]: all_fields)
    {
	if (not sample.count(field))
	{
	    std::cerr<<"Error: sample";
	    if (sample_index)
		std::cerr<<" line "<<*sample_index;
	    std::cerr<<" is missing field '"<<field<<"'"<<std::endl;
	    std::cerr<<"  "<<sample<<"\n";
	    exit(1);
	}
	row[index] = MCON::serialize(sample.at(field));
    }
    assert(sample.size() >= all_fields.size());
    return row;
}

vector<json::value> get_json_row(const map<string,int>& all_fields, const json::object& sample, std::optional<int> sample_index)
{
    vector<json::value> row(all_fields.size());
    for(auto& [field,index]: all_fields)
    {
	if (not sample.count(field))
	{
	    std::cerr<<"Error: sample";
	    if (sample_index)
		std::cerr<<" line "<<*sample_index;
	    std::cerr<<" is missing field '"<<field<<"'"<<std::endl;
	    std::cerr<<"  "<<sample<<"\n";
	    exit(1);
	}
	row[index] = sample.at(field);
    }
    assert(sample.size() >= all_fields.size());
    return row;
}

auto sort_key(const string& name)
{
    vector<variant<int,string>> keylist;

    vector<string> path;
    boost::split(path, name, [](char c) {return c == '[';});

    auto  cnv = boost::cnv::lexical_cast();        // boost::lexical_cast-based converter

    for(int i=0;i<path.size();i++)
    {
	auto& key = path[i];

	if (i==0)
	    keylist.push_back(key);
	else if (key.ends_with(']'))
	{
	    key.resize(key.size()-1);
	    if (auto i = boost::convert<int>(key, cnv))
		keylist.push_back(*i);
	    else
		keylist.push_back(key);
	}
	else
	    keylist.push_back(key);
    }

    return keylist;
}


// 1. Put the specified fields in front.
// 2. Sort the remaining fields and append them.
// 3. Only consider constant fields.
vector<string> sort_fields(const vector<string>& specified_fields, const set<string>& constantFields)
{
    set<string> specified_fields_set;
    for(auto& field: specified_fields)
	specified_fields_set.insert(field);

    for(auto& specified_field: specified_fields_set)
    {
	if (not constantFields.count(specified_field))
	{
	    std::cerr<<"Error: Header: field '"<<specified_field<<"' is missing or does not have a fixed structure!\n";
	    exit(1);
	}
    }

    vector<string> fields2;
    for(auto& field: constantFields)
	if (not specified_fields_set.contains(field))
	    fields2.push_back(field);
    std::sort(fields2.begin(), fields2.end(), [](auto& s1, auto& s2) {return sort_key(s1) < sort_key(s2);});

    // construct specified_fields + fields2
    vector<string> out_fields = specified_fields;

    out_fields.insert(out_fields.end(), fields2.begin(), fields2.end());
    assert(out_fields.size() == constantFields.size());

    return out_fields;
}

std::vector<std::string> tsv_fields(const std::vector<std::string>& first_fields, const json::object& j, bool nested)
{
    auto all_fields = nested?get_keys_nested(atomize(j,true)):get_keys_non_nested(atomize(j,false));

    return sort_fields(first_fields, all_fields);
}


vector<tuple<string,vector<json::value>>> Log::dump_json_table(std::optional<bool> short_names) const
{
    if (not short_names)
        short_names = true;

    // This seems a bit wasteful if we are already non-nested and atomic.
    // However... does non-nested and atomic mean that the top-level fields are fixed?
    auto [constantFields, varyingFields] = classifyFields(samples);

    if (not varyingFields.empty())
    {
        std::cerr<<"varying fields =";
        for(auto& field: varyingFields)
            std::cerr<<" "<<field;
        std::cerr<<"\n";
    }

    const int n_fields = constantFields.size();

    vector<string> first_fields;
    if (fields)
        first_fields = *fields;

    // PROBLEM: first_fields needs to specify the long name of the fields
    vector<string> out_fields = sort_fields(first_fields, constantFields);

    map<string,int> all_fields_map;
    for(int i=0;i<n_fields;i++)
        all_fields_map.insert({out_fields[i],i});

    // Log: Writing TSV: nfields fields
    auto printed_fields = out_fields;
    if (short_names and *short_names)
        printed_fields = short_fields(printed_fields);

    // How about quoting?
    vector<tuple<string,vector<json::value>>> json_table;
    for(int i=0;i<n_fields;i++)
        json_table.push_back({printed_fields[i], {}});

    for(int sample_index=0;sample_index<samples.size(); sample_index++)
    {
        auto sample = MCON::atomize(MCON::unnest(samples[sample_index]), false);

        auto row = get_json_row(all_fields_map, sample, sample_index);

        assert(row.size() == json_table.size());

        for(int i=0;i<n_fields;i++)
            std::get<1>(json_table[i]).push_back( row[i] );
    }

    return json_table;
}

vector<tuple<string, vector<double>>> Log::dump_numeric_table(bool translate_bool, std::optional<bool> short_names) const
{
    auto json_table = dump_json_table(short_names);

    vector<tuple<string, vector<double>>> numeric_table;
    for(auto& [field,values]: json_table)
    {
        if (values[0].is_number())
        {
            vector<double> numeric_values;
            for(auto& value: values)
                numeric_values.push_back( value.to_number<double>() );
            numeric_table.push_back({field, std::move(numeric_values) });
        }

        if (values[0].is_bool() and translate_bool)
        {
            vector<double> numeric_values;
            for(auto& value: values)
                numeric_values.push_back( value.as_bool() );
            numeric_table.push_back({field, std::move(numeric_values) });
        }
    }

    return numeric_table;
}

// TODO: Extract generic infrastructure to get out in-memory tables.
std::ostream& Log::dump_TSV(std::ostream& o, std::optional<bool> short_names) const
{
    auto json_table = dump_json_table(short_names);
    const int n_fields = json_table.size();

    vector<string> fields;
    for(auto& [field,_]: json_table)
        fields.push_back(field);

    // How about quoting?
    write_tsv_line(o, fields)<<"\n";

    for(int sample_index=0;sample_index<samples.size(); sample_index++)
    {
        vector<string> row(n_fields);
        for(int i=0;i<n_fields;i++)
            row[i] = MCON::serialize(std::get<1>(json_table[i])[sample_index]);

        write_tsv_line(o, row)<<"\n";
    }

    return o;
}

Log::Log(const json::object& header)
{
    if (header.count("fields"))
    {
	vector<string> fs;
	for(auto& field: header.at("fields").as_array())
	    fs.push_back(string(field.as_string()));
	fields = std::move(fs);
    }

    if (header.count("nested"))
	nested = header.at("nested").as_bool();

    if (header.count("atomic"))
	atomic = header.at("atomic").as_bool();
}

Log::Log(const json::object& header, const std::vector<json::object>& js)
    :Log(header)
{
    samples = js;
}

Log::Log(const json::object& header, std::vector<json::object>&& js)
    :Log(header)
{
    samples = std::move(js);
}

std::tuple<std::optional<string>,string> detect_format(std::istream& instream)
{
    string firstline;
    getline(instream, firstline);

    try
    {
	auto header = parse(firstline);
	if (header.count("format") and header.at("format") == "MCON" and header.count("version"))
	    return {"MCON",firstline};
	else
	    return {std::optional<string>(),firstline};
    }
    catch (...)
    {
	return {"TSV",firstline};
    }
}


MCON::Log read_MCON(const string& firstline, std::istream& input)
{
    json::object header = parse(firstline);
    vector<json::object> samples;

    string line;
    while(getline(input,line))
	samples.push_back( parse(line) );
    return MCON::Log(header, std::move(samples));
}

json::array convert_to_json(const vector<string>& v)
{
    json::array array;
    for(auto& s: v)
	array.push_back(json::string(s));
    return array;
}

MCON::Log read_TSV(const string& firstline, std::istream& input)
{
    auto tsv_fields = parse_tsv_line(firstline);
    int nfields = tsv_fields.size();

    json::object header = { {"format","MCON"},
			    {"version", "0.1"},
			    {"fields",  convert_to_json(tsv_fields)},
			    {"nested", false} };

    string line;
    vector<json::object> samples;
    int ignored_lines = 0;
    while(getline(input, line))
    {
	auto fields = parse_tsv_line(line);
	if (fields.size() != nfields)
	{
	    std::cerr<<"TSV line "<<1+samples.size()<<" has "<<fields.size()<<" fields, but header has "<<nfields<<" fields.\n";
	    ignored_lines++;
	    break;
	}
	json::object j;
	for(int i=0;i<nfields;i++)
	    j[tsv_fields[i]] = parse(fields[i]);
	samples.push_back(j);
    }
    while(getline(input, line))
	ignored_lines++;

    if (ignored_lines > 0)
    {
	std::cerr<<"Skipping the next "<<ignored_lines<<" lines.\n";
    }
    std::cerr<<"Read "<<samples.size()<<" samples of TSV with "<<nfields<<" fields.\n";
    return MCON::Log(header, samples);
}

MCON::Log read_logfile(std::istream& input, const fs::path& filename)
{
    auto [format,firstline] = detect_format(input);
    if (format == "MCON")
	return read_MCON(firstline, input);
    else if (format == "TSV")
	return read_TSV(firstline, input);
    else if (not format)
	std::cerr<<"Unknown format for file "<<filename;
    else
	std::cerr<<"Unknown format '"<<*format<<"'for file "<<filename;
    exit(1);
}

json::object parse(const std::string& line)
{
    return json::parse(line, {}, {.allow_infinity_and_nan = true}).as_object();
}


std::string serialize(const json::value& j)
{
    return json::serialize(j, {.allow_infinity_and_nan=true});
}

}
