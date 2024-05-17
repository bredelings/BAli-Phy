#include "mcon/mcon.H"
#include <tuple>
#include <optional>
#include <sstream>
#include <boost/algorithm/string/split.hpp>
using std::string;
using std::string_view;
using std::set;
using std::map;
using std::vector;

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
    else if (auto array = j.if_array())
    {
	for(int i=0; i< array->size(); i++)
	    update( j2, get_long_names_and_values_data(prefix+'['+std::to_string(i+1)+']', (array)[i]) );
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
    int nfields = all_fields.size();

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
	row[index] = json::serialize(sample.at(field),{.allow_infinity_and_nan=true});
    }
    assert(sample.size() >= nfields);
    if (sample.size() != nfields)
    {
	for(auto& [key,value]: sample)
	    if (not all_fields.count(key))
	    {
		std::cerr<<"Error: sample";
		if (sample_index)
		    std::cerr<<" line "<<*sample_index;
		std::cerr<<" has extra field '"<<key<<"'"<<std::endl;
		std::cerr<<"  "<<sample<<"\n";
		exit(1);
	    }
    }
    assert(sample.size() == nfields);
    return row;
}

vector<string> tsv_fields(const vector<string>& first_fields, const json::object& j, bool nested)
{
    vector<string> out_fields = first_fields;

    auto all_fields = nested?get_keys_nested(atomize(j,true)):get_keys_non_nested(atomize(j,false));

    set<string> fields1;
    for(auto& field: out_fields)
	fields1.insert(field);

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

    return out_fields;
}
    
std::ostream& Log::dump_TSV(std::ostream& o, std::optional<bool> short_names) const
{
    if (is_nested() or not is_atomic())
    {
	auto log2 = *this;
	log2.unnest();
	log2.atomize();
	if (not short_names)
	    short_names = true;
	return log2.dump_TSV(o, short_names);
    }

    assert(not is_nested());
    assert(is_atomic());

    vector<string> first_fields;
    if (fields)
	first_fields = *fields;

    vector<string> out_fields = tsv_fields(first_fields, samples[0], nested);

    map<string,int> all_fields_map;
    for(int i=0;i<out_fields.size();i++)
	all_fields_map.insert({out_fields[i],i});
    
    // Log: Writing TSV: nfields fields

    auto printed_fields = out_fields;
    if (short_names and *short_names)
	printed_fields = short_fields(printed_fields);

    write_tsv_line(o, printed_fields)<<"\n";

    for(int sample_index=0;sample_index<samples.size(); sample_index++)
    {
	auto& sample = samples[sample_index];

	write_tsv_line(o, get_row(all_fields_map, sample, sample_index))<<"\n";
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
	auto header = json::parse(firstline, {}, {.allow_infinity_and_nan = true}).as_object();
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
    json::object header = json::parse(firstline, {}, {.allow_infinity_and_nan = true}).as_object();
    vector<json::object> samples;

    string line;
    while(getline(input,line))
	samples.push_back(json::parse(line, {}, {.allow_infinity_and_nan = true}).as_object());
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
	    j[tsv_fields[i]] = json::parse(fields[i], {}, {.allow_infinity_and_nan = true}).as_object();
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

}
