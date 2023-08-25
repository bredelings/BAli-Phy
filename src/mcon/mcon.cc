#include "mcon/mcon.H"
#include <tuple>
#include <optional>
using std::string;
using std::string_view;
using std::set;
using std::map;
using std::vector;

namespace MCON
{

bool is_nested_key(string_view key)
{
    return key.ends_with('/');
}

set<string> get_keys_non_nested(const json& j)
{
    set<string> keys;
    for(auto& [key,value]: j.items())
	keys.insert(key);
    return keys;
}

set<string> get_keys_nested(const json& j)
{
    set<string> keys;
    for(auto& [key,value]: j.items())
    {
	if (is_nested_key(key))
	{
	    // This duplicates work, versus computing a prefix.
	    // Could be quadratic in length of string.
	    // Linear in depth of key.
	    // But simple.
	    for(auto& key2: get_keys_nested(value))
		keys.insert(key+key2);
	}
	else
	    keys.insert(key);
    }
    return keys;
}

json get_long_names_and_values_data(const std::string& prefix, const json& j)
{
    json j2;
    if (j.is_object())
    {
	for(auto& [key,value]: j.items())
	    j2.update( get_long_names_and_values_data(prefix+'['+key+']', value) );
    }
    else if (j.is_array())
    {
	for(int i=0; i<j.size(); i++)
	    j2.update( get_long_names_and_values_data(prefix+'['+std::to_string(i+1)+']', j[i]) );
    }
    else
	j2[prefix] = j;
    return j2;
}

json atomize(const json& j, bool nested)
{
    json j2;
    for(auto& [key,value]: j.items())
    {
	if (nested and is_nested_key(key))
	{
	    json j3;
	    for(auto& [key2, value2]: atomize(value, true).items())
		j3[key2] = value2;
	    j2[key] = j3;
	}
	else
	    j2.update( get_long_names_and_values_data(key,value) );
    }
    return j2;
}

json unnest(const json& j, const string& prefix)
{
    json j2;
    for(auto& [key,value]: j.items())
    {
	if (is_nested_key(key))
	    j2.update( unnest(value, prefix+key) );
	else
	    j2[prefix+key] = value;
    }
    return j2;
}

json nest(const json& j)
{
    json j2;

    map<string_view,json> subgroups;
    for(auto& [key,value]: j.items())
    {
	if (auto pos = key.find('/'); pos != string::npos)
	{
	    auto prefix = string_view(key).substr(0,pos+1);
	    auto rest = string_view(key).substr(pos+1);
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

string chop_nested(string key)
{
    if (is_nested_key(key))
	return key.substr(0,key.size()-1);
    else
	return key;
}

json simplify(const json& j)
{
    json j2;
    map<string,int> seen;
    for(auto& [key,value]: j.items())
    {
	seen[chop_nested(key)] += 1;
	if (is_nested_key(key))
	    for(auto& [key2,value2]: value.items())
		seen[chop_nested(key2)] += 1;
    }

    for(auto& [key,value]: j.items())
    {
	if (is_nested_key(key))
	{
	    bool ok = true;
	    for(auto& [k2,v2]: value.items())
		if (seen.at(chop_nested(k2)) > 1)
		    ok = false;
	    if (ok)
		j2.update(value);
	    else
		j2[key] = value;
	}
	else
	    j2[key] = value;
    }

    return j2;
}

vector<string> short_fields(const vector<string>& fields)
{
    json j;
    for(int i=0;i<fields.size();i++)
	j[fields[i]] = i;

    j = unnest(simplify(nest(j)));

    vector<string> fields2(fields.size());
    for(auto& [key,index]: j.items())
	fields2[index] = key;

    return fields2;
}
    

std::ostream& Log::dump_MCON(std::ostream& o) const
{
    json header;
    if (fields)
	header["fields"] = *fields;

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

std::ostream& write_tsv_line(std::ostream& o, const vector<string>& v)
{
    for(int i=0;i<v.size();i++)
    {
	o<<v[i];
	if (i+1<v.size())
	    o<<'\t';
	else
	    o<<'\n';
    }
    return o;
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

    vector<string> out_fields;
    if (fields)
	out_fields = *fields;

    set<string> fields1;
    for(auto& field: out_fields)
	fields1.insert(field);

    auto all_fields = get_keys();
    int nfields = all_fields.size();

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

    // Log: Writing TSV: nfields fields

    auto printed_fields = out_fields;
    if (short_names and *short_names)
	printed_fields = short_fields(printed_fields);

    write_tsv_line(o, printed_fields);

    for(int sample_index=0;sample_index<samples.size(); sample_index++)
    {
	auto& sample = samples[sample_index];

	vector<string> row;
	for(int i=0;i<nfields;i++)
	{
	    if (not sample.count(out_fields[i]))
	    {
		std::cerr<<"Error: sample line "<<sample_index<<" is missing field '"<<out_fields[i]<<"'"<<std::endl;
		std::cerr<<"  "<<sample<<"\n";
		exit(1);
	    }
	    row.push_back(sample.at(out_fields[i]).dump());
	}
	assert(sample.size() >= nfields);
	if (sample.size() != nfields)
	{
	    for(auto& [key,value]: sample.items())
		if (not all_fields.count(key))
		{
		    std::cerr<<"Error: sample line "<<sample_index<<" has extra field '"<<key<<"'"<<std::endl;
		    std::cerr<<"  "<<sample<<"\n";
		    exit(1);
		}
	}
	assert(sample.size() == nfields);
	write_tsv_line(o, row);
    }

    return o;
}

Log::Log(const json& header)
{
    if (header.count("fields"))
	fields = header.at("fields");

    if (header.count("nested"))
	nested = header.at("nested");

    if (header.count("atomic"))
	atomic = header.at("atomic");
}

Log::Log(const json& header, const std::vector<json>& js)
    :Log(header)
{
    samples = js;
}

Log::Log(const json& header, std::vector<json>&& js)
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
	auto header = json::parse(firstline);
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
    json header = json::parse(firstline);
    vector<json> samples;

    string line;
    while(getline(input,line))
	samples.push_back(json::parse(line));
    return MCON::Log(header, std::move(samples));
}

MCON::Log read_TSV(const string& firstline, std::istream& input)
{
    auto tsv_fields = parse_tsv_line(firstline);
    int nfields = tsv_fields.size();

    json header = { {"format","MCON"},
		    {"version", "0.1"},
		    {"fields",  tsv_fields},
		    {"nested", false} };
    
    string line;
    vector<json> samples;
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
	json j;
	for(int i=0;i<nfields;i++)
	    j[tsv_fields[i]] = json::parse(fields[i]);
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

MCON::Log read_logfile(std::istream& input, const string& filename)
{
    auto [format,firstline] = detect_format(input);
    if (format == "MCON")
	return read_MCON(firstline, input);
    else if (format == "TSV")
	return read_TSV(firstline, input);
    else if (not format)
	std::cerr<<"Unknown format for file '"<<filename<<"'";
    else
	std::cerr<<"Unknown format '"<<*format<<"'for file '"<<filename<<"'";
    exit(1);
}

}
