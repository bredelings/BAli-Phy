#include <set>

#include "loggers.H"
#include "tools/parsimony.H"
#include "tools/stats-table.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include "computation/expression/var.H"
#include "mcmc/mcmc.H"
#include "mcmc/logger.H"
#include "substitution/parsimony.H"
#include "models/parse.H"
#include "util/mapping.H"
#include "util/string/join.H"
#include "util/string/split.H"
#include "util/json.hh"

namespace fs = std::filesystem;

extern int log_verbose;

using std::vector;
using std::map;
using std::string;
using std::set;
using std::shared_ptr;
using std::make_shared;

using std::to_string;

using std::optional;

map<string,set<string>> extract_signature(const map<string,map<string,int>>& extensions)
{
    map<string, set<string>> prefix_to_labels;
    for(auto& extension: extensions)
	for(auto& label: extension.second)
	    prefix_to_labels[extension.first].insert(label.first);
    return prefix_to_labels;
}

set<string> extract_signature(const map<string,int>& extensions)
{
    set<string> labels;
    for(auto& [label,_]: extensions)
	labels.insert(label);
    return labels;
}


// {S1 -> {[0] -> [a,b],[1]->[c,d]}, S2 -> {[0] -> [x,y],[1] -> [z,w]} }
vector<vector<vector<int>>> get_un_identifiable_indices(const vector<string>& names, const vector<string>& patterns)
{
    // groups[prefix][label] = {indices} ; and the number of indices should be patterns.size
    map<string,map<string,vector<int>>> groups;
    std::optional<map<string,set<string>>> signature;
    for(auto& pattern : patterns)
    {
	auto z = parameters_with_extension(names, pattern);
	if (not signature)
	    signature = extract_signature(z);
	else
	    if (*signature != extract_signature(z))
		throw myexception()<<"Signature's don't match!";  // FIXME - how to display this?

	// groups[prefix][label] = index
	for(auto& [prefix,label_mapping]: z)
	{
	    auto& group = groups[prefix];
	    for(auto& [label, index]: label_mapping)
	    {
		group[label].push_back(index);
	    }
	}
    }

    // 2. Create the indices, where each vector<vector<int>> contains patterns.size() components, and the first one is the one we sort on.
    vector< vector< vector<int> > > indices;
    for(auto& prefix_ : groups)
    {
	vector<vector<int> > group_indices;
	// Extract the vector<int> for each pattern.
	for(int i=0;i<patterns.size();i++)
	{
	    vector<int> indices;
	    for(auto& label_: prefix_.second)
	    {
		if (i< label_.second.size())
		    indices.push_back(label_.second[i]);
		else
		    throw myexception()<<"label group doesn't have "<<i+1<<" components!";
	    }
	    group_indices.push_back(std::move(indices));
	}
	indices.push_back(std::move(group_indices));
    }

    return indices;
}

/// Determine the parameters of model \a M that must be sorted in order to enforce identifiability.
vector< vector< vector<int> > > get_un_identifiable_indices(const vector<string>& names)
{
    vector< vector< vector<int> > > indices;

    for(auto& patterns: vector<vector<string>>{ {"Rates.free:rates*","Rates.free:frequencies*"}, {"m3:omegas*","m3:ps*"} } )
    {
	auto x = get_un_identifiable_indices(names, patterns);
	indices.insert(indices.end(), x.begin(), x.end());
    }

    return indices;
}

template <typename K>
void add_value(json& j, const K& key, const json& value)
{
    j[key] = value;
}

template <typename K>
void add_children(json& j, const K& key, const json& value)
{
    j[string(key)+'/'] = value;
}

json logged_params_and_some_computed_stuff(const Model& M, const json& jlog, long t)
{
    using namespace MCMC;

    json j;

    add_value(j, "iter", t);
    add_value(j, "prior", log(M.prior()) );
    add_value(j, "likelihood", log(M.likelihood()));
    add_value(j, "posterior", log(M.probability()));

    add_children(j, "parameters", jlog);

    return j;
}

string logged_params_and_some_computed_stuff_with_header(const Model& M, const json& jlog, long t)
{
    json j = logged_params_and_some_computed_stuff(M,jlog,t);

    string line = j.dump()+"\n";

    if (t == 0)
    {
        json header;
        header["version"] = "0.1";
        header["fields"] = {"iter","prior","likelihood","posterior"};
        header["nested"] = true;
        line = header.dump()+"\n"+line;
    }
    return line;
}


owned_ptr<MCMC::TableFunction<string>> construct_table_function(owned_ptr<Model>& M)
{
    using namespace MCMC;
    owned_ptr<TableGroupFunction<string> > TL = claim(new TableGroupFunction<string>);
  
    TL->add_field("iter", [](const Model&, const json&, long t) {return convertToString(t);});
    TL->add_field("prior", [](const Model& M, const json&, long) {return convertToString(log(M.prior()));});
    TL->add_field("likelihood", [](const Model& M, const json&, long) {return convertToString(log(M.likelihood()));}); TL->add_field("posterior", [](const Model& M, const json&, long) {return convertToString(log(M.probability()));});
  
    {
	json log = M->get_logged_parameters();
	vector<string> names = parameter_names(log);
	names = short_parameter_names(names);

	json_to_table_function T1(names);

	SortedTableFunction T2(T1, get_un_identifiable_indices(names));

	TL->add_fields( ConvertTableToStringFunction<json>( T2 ) );
    }

    return TL;
}


string table_logger_line(MCMC::TableFunction<string>& TF, const Model& M, const json& jlog, long t)
{
    std::ostringstream o;
    auto values = TF(M,jlog,t);
    auto names = TF.field_names();

    for(int i=0;i<TF.n_fields();i++) {
	string name = names[i];
	if (log_verbose <= 0 and name.size() > 1 and name[0] == '*') continue;
	o<<"    "<<name<<" = "<<values[i];
    }
    return o.str();
}

set<string> get_log_formats(const boost::program_options::variables_map& args, bool is_A_T_model)
{
    string log_format = is_A_T_model ? "tsv" : "json";
    if (args.count("log-format"))
        log_format = args["log-format"].as<string>();
    auto log_formats_vec = split(log_format,',');
    set<string> log_formats;
    for(auto& format: log_formats_vec)
        log_formats.insert(format);
    return log_formats;
}

vector<MCMC::Logger> construct_loggers(const boost::program_options::variables_map& args,
                                       const json& jinfo,
                                       owned_ptr<Model>& M, int subsample,
                                       int proc_id,
                                       const fs::path& dir_name)
{
    // FIXME - avoid the need to manually SubSampleFunction to every logger?

    using namespace MCMC;
    vector<Logger> loggers;

    bool is_A_T_model = args.count("align");

    int n_partitions = 0;
    if (is_A_T_model)
    {
        n_partitions = args.at("align").as<vector<string>>().size();
        assert(jinfo.at("partitions").size() == n_partitions);
    }

    auto base = (dir_name / ("C" + convertToString(proc_id+1))).string();

    auto TL = construct_table_function(M);

    auto log_formats = get_log_formats(args, args.count("align"));

    // Write out scalar numerical variables (and functions of them) to C<>.log
    if (log_formats.count("tsv"))
        loggers.push_back( append_line_to_file(base + ".log", Subsample_Function(TableLogger<string>(*TL), subsample)) );
  
    // FIXME: output all the extra stuff from construction_table_function( ).
    if (log_formats.count("json"))
        loggers.push_back( append_to_file(base + ".log.json", &logged_params_and_some_computed_stuff_with_header) );

    return loggers;
}

