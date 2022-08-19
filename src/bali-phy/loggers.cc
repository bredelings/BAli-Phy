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
    for(auto& label: extensions)
	labels.insert(label.first);
    return labels;
}


// {S1 -> {[0] -> [a,b],[1]->[c,d]}, S2 -> {[0] -> [x,y],[1] -> [z,w]} }
vector<vector<vector<int>>> get_un_identifiable_indices(const vector<string>& names, const vector<string>& patterns)
{
    // groups[prefix][label] = {indices} ; and the number of indices should be patterns.size
    map<string,map<string,vector<int>>> groups;
    std::optional<map<string,set<string>>> signature;
    for(int i=0;i<patterns.size();i++)
    {
	const auto& pattern = patterns[i];

	auto z = parameters_with_extension(names, pattern);
	if (not signature)
	    signature = extract_signature(z);
	else
	    if (*signature != extract_signature(z))
		throw myexception()<<"Signature's don't match!";  // FIXME - how to display this?

	// groups[prefix][label] = index
	for(auto& x: z)
	{
	    auto& prefix = x.first;
	    auto& label_mapping = x.second;

	    auto& group = groups[prefix];
	    for(auto& y: label_mapping)
	    {
		auto& label = y.first;
		int index = y.second;
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

json logged_params_and_some_computed_stuff(const Model& M, long t)
{
    using namespace MCMC;

    json j;

    add_value(j, "iter", t);
    add_value(j, "prior", log(M.prior()) );
    add_value(j, "likelihood", log(M.likelihood()));
    add_value(j, "posterior", log(M.probability()));

    if (auto P = dynamic_cast<const Parameters*>(&M); P and P->t().n_nodes() > 1)
    {
	if (P->variable_alignment())
        {
	    add_value(j, "prior_A", log(P->prior_alignment()));
            add_value(j, "|A|", alignment_length(*P));
	    add_value(j, "#indels", n_indels(*P));
	    add_value(j, "|indels|", n_indels(*P));
	}
	add_value(j, "#substs", n_substs(*P));

	for(int i=0;i<P->n_data_partitions();i++)
	{
            auto part = (*P)[i];
            json partition_j;

	    add_value(partition_j, "likelihood", log(part.likelihood()));
	    if ((*P)[i].variable_alignment())
	    {
		add_value(partition_j, "prior_A", log(part.prior_alignment()));
		add_value(partition_j, "|A|", alignment_length(part));
		add_value(partition_j, "#indels", n_indels(part));
		add_value(partition_j, "|indels|", total_length_indels(part));
	    }

	    auto a = (*P)[i].get_alphabet();
	    add_value(partition_j, "#substs", n_mutations(part, unit_cost_matrix(*a)));

	    if (auto Do = dynamic_pointer_cast<const Doublets>(a))
		add_value(partition_j, "#substs(nuc)", n_mutations(part, nucleotide_cost_matrix(*Do)));
	    if (auto Tr = dynamic_pointer_cast<const Triplets>(a))
		add_value(partition_j, "#substs(nuc)", n_mutations(part, nucleotide_cost_matrix(*Tr)));
	    if (auto C = dynamic_pointer_cast<const Codons>(a))
		add_value(partition_j, "#substs(aa)", n_mutations(part, amino_acid_cost_matrix(*C)));

            add_children(j, "P"+convertToString(i+1), partition_j);
	}

	// Add fields Scale<s>*|T|
	for(int i=0;i<P->n_branch_scales();i++)
	{
	    auto name = string("Scale[")+to_string(i+1)+"]*|T|";

            add_value(j, name, P->get_branch_scale(i)*tree_length(P->t()));
	}

	add_value(j, "|T|", tree_length(P->t()));
    }

    add_children(j, "parameters", M.get_logged_parameters());

    return j;
}

string logged_params_and_some_computed_stuff_with_header(const Model& M, long t)
{
    json j = logged_params_and_some_computed_stuff(M,t);

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


owned_ptr<MCMC::TableFunction<string>> construct_table_function(owned_ptr<Model>& M, const vector<string>&)
{
    owned_ptr<Parameters> P = M.as<Parameters>();

    using namespace MCMC;
    owned_ptr<TableGroupFunction<string> > TL = claim(new TableGroupFunction<string>);
  
    TL->add_field("iter", [](const Model&, long t) {return convertToString(t);});
    TL->add_field("prior", [](const Model& M, long) {return convertToString(log(M.prior()));});
    TL->add_field("likelihood", [](const Model& M, long) {return convertToString(log(M.likelihood()));});
    TL->add_field("posterior", [](const Model& M, long) {return convertToString(log(M.probability()));});
  
    if (P and P->t().n_nodes() > 1)
    {
	if (P->variable_alignment()) {
	    TL->add_field("prior_A", [](const Parameters& P) {return convertToString(log(P.prior_alignment()));});
	    TL->add_field("|A|", Get_Total_Alignment_Length_Function() );
	    TL->add_field("#indels", Get_Total_Num_Indels_Function() );
	    TL->add_field("|indels|", Get_Total_Total_Length_Indels_Function() );
	}
	TL->add_field("#substs", Get_Total_Num_Substitutions_Function() );
	for(int i=0;i<P->n_data_partitions();i++)
	{
	    string prefix = "P"+convertToString(i+1)+"/";
	    TL->add_field(prefix + "likelihood", [i](const Parameters& P) {return convertToString(log(P[i].likelihood()));});
	    if ((*P)[i].variable_alignment())
	    {
		TL->add_field(prefix+"prior_A", [i](const Parameters& P) {return convertToString(log(P[i].prior_alignment()));});
		TL->add_field(prefix+"|A|", [i](const Parameters& P){return convertToString(alignment_length(P[i]));});
		TL->add_field(prefix+"#indels", [i](const Parameters& P){return convertToString(n_indels(P[i]));});
		TL->add_field(prefix+"|indels|", [i](const Parameters& P){return convertToString(total_length_indels(P[i]));});
	    }
	    auto a = (*P)[i].get_alphabet();
	    TL->add_field(prefix+"#substs", [i,cost = unit_cost_matrix(*a)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	    if (auto Do = dynamic_pointer_cast<const Doublets>(a))
		TL->add_field(prefix+"#substs(nuc)", [i,cost = nucleotide_cost_matrix(*Do)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	    if (auto Tr = dynamic_pointer_cast<const Triplets>(a))
		TL->add_field(prefix+"#substs(nuc)", [i,cost = nucleotide_cost_matrix(*Tr)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	    if (auto C = dynamic_pointer_cast<const Codons>(a))
		TL->add_field(prefix+"#substs(aa)", [i,cost = amino_acid_cost_matrix(*C)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	}

	// Add fields Scale<s>*|T|
	for(int i=0;i<P->n_branch_scales();i++)
	{
	    auto name = string("Scale[")+to_string(i+1)+"]*|T|";

	    auto f = [i](const Parameters& P) {return convertToString( P.get_branch_scale(i)*tree_length(P.t()));};

	    TL->add_field(name, f);
	}

	TL->add_field("|T|", Get_Tree_Length_Function() );
    }

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


string table_logger_line(MCMC::TableFunction<string>& TF, const Model& M, long t)
{
    std::ostringstream o;
    auto values = TF(M,t);
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
                                       owned_ptr<Model>& M, int subsample,
                                       const vector<string>& Rao_Blackwellize,
                                       int proc_id,
                                       const fs::path& dir_name)
{
    // FIXME - avoid the need to manually SubSampleFunction to every logger?

    using namespace MCMC;
    vector<Logger> loggers;

    owned_ptr<Parameters> P = M.as<Parameters>();

    auto base = (dir_name / ("C" + convertToString(proc_id+1))).string();

    auto TL = construct_table_function(M, Rao_Blackwellize);

    auto TF3 = &logged_params_and_some_computed_stuff_with_header;

    auto log_formats = get_log_formats(args, (bool)P);

    // Write out scalar numerical variables (and functions of them) to C<>.log
    if (log_formats.count("tsv"))
        loggers.push_back( append_line_to_file(base + ".log", Subsample_Function(TableLogger<string>(*TL), subsample)) );
  
    // FIXME: output all the extra stuff from construction_table_function( ).
    if (log_formats.count("json"))
        loggers.push_back( append_to_file(base + ".log.json", &logged_params_and_some_computed_stuff_with_header) );

    if (not P) return loggers;

    // Write out the (scaled) tree each iteration to C<>.trees
    if (P->t().n_nodes() > 1)
	loggers.push_back( append_line_to_file(base + ".trees", Subsample_Function(TreeFunction(), subsample) ) );
  
    // Write out the MAP point to C<>.MAP - later change to a dump format that could be reloaded?
    {
	ConcatFunction F; 
	F<<TF3<<"\n";
	if (P->t().n_nodes() > 1)
	    for(int i=0;i<P->n_data_partitions();i++)
		if ((*P)[i].variable_alignment())
		    F<<AlignmentFunction(i)<<"\n\n";
	F<<TreeFunction()<<"\n\n";
	loggers.push_back( append_to_file(base + ".MAP", MAP_Function(F)) );
    }

    // Write out the probability that each column is in a particular substitution component to C<>.P<>.CAT
    if (P->contains_key("log-categories"))
	for(int i=0;i<P->n_data_partitions();i++)
	    loggers.push_back( append_to_file(base + ".P" + convertToString(i+1)+".CAT", 
                                              Subsample_Function(Mixture_Components_Function(i),subsample) ) );

    // Write out the alignments for each (variable) partition to C<>.P<>.fastas
    int alignment_extra_subsample = P->load_value("alignment-extra-subsample",10);
    int alignment_subsample = alignment_extra_subsample*subsample;
    if (P->t().n_nodes() > 1)
	for(int i=0;i<P->n_data_partitions();i++)
	    if ((*P)[i].variable_alignment() or P->load_value("write-fixed-alignments",false)) 
	    {
		string filename = base + ".P" + convertToString(i+1)+".fastas";
		
		ConcatFunction F;
		auto iterations = [](const Model&, long t) {return convertToString(t);};
		F<<"iterations = "<<iterations<<"\n\n";
		auto infer_ambiguous_observed = P->load_value("infer-ambiguous-observed",false);
		F<<Ancestral_Sequences_Function(i, infer_ambiguous_observed);
		
		loggers.push_back( append_to_file(filename, Subsample_Function(F, alignment_subsample) ) );
	    }

    return loggers;
}

