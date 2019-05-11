#include <set>

#include "loggers.H"
#include "tools/parsimony.H"
#include "computation/expression/parameter.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include "computation/expression/var.H"
#include "mcmc/mcmc.H"
#include "mcmc/logger.H"
#include "substitution/parsimony.H"
#include "models/parse.H"
#include "util/mapping.H"
#include "util/string/join.H"
#include "util/json.hh"

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

// We should be able to collapse this to some kind of visitor pattern!

vector<string> parameter_names(const json& children);

vector<string> parameter_names_children(const json& children)
{
    vector<string> all_names;
    for(auto& [key, value]: children.items())
    {
	vector<string> names = parameter_names(value);
	for(auto& name: names)
	    all_names.push_back(key + "/" + name);

	if (value.find("value") != value.end())
	{
	    json v = *value.find("value");
	    if (v.is_array())
	    {
		// FIXME we are not looking looking into the value for "value" / "children"
		for(int i=0;i<v.size();i++)
		    all_names.push_back(key+"["+std::to_string(i+1)+"]");
	    }
	    else if (v.is_object())
	    {
		// FIXME we are not looking looking into value2 for "value" / "children"
		for(auto& [key2,value2]: v.items())
		    all_names.push_back(key+"["+key2+"]");
	    }
	    else
		all_names.push_back(key);
	}
    }
    return all_names;
}

vector<string> parameter_names(const json& j)
{
    auto children = j.find("children");
    if (children == j.end())
	return {};
    else
	return parameter_names_children(*children);
}

vector<json> parameter_values(const json& children);

vector<json> parameter_values_children(const json& children)
{
    vector<json> all_values;
    for(auto& [key, value]: children.items())
    {
	vector<json> values = parameter_values(value);
	for(auto& value: values)
	    all_values.push_back(std::move(value));

	if (value.find("value") != value.end())
	{
	    json v = *value.find("value");
	    if (v.is_array())
	    {
		// FIXME we are not looking looking into the value for "value" / "children"
		for(int i=0;i<v.size();i++)
		    all_values.push_back(v[i]);
	    }
	    else if (v.is_object())
	    {
		// FIXME we are not looking looking into value2 for "value" / "children"
		for(auto& [key2,value2]: v.items())
		    all_values.push_back(value2);
	    }
	    else
		all_values.push_back(v);
	}
    }
    return all_values;
}

vector<json> parameter_values(const json& j)
{
    auto children = j.find("children");
    if (children == j.end())
	return {};
    else
	return parameter_values_children(*children);
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
	    const alphabet& a = (*P)[i].get_alphabet();
	    TL->add_field(prefix+"#substs", [i,cost = unit_cost_matrix(a)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	    if (const Doublets* Do = dynamic_cast<const Doublets*>(&a))
		TL->add_field(prefix+"#substs(nuc)", [i,cost = nucleotide_cost_matrix(*Do)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	    if (const Triplets* Tr = dynamic_cast<const Triplets*>(&a))
		TL->add_field(prefix+"#substs(nuc)", [i,cost = nucleotide_cost_matrix(*Tr)](const Parameters& P) {return convertToString(n_mutations(P[i],cost));});
	    if (const Codons* C = dynamic_cast<const Codons*>(&a))
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
	vector<string> names = parameter_names_children(log);
	for(auto& name: names)
	    name = translate_structures(name);
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

vector<MCMC::Logger> construct_loggers(owned_ptr<Model>& M, int subsample, const vector<string>& Rao_Blackwellize, int proc_id, const string& dir_name)
{
    // FIXME - avoid the need to manually SubSampleFunction to every logger?

    using namespace MCMC;
    vector<Logger> loggers;

    owned_ptr<Parameters> P = M.as<Parameters>();

    string base = dir_name + "/" + "C" + convertToString(proc_id+1);

    auto TL = construct_table_function(M, Rao_Blackwellize);

    auto TF = TableLogger<string>(*TL);

    auto TF3 = [TL](const Model& M, long t) mutable { return table_logger_line(*TL,M,t); };

    Logger s = FunctionLogger(base +".log", Subsample_Function(TF, subsample));
  
    // Write out scalar numerical variables (and functions of them) to C<>.p
    loggers.push_back( s );
  
    Logger j = FunctionLogger(base + ".log.json", [](const Model& M, long){return M.get_logged_parameters();});

    loggers.push_back( j );

    if (not P) return loggers;

    // Write out the (scaled) tree each iteration to C<>.trees
    if (P->t().n_nodes() > 1)
	loggers.push_back( FunctionLogger(base + ".trees", Subsample_Function(TreeFunction()<<"\n", subsample) ) );
  
    // Write out the MAP point to C<>.MAP - later change to a dump format that could be reloaded?
    {
	ConcatFunction F; 
	F<<TF3<<"\n";
	F<<Show_SModels_Function()<<"\n";
	if (P->t().n_nodes() > 1)
	    for(int i=0;i<P->n_data_partitions();i++)
		if ((*P)[i].variable_alignment())
		    F<<AlignmentFunction(i)<<"\n\n";
	F<<TreeFunction()<<"\n\n";
	loggers.push_back( FunctionLogger(base + ".MAP", MAP_Function(F)) );
    }

    // Write out the probability that each column is in a particular substitution component to C<>.P<>.CAT
    if (P->contains_key("log-categories"))
	for(int i=0;i<P->n_data_partitions();i++)
	    loggers.push_back( FunctionLogger(base + ".P" + convertToString(i+1)+".CAT", 
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
		
		loggers.push_back( FunctionLogger(filename, Subsample_Function(F, alignment_subsample) ) );
	    }

    return loggers;
}

