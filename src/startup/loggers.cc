#include <set>
#include <boost/optional.hpp>

#include "loggers.H"
#include "tools/parsimony.H"
#include "computation/expression/expression.H"
#include "computation/expression/var.H"
#include "mcmc/mcmc.H"
#include "mcmc/logger.H"
#include "substitution/parsimony.H"
#include "models/parse.H"

using std::vector;
using std::map;
using std::string;
using std::set;
using std::shared_ptr;
using std::make_shared;

using std::to_string;

using boost::optional;

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
    boost::optional<map<string,set<string>>> signature;
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

void find_sub_loggers(Model& M, int& index, const string& name, vector<int>& logged_computations, vector<string>& logged_names)
{
    assert(index != -1);
    auto result = M.evaluate(index);
    if (result.is_double() or result.is_int())
    {
	logged_computations.push_back(index);
	logged_names.push_back(name);
	index = -1;
	return;
    }

    if (result.head().is_a<constructor>())
    {
	auto& c = result.head().as_<constructor>();
	if (is_bool_true(c) or is_bool_false(c))
	{
	    logged_computations.push_back(index);
	    logged_names.push_back(name);
	    index = -1;
	    return;
	}

	if (c.f_name == "[]")
	    return;

	if (c.f_name == ":")
	{
	    expression_ref L = M.get_expression(index);

	    expression_ref E = {var("Data.List.length"),L};
	    int length = M.evaluate_expression(E).as_int();

	    expression_ref E1 = {var("Data.List.head"),L};
	    expression_ref first_elem = M.evaluate_expression(E1);

	    if (first_elem.head().is_a<constructor>() and first_elem.head().as_<constructor>().f_name == "(,)")
	    {
		int index2 = -1;
		for(int i=0;i<length;i++)
		{
		    expression_ref x = {var("Data.List.!!"),L,i};
		    expression_ref x1 = {var("Data.Tuple.fst"),x};
		    expression_ref x2 = {var("Data.Tuple.snd"),x};
		    const String field_name = M.evaluate_expression( {var("Prelude.listToString"),x1} ).as_<String>();

		    if (index2 == -1)
			index2 = M.add_compute_expression(x2);
		    else
			M.set_compute_expression(index2, x2);

		    find_sub_loggers(M, index2, name+"["+field_name+"]", logged_computations, logged_names);
		}
	    }
	    else
	    {
		int index2 = -1;
		for(int i=0;i<length;i++)
		{
		    expression_ref E2 = {var("Data.List.!!"),L,i} ;
		    if (index2 == -1)
			index2 = M.add_compute_expression(E2);
		    else
			M.set_compute_expression(index2, E2);

		    find_sub_loggers(M, index2, name+"["+convertToString(i+1)+"]", logged_computations, logged_names);
		}
	    }
	}
    }
}

owned_ptr<MCMC::TableFunction<string>> construct_table_function(owned_ptr<Model>& M, const vector<string>& Rao_Blackwellize)
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

	    auto f = [i](const Parameters& P) {return convertToString( P.branch_scale(i)*tree_length(P.t()));};

	    TL->add_field(name, f);
	}

	TL->add_field("|T|", Get_Tree_Length_Function() );
    }

    {
	vector<int> logged_computations;
	vector<string> logged_names;

	vector<string> names_ = parameter_names(*M);
	for(auto& name: names_)
	    name = translate_structures(name);
	names_ = short_parameter_names(names_);
	set<string> names(names_.begin(), names_.end());

	for(int i=0;i<M->n_parameters();i++)
	{
	    string name = M->parameter_name(i);
	    if (name.size() and name[0] == '*' and log_verbose <= 0) continue;

	    int index = M->add_compute_expression(parameter(name));

	    find_sub_loggers(*M, index, names_[i], logged_computations, logged_names);
	}

	TableGroupFunction<expression_ref> T1;
	for(int i=0;i<logged_computations.size();i++)
	{
	    int index = logged_computations[i];
	    string name = logged_names[i];
	    T1.add_field(name, [index](const Model& M, long) {return get_computation(M,index);} );
	}

	SortedTableFunction T2(T1, get_un_identifiable_indices(logged_names));

	TL->add_fields( ConvertTableToStringFunction<expression_ref>( T2 ) );
    }

    vector<string> short_names = short_parameter_names(*M);
    for(const auto& p: Rao_Blackwellize)
    {
	auto p_index = M->maybe_find_parameter(p);
	if (not p_index)
	    p_index = find_index(short_names, p);

	if (p_index)
	{
	    if (not M->parameter_is_modifiable_reg(*p_index))
		throw myexception()<<"Can't Rao-Blackwellize parameter '"<<p<<"': not directly modifiable!";

	    vector<expression_ref> values = {0,1};
	    TL->add_field("RB-"+p, Get_Rao_Blackwellized_Parameter_Function(*p_index, values));
	}
	else
	    throw myexception()<<"No such parameter '"<<p<<"' to Rao-Blackwellize";
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
    o<<"\n";
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

