#include "A-T-model.H"
#include "io.H"
#include "models/setup.H"
#include "tree/tree-util.H" //extends
#include "alignment/alignment-constraint.H"
#include "alignment/load.H"
#include "alignment/index-matrix.H"
#include "models/parse.H"
#include "models/rules.H"
#include "startup/paths.H"
#include "startup/help.hh"
#include "computation/expression/lambda.H"
#include <boost/optional/optional_io.hpp>
#include <boost/filesystem/operations.hpp>

namespace fs = boost::filesystem;

namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

using std::cout;
using std::cerr;
using std::clog;
using std::endl;
using std::ostream;
using std::string;
using std::vector;
using boost::optional;

/// Replace negative or zero branch lengths with saner values.
void sanitize_branch_lengths(SequenceTree& T)
{
    double min_branch = 1e-6;
    for(int i=0;i<T.n_branches();i++)
	if (T.branch(i).has_length() and T.branch(i).length() > 0)
	    min_branch = std::min(min_branch,T.branch(i).length());

    for(int i=0;i<T.n_branches();i++)
	if (T.branch(i).has_length())
	{
	    double L = T.branch(i).length();
	    if (L <= 0)
	    {
		if (log_verbose > 0)
		{
		    if (L < 0)
			std::cerr<<"Read in negative branch length "<<L<<": ";
		    else if (L == 0)
			std::cerr<<"Read in zero branch length: ";
		    std::cerr<<"setting length to "<<min_branch;
		}
		T.branch(i).set_length(min_branch);
	    }
	}
}

vector<double> get_geometric_heating_levels(const string& s)
{
    vector<double> levels;

    vector<string> parse = split(s,'/');

    if (parse.size() != 2) return levels;

    try
    {
	int n_levels = convertTo<int>(parse[1]);
	levels.resize(n_levels);
    
	parse = split(parse[0],'-');
	levels[0] = convertTo<double>(parse[0]);
	levels.back() = convertTo<double>(parse[1]);
	double factor = pow(levels.back()/levels[0], 1.0/(n_levels-1));
    
	for(int i=1;i<levels.size()-1;i++)
	    levels[i] = levels[i-1]*factor;
    
	return levels;
    }
    catch (...)
    {
	throw myexception()<<"I don't understand beta level string '"<<s<<"'";
    }
}


void setup_heating(int proc_id, const variables_map& args, Parameters& P) 
{
    if (args.count("beta")) 
    {
	string beta_s = args["beta"].as<string>();

	vector<double> beta = get_geometric_heating_levels(beta_s);
	if (not beta.size())
	    beta = split<double>(beta_s,',');

	P.PC->all_betas = beta;

	if (proc_id >= beta.size())
	    throw myexception()<<"not enough temperatures given: only got "<<beta.size()<<", wanted at least "<<proc_id+1;

	P.beta_index = proc_id;

	P.set_beta(beta[proc_id]);

	P.PC->beta_series.push_back(beta[proc_id]);
    }

    if (args.count("dbeta")) {
	vector<string> deltas = split(args["dbeta"].as<string>(),',');
	for(int i=0;i<deltas.size();i++) {
	    vector<double> D = split<double>(deltas[i],'*');
	    if (D.size() != 2)
		throw myexception()<<"Couldn't parse beta increment '"<<deltas[i]<<"'";
	    int D1 = (int)D[0];
	    double D2 = D[1];
	    for(int i=0;i<D1;i++) {
		double next = P.PC->beta_series.back() + D2;
		next = std::max(0.0,next);
		P.PC->beta_series.push_back(next);
	    }
	}
    }
    for(double b:P.PC->beta_series)
	std::cout<<b<<"\n";
}

vector<model_t>
get_imodels(const Rules& R, const shared_items<string>& imodel_names_mapping, const SequenceTree&)
{
    vector<model_t> imodels;
    for(int i=0;i<imodel_names_mapping.n_unique_items();i++) 
	imodels.push_back( get_model(R, "IndelModel",imodel_names_mapping.unique(i)) );
    return imodels;
}

template <typename T>
json optional_to_json(const boost::optional<T>& o)
{
    if (not o)
	return nullptr;
    else
	return *o;
}


// FIXME - maybe we should try to make a single giant model so that we get S1/parameter
//           exactly when this occurs for the logged parameter names, themselves.
//
//           This would also help it we have some variables that are outside of models, because
//             they are shared between them.

json log_summary(ostream& out_cache, ostream& out_screen,ostream& out_both,
		 const vector<model_t>& IModels, const vector<model_t>& SModels,
		 const vector<model_t>& ScaleModels,
		 const model_t& branch_length_model,
		 const Parameters& P,const variables_map& args, const Rules& rules)
{
    json info;
    json partitions;

    json tree;
    if (P.t().n_branches() > 1)
    {
	out_both<<"T:topology ~ uniform on tree topologies\n";
	tree["topology"] = "uniform";
    }

    if (P.t().n_branches() > 0)
    {
	out_both<<"T:lengths "<<branch_length_model.show(rules)<<endl<<endl;
	tree["lengths"] = branch_length_model.show(rules, false);
    }

    //-------- Log some stuff -----------//
    vector<string> filenames = args["align"].as<vector<string> >();

    for(int i=0;i<P.n_data_partitions();i++)
    {
	json partition;

	// 1. filename 
	out_cache<<"data"<<i+1<<" = "<<filenames[i]<<endl;
	out_screen<<"#"<<i+1<<": file = "<<filenames[i]<<endl;
	partition["filename"] = filenames[i];

	// 2. alphabet
	string a_name = P[i].get_alphabet().name;
	out_screen<<"#"<<i+1 <<": alphabet = "<<a_name<<"\n";
	out_cache<<"alphabet"<<i+1<<" = "<<a_name<<endl;
	partition["alphabet"] = a_name;

	// 3. substitution model
	auto s_index = P.smodel_index_for_partition(i);
	out_screen<<"#"<<i+1<<": subst "<<indent_and_wrap(0,12,1000,SModels[*s_index].show_main(rules,false))<<" (S"<<*s_index+1<<")\n";
	out_cache<<"smodel-index"<<i+1<<" = "<<P.smodel_index_for_partition(i)<<endl;
	partition["smodel"] = optional_to_json( P.smodel_index_for_partition(i) );

	// 4. indel model
	if (auto i_index = P.imodel_index_for_partition(i))
	    out_screen<<"#"<<i+1<<": indel "<<indent_and_wrap(0,12,1000,IModels[*i_index].show_main(rules, false))<<" (I"<<*i_index+1<<")\n";
	else
	    out_screen<<"#"<<i+1<<": indel = none\n";
	out_cache<<"imodel-index"<<i+1<<" = "<<P.imodel_index_for_partition(i)<<endl;
	partition["imodel"] = optional_to_json( P.imodel_index_for_partition(i) );

	// 5. scale model
	auto scale_index = P.scale_index_for_partition(i);
	out_screen<<"#"<<i+1<<": scale "<<indent_and_wrap(0,12,1000,ScaleModels[*scale_index].show_main(rules,false))<<" (Scale"<<*scale_index+1<<")\n";
	out_cache<<"scale-index"<<i+1<<" = "<<P.scale_index_for_partition(i)<<endl;
	partition["scale"] = optional_to_json( P.scale_index_for_partition(i) );

	out_screen<<endl;
	out_cache<<endl;
	partitions.push_back(partition);
    }

    json smodels = json::array();
    for(int i=0;i<P.n_smodels();i++)
    {
	//    out_cache<<"subst model"<<i+1<<" = "<<P.SModel(i).name()<<endl<<endl;
	out_cache<<"subst model"<<i+1<<" "<<SModels[i].show(rules)<<endl<<endl;
	smodels.push_back(SModels[i].pretty_model());
	string e = SModels[i].show_extracted(rules);
	if (e.size())
	    out_screen<<"Substitution model (S"<<i+1<<") -- priors:"<<e<<"\n\n";
    }

    json imodels = json::array();
    for(int i=0;i<P.n_imodels();i++)
    {
	out_cache<<"indel model"<<i+1<<" "<<IModels[i].show(rules)<<endl<<endl;
	imodels.push_back(IModels[i].pretty_model());
	string e = IModels[i].show_extracted(rules);
	if (e.size())
	    out_screen<<"Insertion/deletion model (I"<<i+1<<") -- priors:"<<e<<"\n\n";
    }

    json scales = json::array();
    for(int i=0;i<P.n_branch_scales();i++)
    {
	out_cache<<"scale model"<<i+1<<" "<<ScaleModels[i].show(rules)<<endl<<endl;
	scales.push_back(ScaleModels[i].pretty_model());
	string e = ScaleModels[i].show_extracted(rules);
	if (e.size())
	    out_screen<<"Scale model (Scale"<<i+1<<") -- priors:"<<e<<"\n\n";
    }

    info["partitions"] = partitions;
    info["smodels"] = smodels;
    info["imodels"] = imodels;
    info["scales"] = scales;
    info["tree"] = tree;
    return info;
}

void check_alignment_names(const alignment& A)
{
    const string forbidden = "();:\"'[]&,";

    for(int i=0;i<A.n_sequences();i++) {
	const string& name = A.seq(i).name;
	for(int j=0;j<name.size();j++)
	    for(int c=0;c<forbidden.size();c++)
		for(int pos=0;pos<name.size();pos++)
		    if (name[pos] == forbidden[c])
			throw myexception()<<"Sequence name '"<<name<<"' contains illegal character '"<<forbidden[c]<<"'";
    }
}

void check_alignment_values(const alignment& A,const string& filename)
{
    const alphabet& a = A.get_alphabet();

    for(int i=0;i<A.n_sequences();i++)
    {
	string name = A.seq(i).name;

	for(int j=0;j<A.length();j++) 
	    if (A.unknown(j,i))
		throw myexception()<<"Alignment file '"<<filename<<"' has a '"<<a.unknown_letter<<"' in sequence '"<<name<<"'.\n (Please replace with gap character '"<<a.gap_letter<<"' or wildcard '"<<a.wildcard<<"'.)";
    }
}

/// If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
void set_foreground_branches(Parameters& P, const SequenceTree& T)
{
    if (auto attribute_index = T.maybe_find_undirected_branch_attribute_index_by_name("foreground"))
    {
	for(int b=0;b<T.n_branches();b++)
	{
	    boost::any value = T.branch(b).undirected_attribute(*attribute_index);
	    if (value.empty()) continue;

	    int foreground_level = convertTo<int>( boost::any_cast<string>( value) );

	    P.set_parameter_value( P.find_parameter("*Main.branchCat"+convertToString(b+1)), foreground_level);
	    std::cerr<<"Setting branch '"<<b<<"' to foreground level "<<foreground_level<<"\n";;
	}
    }
}

void write_branch_numbers(ostream& o, SequenceTree T)
{
    // Write out a tree 
    auto flags = o.flags();
    o.unsetf(std::ios::floatfield);
    for(int b=0;b<T.n_branches();b++)
	T.branch(b).set_length(b);
    o<<"branch numbers = "<<T<<"\n\n";
    o.flags(flags);
}

// return the list of constrained branches
vector<int> load_alignment_branch_constraints(const string& filename, const SequenceTree& TC)
{
    // open file
    checked_ifstream file(filename,"alignment-branch constraint file");

    // read file
    string line;
    vector<vector<string> > name_groups;
    while(portable_getline(file,line)) {
	vector<string> names = split(line,' ');
	for(int i=names.size()-1;i>=0;i--)
	    if (names[i].size() == 0)
		names.erase(names.begin()+i);

	if (names.size() == 0) 
	    continue;
	else if (names.size() == 1)
	    throw myexception()<<"In alignment constraint file: you must specify more than one sequence per group.";
    
	name_groups.push_back(names);
    }

    // parse the groups into mask_groups;
    vector< dynamic_bitset<> > mask_groups(name_groups.size());
    for(int i=0;i<mask_groups.size();i++) 
    {
	mask_groups[i].resize(TC.n_leaves());
	mask_groups[i].reset();

	for(int j=0;j<name_groups[i].size();j++)
	{
	    if (auto index = find_index(TC.get_leaf_labels(), name_groups[i][j]))
		mask_groups[i][*index] = true;
	    else
		throw myexception()<<"Reading alignment constraint file '"<<filename<<"':\n"
				   <<"   Can't find leaf taxon '"<<name_groups[i][j]<<"' in the tree.";
	}
    }

    // 1. check that each group is a fully resolved clade in the constraint tree (no polytomies)
    // 2. construct the list of constrained branches
    // FIXME - what if the user specifies nested clades?  Won't we get branches twice, then?
    //       - SOLUTION: use a bitmask.
    vector<int> branches;
    for(int i=0;i<mask_groups.size();i++) 
    {
	// find the branch that corresponds to a mask
	boost::dynamic_bitset<> mask(TC.n_leaves());
	int found = -1;
	for(int b=0;b<2*TC.n_branches() and found == -1;b++) 
	{
	    mask = TC.partition(b);

	    if (mask_groups[i] == mask)
		found = b;
	}

	// complain if we can't find it
	if (found == -1) 
	    throw myexception()<<"Alignment constraint: clade '"
			       <<join(name_groups[i],' ')
			       <<"' not found in topology constraint tree.";
    
	// mark branch and child branches as constrained
	vector<const_branchview> b2 = branches_after_inclusive(TC,found); 
	for(int j=0;j<b2.size();j++) {
	    if (b2[j].target().degree() > 3)
		throw myexception()<<"Alignment constraint: clade '"
				   <<join(name_groups[i],' ')
				   <<"' has a polytomy in the topology constraint tree.";
	    branches.push_back(b2[j].undirected_name());
	}
    }


    return branches;
}

alignment unalign_A(const alignment& A)
{
    vector<int> L;
    for(int i=0;i<A.n_sequences();i++)
	L.push_back(A.seqlength(i));

    return get_alignment(unaligned_matrix(L),A);
}


owned_ptr<Model> create_A_and_T_model(const Rules& R, variables_map& args, const std::shared_ptr<module_loader>& L,
				      ostream& out_cache, ostream& out_screen, ostream& out_both, json& info,
				      int proc_id)
{
    //------ Determine number of partitions ------//
    vector<string> filenames = args["align"].as<vector<string> >();
    const int n_partitions = filenames.size();

    //-------------Choose an indel model--------------//
    auto imodel_names_mapping = get_mapping(args, "imodel", n_partitions);
    auto& imodel_mapping = imodel_names_mapping.item_for_partition;

    for(int i=0;i<imodel_names_mapping.n_unique_items();i++)
	if (imodel_names_mapping.unique(i) == "")
	    imodel_names_mapping.unique(i) = "rs07";

    //------------- Get smodel names -------------------
    auto smodel_names_mapping = get_mapping(args, "smodel", n_partitions);
    auto& smodel_mapping = smodel_names_mapping.item_for_partition;

    vector<model_t> full_smodels(smodel_names_mapping.n_unique_items());

    // 1. Get smodels for all SPECIFIED smodel names.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
	if (not smodel_names_mapping.unique(i).empty())
	    full_smodels[i] = get_model(R, "MultiMixtureModel[a]",smodel_names_mapping.unique(i), false);

    //------------- Get alphabet names -------------------
    shared_items<string> alphabet_names_mapping = get_mapping(args, "alphabet", filenames.size());
    vector<string> alphabet_names;
    for(int i=0;i<alphabet_names_mapping.n_partitions();i++)
	alphabet_names.push_back(alphabet_names_mapping[i]);

    // 2. For all SPECIFIED smodels, try to determine the alphabet.
    //    (Alphabets for SPECIFIED smodel groups must all specified or all unspecified).
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
	if (smodel_names_mapping.unique(i).empty()) continue;

	auto type = full_smodels[i].type;
	auto& constraints = full_smodels[i].constraints;
	auto alphabet_type = type.begin()->second;

	vector<int> a_specified;
	vector<int> a_unspecified;
	for(int j: smodel_names_mapping.partitions_for_item[i])
	{
	    if (alphabet_names[j].empty())
		a_unspecified.push_back(j);
	    else
		a_specified.push_back(j);
	}
	if (a_specified.size() and a_unspecified.size())
	    throw myexception()<<"SModel "<<i+1<<" applied to partition "<<a_specified[i]+1<<" (alphabet specified) and partition "
			       <<a_unspecified[0]+1<<" (alphabet not specified).";

	if (a_specified.size())
	{
	    for(int j: a_specified)
		if (alphabet_names[j] != alphabet_names[a_specified[0]])
		    throw myexception()<<"Partitions "<<a_specified[0]+1<<" and "<<j+1<<" have different alphabets, but are given the same substitution model!";
	    string a = alphabet_names[a_specified[0]];
	    if (alphabet_type.get_value<string>() == "Codons")
	    {
		if (a != "Codons")
		    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires a codon alphabet!";
	    }
	    else if (alphabet_type.get_value<string>() == "Triplets")
	    {
		if (a != "Triplets")
		    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires a triplet alphabet!";
	    }
	    else if (alphabet_type.get_value<string>() == "AA")
	    {
		if (a != "AA" and a != "Amino-Acids" and a != "Amino-AcidsWithStop")
		    throw myexception()<<"Partition "<<a_specified[0]+1<<" has specified alphabet '"<<a<<"' but the substitution model requires an amino-acid alphabet!";
	    }
	}
	else if (alphabet_type.get_value<string>() == "Codons")
	{
	    for(int j: smodel_names_mapping.partitions_for_item[i])
		alphabet_names[j] = "Codons";
	}
	else if (alphabet_type.get_value<string>() == "Triplets")
	{
	    for(int j: smodel_names_mapping.partitions_for_item[i])
		alphabet_names[j] = "Triplets";
	}
	else
	{
	    bool triplet_constraint = false;
	    for(auto& constraint: constraints)
		if (constraint.get_value<string>() == "Triplets" and constraint.begin()->second.get_value<string>() == "a")
		    triplet_constraint = true;

	    if (triplet_constraint)
		for(int j: smodel_names_mapping.partitions_for_item[i])
		    alphabet_names[j] = "Triplets";
	}
//      Use the auto-detected alphabet right now -- it leaves to better error messages.
//	else if (alphabet_type.get_value<string>() == "AA")
//	{
//	    for(int j: smodel_names_mapping.partitions_for_item[i])
//		alphabet_names[j] = "AA";
//	}
    }

    //----------- Load alignments  ---------//
    vector<alignment> A(filenames.size());

    // 3. -- load alignments for SPECIFIED alphabets
    for(int i=0;i<filenames.size();i++)
	if (alphabet_names[i].size())
	    // FIXME - if we are loading codons, FIRST determine the UNDERLYING alphabet, then
	    // But actually we DO this inside the routine, right?
	    A[i] = load_alignment(filenames[i], alphabet_names[i]);

    // 4. -- load alignments for UNSPECIFIED alphabets && set alphabet names.
    for(int i=0;i<filenames.size();i++)
    {
	if (not A[i].has_alphabet())
	    A[i] = load_alignment(filenames[i]);
	alphabet_names[i] = A[i].get_alphabet().name;
    }

    for(int i=0;i<A.size();i++) {
	check_alignment_names(A[i]);
	check_alignment_values(A[i],filenames[i]);
    }

    //----------- Load tree and link to alignments ---------//
    SequenceTree T;

    if (args.count("tree"))
    {
	T = load_T(args);
    }
    else
    {
	SequenceTree TC = star_tree(sequence_names(A[0]));
	if (args.count("t-constraint"))
	    TC = load_constraint_tree(args["t-constraint"].as<string>(),sequence_names(A[0]));

	T = TC;
	RandomTree(T);
    }

    for(auto& a: A)
	link(a,T,true);

    //--------- Handle branch lengths <= 0 and very short branch lengths --------//
    sanitize_branch_lengths(T);

    //--------- Set up indel model --------//
    auto full_imodels = get_imodels(R, imodel_names_mapping, T);

    // 5. ----- Check that all smodel-linked partitions end up with the same alphabet. -----
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
	int first_index = smodel_names_mapping.partitions_for_item[i][0];
	for(int j: smodel_names_mapping.partitions_for_item[i])
	    if (alphabet_names[j] != alphabet_names[first_index])
		throw myexception()<<"Partitions "<<first_index+1<<" and "<<j+1<<" have different alphabets '"<<alphabet_names[first_index]<<"' and '"<<alphabet_names[j]<<"', but share a linked substitution model!";
    }

    // 6. ------ Check that all alphabet-linked partitions end up with the same alphabet. -----
    for(int i=0;i<alphabet_names_mapping.n_unique_items();i++)
    {
	int first_index = alphabet_names_mapping.partitions_for_item[i][0];
	for(int other_index: alphabet_names_mapping.partitions_for_item[i])
	{
	    if (alphabet_names[first_index] != alphabet_names[other_index])
		throw myexception()<<"Partitions "<<first_index+1<<" and "<<other_index+1<<" have different alphabets, but are specified as being linked!";
	}
    }
    // 7. --------- Get UNSPECIFIED substitution models, which depend on the alphabet --------//
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
	if (smodel_names_mapping.unique(i).empty())
	{
	    // Check that all the alphabets are the same before using the alphabet to get a default model.
	    int first_index = smodel_names_mapping.partitions_for_item[i][0];
	    const alphabet& a = A[first_index].get_alphabet();

	    smodel_names_mapping.unique(i) = default_markov_model(a);

	    if (smodel_names_mapping.unique(i) == "")
		throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a.name<<"'";

	    full_smodels[i] = get_model(R, "MultiMixtureModel[a]",smodel_names_mapping.unique(i), false);
	}
    
    // 8. Check that alignment alphabet fits requirements from smodel.
    for(int i=0;i<smodel_names_mapping.n_unique_items();i++)
    {
	int first_index = smodel_names_mapping.partitions_for_item[i][0];
	const alphabet& a = A[first_index].get_alphabet();

	auto type = full_smodels[i].type;
	auto alphabet_type = type.begin()->second;

	if (alphabet_type.get_value<string>() == "Codons" and not dynamic_cast<const Codons*>(&a))
	    throw myexception()<<"Substitution model S"<<i+1<<" requires a codon alphabet, but sequences are '"<<a.name<<"'";;

	if (alphabet_type.get_value<string>() == "Triplets" and not dynamic_cast<const Triplets*>(&a))
	    throw myexception()<<"Substitution model S"<<i+1<<" requires a triplet alphabet, but sequences are '"<<a.name<<"'";;

	if (alphabet_type.get_value<string>() == "AA" and not dynamic_cast<const AminoAcids*>(&a))
	    throw myexception()<<"Substitution model S"<<i+1<<" requires an amino-acid alphabet, but sequences are '"<<a.name<<"'";;
    }

    //-------------- Which partitions share a scale? -----------//
    shared_items<string> scale_names_mapping = get_mapping(args, "scale", A.size());

    auto scale_mapping = scale_names_mapping.item_for_partition;

    vector<model_t> full_scale_models(scale_names_mapping.n_unique_items());

    for(int i=0;i<scale_names_mapping.n_unique_items();i++)
    {
	auto scale_model = scale_names_mapping.unique(i);
	if (scale_model.empty())
	    scale_model = "~gamma[0.5,2]";
	full_scale_models[i] = get_model(R, "Double", scale_model, false);
    }

    //-------------- Branch length model --------------------//
    model_t branch_length_model;
    {
	string M;
	if (args.count("branch-lengths"))
	    M = args["branch-lengths"].as<string>();
	else
	    M = "~iid[num_branches[T],gamma[0.5,div[2,num_branches[T]]]]";

	branch_length_model = get_model(R, "List[Double]", M, true, {{"T","Tree"}});
	branch_length_model.expression = lambda_quantify(var("arg_T"), branch_length_model.expression);
    }

    //-------------- Likelihood calculator types -----------//
    vector<int> likelihood_calculators(A.size(), 0);
    if (args.count("likelihood-calculators"))
    {
	likelihood_calculators = split<int>(args["likelihood-calculators"].as<string>(), ",");
	if (likelihood_calculators.size() == 1)
	    likelihood_calculators = vector<int>(A.size(), likelihood_calculators[0]);
	if (likelihood_calculators.size() != A.size())
	    throw myexception()<<"We have "<<A.size()<<" partitions, but only got "<<likelihood_calculators.size()<<" likelihood calculator types.";
	for(int i=0; i<A.size();i++)
	{
	    int c = likelihood_calculators[i];
	    if (c != 0 and c != 1)
		throw myexception()<<"Calculator "<<c<<" not recognized!";
	    if (c != 0 and imodel_mapping[i])
		throw myexception()<<"Calculator "<<c<<" does not work with a variable alignment!";
	}
    }

    bool unalign = args.count("unalign");
    for(int i=0;i<A.size();i++)
	if (unalign and imodel_mapping[i])
	    if (likelihood_calculators[i] != 0)
		throw myexception()<<"Can't unalign with calculator "<<likelihood_calculators[i]<<"!";
    
    //--------------- Create the Parameters object---------------//
    Model::key_map_t keys;
    if (args.count("set"))
	keys = parse_key_map(args["set"].as<vector<string> >());
    Parameters P(L, A, T, full_smodels, smodel_mapping, full_imodels, imodel_mapping, full_scale_models, scale_mapping, branch_length_model, likelihood_calculators, keys);

    //-------- Set the alignments for variable partitions ---------//
    for(int i=0;i<P.n_data_partitions();i++)
	if (not unalign and P.get_data_partition(i).has_IModel())
	    P.get_data_partition(i).set_alignment(A[i]);

    // If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
    set_foreground_branches(P, T);

    //------------- Write out a tree with branch numbers as branch lengths------------- //
    write_branch_numbers(out_cache, T);

    //-------------------- Log model -------------------------//
    info = log_summary(out_cache, out_screen, out_both, full_imodels, full_smodels, full_scale_models, branch_length_model, P,args, R);

    //----------------- Tree-based constraints ----------------//
    if (args.count("t-constraint"))
	P.PC->TC = load_constraint_tree(args["t-constraint"].as<string>(), T.get_leaf_labels());

    if (args.count("a-constraint"))
	P.PC->AC = load_alignment_branch_constraints(args["a-constraint"].as<string>(),P.PC->TC);

    if (not extends(T, P.PC->TC))
	throw myexception()<<"Initial tree violates topology constraints.";

    //---------- Alignment constraint (horizontal) -----------//
    vector<string> ac_filenames(P.n_data_partitions(),"");
    if (args.count("align-constraint")) 
    {
	ac_filenames = split(args["align-constraint"].as<string>(),':');

	if (ac_filenames.size() != P.n_data_partitions())
	    throw myexception()<<"Need "<<P.n_data_partitions()<<" alignment constraints (possibly empty) separated by colons, but got "<<ac_filenames.size();
    }

    for(int i=0;i<P.n_data_partitions();i++)
	P.PC->DPC[i].alignment_constraint = load_alignment_constraint(ac_filenames[i],T);

    //------------------- Handle heating ---------------------//
    setup_heating(proc_id,args,P);

    return P;
}

void write_initial_alignments(variables_map& args, int proc_id, const string& dir_name)
{
    vector<string> filenames = args["align"].as<vector<string> >();

    fs::path dir(dir_name);

    string base = "C" + convertToString(proc_id+1);

    int i=0;
    for(auto& filename: filenames)
    {
	auto source = fs::path(filename);
	auto target = fs::path(base+".P"+convertToString(i+1)+".initial.fasta");
	fs::copy_file(source,dir/target);
	i++;
    }
}

