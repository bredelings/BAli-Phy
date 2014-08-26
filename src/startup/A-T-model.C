#include "A-T-model.H"
#include "io.H"
#include "smodel/setup.H"
#include "imodel/setup.H"
#include "tree/tree-util.H" //extends
#include "alignment/alignment-constraint.H"

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

/// Replace negative or zero branch lengths with saner values.
void sanitize_branch_lengths(SequenceTree& T)
{
  double min_branch = 0.000001;
  for(int i=0;i<T.n_branches();i++)
  {
    if (not T.branch(i).has_length())
      T.branch(i).set_length(3.0/T.n_branches());
    if (T.branch(i).length() > 0)
      min_branch = std::min(min_branch,T.branch(i).length());
  }
  
  for(int i=0;i<T.n_branches();i++) {
    if (T.branch(i).length() == 0)
      T.branch(i).set_length(min_branch);
    if (T.branch(i).length() < 0)
      T.branch(i).set_length( - T.branch(i).length() );
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

    P.all_betas = beta;

    if (proc_id >= beta.size())
      throw myexception()<<"not enough temperatures given: only got "<<beta.size()<<", wanted at least "<<proc_id+1;

    P.beta_index = proc_id;

    P.set_beta(beta[proc_id]);

    P.beta_series.push_back(beta[proc_id]);
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
	double next = P.beta_series.back() + D2;
	next = std::max(0.0,next);
	P.beta_series.push_back(next);
      }
    }
  }
  for(double b:P.beta_series)
    std::cout<<b<<"\n";
}

void setup_partition_weights(const variables_map& args, Parameters& P) 
{
  if (args.count("partition-weights")) {

    string filename = args["partition-weights"].as<string>();

    const double n = 0.6;

    checked_ifstream partitions(filename,"partition weights file");
    string line;
    while(portable_getline(partitions,line)) {
      Partition p(P.T().get_leaf_labels(),line);
      portable_getline(partitions,line);
      double o = convertTo<double>(line);
      
      cerr<<p<<"      P = "<<o<<endl;
      if (o > n) {
	double w = n/(1-n)*(1-o)/o;
	log_double_t w2 = w;
	
	P.partitions.push_back(p);
	P.partition_weights.push_back(w2);
	
	cerr<<P.partitions.back()<<"      weight = "<<w<<endl;
      }
    }
  }
}

vector<expression_ref>
get_smodels(const module_loader& L,const variables_map& args, const vector<alignment>& A,
	    shared_items<string>& smodel_names_mapping)
{
  vector<expression_ref> smodels;
  for(int i=0;i<smodel_names_mapping.n_unique_items();i++) 
  {
    vector<alignment> alignments;
    for(int j=0;j<smodel_names_mapping.n_partitions_for_item(i);j++)
      alignments.push_back(A[smodel_names_mapping.partitions_for_item[i][j]]);

    if (smodel_names_mapping.unique(i) == "")
    {
      const alphabet& a = alignments[0].get_alphabet();
      smodel_names_mapping.unique(i) = default_markov_model(a);

      if (smodel_names_mapping.unique(i) == "")
	throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a.name<<"'";
    }

    expression_ref full_smodel = get_smodel(L,
						    args,
						    smodel_names_mapping.unique(i),
						    alignments);
    smodels.push_back(full_smodel);
    //    cout<<"SModel "<<i+1<<": prior = "<<log(smodels.back()->prior())<<"\n";
  }
  return smodels;
}

vector<expression_ref> 
get_imodels(const shared_items<string>& imodel_names_mapping, const SequenceTree& T)
{
  vector<expression_ref> imodels;
  for(int i=0;i<imodel_names_mapping.n_unique_items();i++) 
    imodels.push_back( get_imodel(imodel_names_mapping.unique(i), T) );
  return imodels;
}

void log_summary(ostream& out_cache, ostream& out_screen,ostream& out_both,
		 const shared_items<string>& imodels, const shared_items<string>& smodels,
		 const Parameters& P,const variables_map& args)
{
  //-------- Log some stuff -----------//
  vector<string> filenames = args["align"].as<vector<string> >();
  for(int i=0;i<filenames.size();i++) {
    out_cache<<"data"<<i+1<<" = "<<filenames[i]<<endl<<endl;
    out_cache<<"alphabet"<<i+1<<" = "<<P[i].get_alphabet().name<<endl<<endl;
  }

  for(int i=0;i<P.n_data_partitions();i++) {
    out_cache<<"smodel-index"<<i+1<<" = "<<P.get_smodel_index_for_partition(i)<<endl;
    out_cache<<"imodel-index"<<i+1<<" = "<<P.get_imodel_index_for_partition(i)<<endl;
  }
  out_cache<<endl;

  for(int i=0;i<P.n_smodels();i++)
    //    out_cache<<"subst model"<<i+1<<" = "<<P.SModel(i).name()<<endl<<endl;
    out_cache<<"subst model"<<i+1<<" = "<<smodels.unique(i)<<endl<<endl;

  for(int i=0;i<P.n_imodels();i++)
    out_cache<<"indel model"<<i+1<<" = "<<imodels.unique(i)<<endl<<endl;

  out_screen<<"\n";
  for(int i=0;i<P.n_data_partitions();i++) {
    int s_index = P.get_smodel_index_for_partition(i);
    //    out_screen<<"#"<<i+1<<": subst ~ "<<P.SModel(s_index).name()<<" ("<<s_index+1<<")    ";
    out_screen<<"#"<<i+1<<": subst ~ "<<smodels[i]<<" ("<<s_index+1<<")    ";

    int i_index = P.get_imodel_index_for_partition(i);
    string i_name = "none";
    if (i_index != -1)
      i_name = imodels[i];
    out_screen<<" indel ~ "<<i_name<<" ("<<i_index+1<<")"<<endl;;
  }
  out_screen<<"\n";

  out_both<<"Prior on branch lengths T[b]:\n";
  if (P.branch_prior_type == 0)
    out_both<<" T[b] ~ Exponential(Main.mu<i>)   [mean=Main.mu<i>, variance=Main.mu<i>^2]"<<endl;
  else if (P.branch_prior_type == 1)
    out_both<<" T[b] ~ Gamma(alpha=0.5, beta=2*Main.mu<i>)   [mean=Main.mu<i>, variance=2*Main.mu<i>^2]"<<endl;
  else if (P.branch_prior_type == 2)
  {
    out_both<<" T[b]/Tree length ~ Dirichlet(alpha=0.5)"<<endl;
    out_both<<" Tree length ~ Gamma(alpha=0.5, beta=2*Main.mu<i>)   [mean=Main.mu<i>, variance=2*Main,mu<i>^2]"<<endl;
  }
  if (P.n_data_partitions() > 1)
    out_both<<"(Each partition i has a separate 'Main.mu<i>' except where specified by --same-scale.)"<<endl;
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
void set_foreground_branches(Parameters& P)
{
  const SequenceTree& T = P.T();

  if (T.find_undirected_branch_attribute_index_by_name("foreground") != -1)
  {
    int attribute_index = T.find_undirected_branch_attribute_index_by_name("foreground");

    for(int b=0;b<T.n_branches();b++)
    {
      boost::any value = T.branch(b).undirected_attribute(attribute_index);
      if (value.empty()) continue;

      int foreground_level = convertTo<int>( boost::any_cast<string>( value) );

      P.set_parameter_value( P.find_parameter("*Main.branchCat"+convertToString(b+1)), object_ref(Int(foreground_level)));
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
      int index = find_index(TC.get_leaf_labels(), name_groups[i][j]);

      if (index == -1)
	throw myexception()<<"Reading alignment constraint file '"<<filename<<"':\n"
			   <<"   Can't find leaf taxon '"<<name_groups[i][j]<<"' in the tree.";
      else
	mask_groups[i][index] = true;
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


owned_ptr<Model> create_A_and_T_model(variables_map& args, const module_loader& L,
						  ostream& out_cache, ostream& out_screen, ostream& out_both,
						  int proc_id)
{
  if (args["subA-index"].as<string>() == "leaf")
    use_internal_index = false;

  //------ Determine number of partitions ------//
  vector<string> filenames = args["align"].as<vector<string> >();
  const int n_partitions = filenames.size();

  //-------------Choose an indel model--------------//
  //FIXME - make a shared_items-like class that also holds the items to we can put the whole state in one object.
  //FIXME - make bali-phy.C a more focussed and readable file - remove setup junk to other places? (where?)
  vector<int> imodel_mapping(n_partitions, -1);
  shared_items<string> imodel_names_mapping(vector<string>(),imodel_mapping);

  if (args.count("traditional")) {
    if (args.count("imodel"))
      throw myexception()<<"Error: you specified both --imodel <arg> and --traditional";
  }
  else {
    imodel_names_mapping = get_mapping(args, "imodel", n_partitions);

    for(int i=0;i<imodel_names_mapping.n_unique_items();i++)
      if (imodel_names_mapping.unique(i) == "")
	imodel_names_mapping.unique(i) = "RS07";
      
    imodel_mapping = imodel_names_mapping.item_for_partition;
  }

  //----------- Load alignments and tree ---------//
  vector<alignment> A;
  SequenceTree T;
  // FIXME - do I want to allow/remove internal node sequences here?
  vector<bool> internal_sequences(n_partitions);
  for(int i=0;i<internal_sequences.size();i++)
    internal_sequences[i] = (imodel_mapping[i] != -1);

  //       - and only if there is an indel model?
  if (args.count("tree"))
    load_As_and_T(args,A,T,internal_sequences);
  else
    load_As_and_random_T(args,A,T,internal_sequences);

  for(int i=0;i<A.size();i++) {
    check_alignment_names(A[i]);
    check_alignment_values(A[i],filenames[i]);
  }

  //--------- Handle branch lengths <= 0 --------//
  sanitize_branch_lengths(T);

  //--------- Do we have enough sequences? ------//
  //    if (T.n_leaves() < 3)
  //      throw myexception()<<"At least 3 sequences must be provided - you provided only "<<T.n_leaves()<<".";

  //--------- Set up the substitution model --------//

  // FIXME - change to return a (model, standardized name) pair.
  vector<expression_ref> full_imodels = get_imodels(imodel_names_mapping, T);

  //--------- Set up the substitution model --------//
  shared_items<string> smodel_names_mapping = get_mapping(args, "smodel", n_partitions);
    
  vector<int> smodel_mapping = smodel_names_mapping.item_for_partition;

  // FIXME - change to return a (model, standardized name) pair.
  vector<expression_ref> full_smodels = get_smodels(L,args,A,smodel_names_mapping);

  //-------------- Which partitions share a scale? -----------//
  shared_items<string> scale_names_mapping = get_mapping(args, "same-scale", A.size());

  vector<int> scale_mapping = scale_names_mapping.item_for_partition;

  //-------------Create the Parameters object--------------//
  Parameters P(L, A, T, full_smodels, smodel_mapping, full_imodels, imodel_mapping, scale_mapping);

  // If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
  set_foreground_branches(P);

  //------------- Set the branch prior type --------------//
  string branch_prior = args["branch-prior"].as<string>();
  if (branch_prior == "Exponential")  
    P.branch_prior_type = 0;
  else if (branch_prior == "Gamma") 
    P.branch_prior_type = 1;
  else if (branch_prior == "Dirichlet") 
    P.branch_prior_type = 2;
  else
    throw myexception()<<"I don't understand --branch-prior argument '"<<branch_prior<<"'.\n  Only 'Exponential' and 'Gamma' are allowed.";

  //------------- Write out a tree with branch numbers as branch lengths------------- //
  write_branch_numbers(out_cache, T);

  //-------------------- Log model -------------------------//
  log_summary(out_cache,out_screen,out_both,imodel_names_mapping,smodel_names_mapping,P,args);

  //----------------- Tree-based constraints ----------------//
  if (args.count("t-constraint"))
    P.TC = cow_ptr<SequenceTree>(load_constraint_tree(args["t-constraint"].as<string>(), T.get_leaf_labels()));

  if (args.count("a-constraint"))
    P.AC = load_alignment_branch_constraints(args["a-constraint"].as<string>(),*P.TC);

  if (not extends(T, *P.TC))
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
    P[i].alignment_constraint = load_alignment_constraint(ac_filenames[i],T);

  //------------------- Handle heating ---------------------//
  setup_heating(proc_id,args,P);

  // read and store partitions and weights, if any.
  setup_partition_weights(args,P);

  return P;
}

void write_initial_alignments(const owned_ptr<Model>& M, int proc_id, string dir_name)
{
  const Parameters* P = M.as<const Parameters>();
  if (not P) return;

  vector<alignment> A;
  for(int i=0;i<P->n_data_partitions();i++)
    A.push_back((*P)[i].A());

  string base = dir_name + "/" + "C" + convertToString(proc_id+1);
  for(int i=0;i<A.size();i++)
  {
    checked_ofstream file(base+".P"+convertToString(i+1)+".initial.fasta");
    file<<A[i]<<endl;
  }
}

