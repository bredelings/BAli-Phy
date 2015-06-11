#include "loggers.H"
#include "tools/parsimony.H"

#include <set>

using std::vector;
using std::string;
using std::set;

/// Determine the parameters of model \a M that must be sorted in order to enforce identifiability.
vector< vector< vector<int> > > get_un_identifiable_indices(const Model& M, const vector<string>& names)
{
  if (not dynamic_cast<const Parameters*>(&M)) return {};

  const Parameters& P = dynamic_cast<const Parameters&>(M);
  vector< vector< vector<int> > > indices;

  int n_smodels = P.n_smodels();

  for(int i=0;i<n_smodels+1;i++) 
  {
    string prefix = "^";
    if (i>0)
      prefix = string("S")+convertToString(i) + ".";

    vector< vector<int> > DP;
    if (parameters_with_extension(names, prefix + "DP.rate*").size()  )
    {
      DP.push_back( parameters_with_extension(names, prefix + "DP.rate*") );
      DP.push_back( parameters_with_extension(names, prefix + "DP.f*") );
      indices.push_back( DP );
    }

    vector< vector<int> > M3;
    if (parameters_with_extension(names, prefix + "M3.omega*").size() )
    {
      M3.push_back( parameters_with_extension(names, prefix + "M3.omega*") );
      M3.push_back( parameters_with_extension(names, prefix + "M3.f*") );
      indices.push_back( M3 );
    }
  }

  return indices;
}

void find_sub_loggers(Model& M, int& index, const string& name, vector<int>& logged_computations, vector<string>& logged_names)
{
  assert(index != -1);
  object_ref result = M.evaluate(index);
  if ((bool)dynamic_pointer_cast<const Double>(result) or (bool)dynamic_pointer_cast<const Int>(result))
  {
    logged_computations.push_back(index);
    logged_names.push_back(name);
    index = -1;
    return;
  }

  if (auto c = dynamic_pointer_cast<const constructor>(result))
  {
    if (c->f_name == "Prelude.True" or c->f_name == "Prelude.False")
    {
      logged_computations.push_back(index);
      logged_names.push_back(name);
      index = -1;
      return;
    }

    if (c->f_name == "[]")
      return;

    if (c->f_name == ":")
    {
      expression_ref L = M.get_expression(index);
      expression_ref E = (identifier("Prelude.length"),L);
      int length = *convert<const Int>(M.evaluate_expression(E));
      int index2 = -1;
      for(int i=0;i<length;i++)
      {
	expression_ref E2 = (identifier("Prelude.!!"),L,i) ;
	if (index2 == -1)
	  index2 = M.add_compute_expression(E2);
	else
	  M.set_compute_expression(index2, E2);

	find_sub_loggers(M, index2, name+"!!"+convertToString(i), logged_computations, logged_names);
      }
    }
  }
}

owned_ptr<MCMC::TableFunction<string> > construct_table_function(owned_ptr<Model>& M, const vector<string>& Rao_Blackwellize)
{
  owned_ptr<Parameters> P = M.as<Parameters>();

  using namespace MCMC;
  owned_ptr<TableGroupFunction<string> > TL = claim(new TableGroupFunction<string>);
  
  TL->add_field("iter", ConvertToStringFunction<long>( IterationsFunction() ) );
  TL->add_field("prior", GetPriorFunction() );
  if (P)
    for(int i=0;i<P->n_data_partitions();i++)
      if ((*P)[i].variable_alignment())
	TL->add_field("prior_A"+convertToString(i+1), GetAlignmentPriorFunction(i) );
  TL->add_field("likelihood", GetLikelihoodFunction() );
  TL->add_field("logp", GetProbabilityFunction() );
  
  {
    vector<int> logged_computations;
    vector<string> logged_names;

    vector<string> names_ = parameter_names(*M);
    set<string> names(names_.begin(), names_.end());

    // FIXME: Using short_parameter_names should be nice... but
    //          we are now logging EXPRESSIONS as well as actual parameters
    //        This makes such simplification difficult.

    for(int i=0;i<M->n_parameters();i++)
    {
      string name = M->parameter_name(i);
      if (name.size() and name[0] == '*' and not log_verbose) continue;

      int index = M->add_compute_expression(parameter(name));

      find_sub_loggers(*M, index, name, logged_computations, logged_names);
    }

    TableGroupFunction<object_ref> T1;
    for(int i=0;i<logged_computations.size();i++)
    {
      int index = logged_computations[i];
      string name = logged_names[i];
      T1.add_field(name, GetComputationFunction(index) );
    }

    SortedTableFunction T2(T1, get_un_identifiable_indices(*M, logged_names));

    TL->add_fields( ConvertTableToStringFunction<object_ref>( T2 ) );
  }

  for(const auto& p: Rao_Blackwellize)
  {
    int p_index = M->find_parameter(p);
    if (p_index == -1)
      throw myexception()<<"No such parameter '"<<p<<"' to Rao-Blackwellize";

    vector<object_ref> values = {Int(0),Int(1)};
    TL->add_field("RB-"+p, Get_Rao_Blackwellized_Parameter_Function(p_index, values));
  }

  if (not P) return TL;

  for(int i=0;i<P->n_data_partitions();i++)
  {
    if ((*P)[i].variable_alignment())
    {
      TL->add_field("|A"+convertToString(i+1)+"|", Get_Alignment_Length_Function(i) );
      TL->add_field("#indels"+convertToString(i+1), Get_Num_Indels_Function(i) );
      TL->add_field("|indels"+convertToString(i+1)+"|", Get_Total_Length_Indels_Function(i) );
    }
    const alphabet& a = (*P)[i].get_alphabet();
    TL->add_field("#substs"+convertToString(i+1), Get_Num_Substitutions_Function(i, unit_cost_matrix(a)) );
    if (const Triplets* Tr = dynamic_cast<const Triplets*>(&a))
      TL->add_field("#substs(nuc)"+convertToString(i+1), Get_Num_Substitutions_Function(i, nucleotide_cost_matrix(*Tr)) );
    if (const Codons* C = dynamic_cast<const Codons*>(&a))
      TL->add_field("#substs(aa)"+convertToString(i+1), Get_Num_Substitutions_Function(i, amino_acid_cost_matrix(*C)) );
  }
  
  if (P->variable_alignment()) {
    TL->add_field("|A|", Get_Total_Alignment_Length_Function() );
    TL->add_field("#indels", Get_Total_Num_Indels_Function() );
    TL->add_field("|indels|", Get_Total_Total_Length_Indels_Function() );
  }
  TL->add_field("#substs", Get_Total_Num_Substitutions_Function() );
  
  TL->add_field("|T|", Get_Tree_Length_Function() );

  return TL;
}

vector<owned_ptr<MCMC::Logger> > construct_loggers(owned_ptr<Model>& M, const vector<string>& Rao_Blackwellize, int proc_id, const string& dir_name)
{
  using namespace MCMC;
  vector<owned_ptr<Logger> > loggers;

  owned_ptr<Parameters> P = M.as<Parameters>();

  string base = dir_name + "/" + "C" + convertToString(proc_id+1);

  owned_ptr<TableFunction<string> > TF = construct_table_function(M, Rao_Blackwellize);

  // Write out scalar numerical variables (and functions of them) to C<>.p
  loggers.push_back( TableLogger(base +".p", *TF) );
  
  if (not P) return loggers;

  // Write out the (scaled) tree each iteration to C<>.trees
  loggers.push_back( FunctionLogger(base + ".trees", TreeFunction()<<"\n" ) );
  
  // Write out the MAP point to C<>.MAP - later change to a dump format that could be reloaded?
  {
    ConcatFunction F; 
    F<<TableViewerFunction(*TF)<<"\n";
    F<<Show_SModels_Function()<<"\n";
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
					Mixture_Components_Function(i) ) );

  // Write out ancestral sequences
  if (P->contains_key("log-ancestral"))
    for(int i=0;i<P->n_data_partitions();i++)
      loggers.push_back( FunctionLogger(base + ".P" + convertToString(i+1)+".ancestral.fastas", 
					Ancestral_Sequences_Function(i) ) );

  // Write out the alignments for each (variable) partition to C<>.P<>.fastas
  for(int i=0;i<P->n_data_partitions();i++)
    if ((*P)[i].variable_alignment()) 
    {
      string filename = base + ".P" + convertToString(i+1)+".fastas";

      ConcatFunction F;
      F<<"iterations = "<<ConvertToStringFunction<long> ( IterationsFunction() )<<"\n\n";
      F<<AlignmentFunction(i);

      loggers.push_back( FunctionLogger(filename, Subsample_Function(F,10) ) );
    }
  return loggers;
}

