/*
   Copyright (C) 2011 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file   logger.C
/// \brief  Provides classes for constructing MCMC samplers.
///
/// This file provides classes for constructing MCMC samplers.  The
/// class Sampler is used to run the main loop of the sampler for
/// bali-phy.
///
/// \author Benjamin Redelings
/// 

#include <iostream>

#include "mcmc.H"
#include "logger.H"
#include "util.H"

#include "substitution/substitution.H"    // for get_model_probabilitiesby_alignment_column( )

#include "monitor.H"         // for show_smodel( )
#include "n_indels2.H"
#include "tools/parsimony.H"
#include "tools/parsimony2.H"
#include "alignment/alignment-util.H"

using std::endl;
using std::pair;

namespace MCMC {
  using std::vector;
  using std::valarray;
  using std::cerr;
  using std::clog;
  using std::string;
  using std::ostream;

int SortedTableFunction::n_fields() const
{
  return F->n_fields();
}

vector<string> SortedTableFunction::field_names() const
{
  vector<string> names = F->field_names();

  for(int i=0;i<names.size();i++)
    if (sorted_index[i] != -1)
      names[i] += "[S" + convertToString(sorted_index[i]+1) + "]";

  return names;
}

/// \brief Force identifiability by sorting certain parameters according to the order of indices[0]
///
/// \param v The values of all parameters.
/// \param indices The indices of parameter values to reorder.
///
/// Parameter values indexed by indices[i] are sorted so that the parameter values indexed
/// by indices[0] are in increasing order.
///
vector<expression_ref> make_identifiable(const vector<expression_ref>& v,const vector< vector<int> >& indices)
{
  assert(indices.size());
  int N = indices[0].size();

  vector<expression_ref> v_sub = select(v,indices[0]);

  vector<int> O = iota(N);
  std::sort(O.begin(),O.end(), 
	    [&v_sub](int i, int j) 
	    {
	      double di = v_sub[i].as_double();
	      double dj = v_sub[j].as_double();
	      return di < dj;
	    });

  vector<int> O_all = iota<int>(v.size());
  for(int i=0;i<indices.size();i++) 
  {
    assert(indices[i].size() == N);
    for(int j=0;j<N;j++) {
      // indices[i][j] -> indices[i][O[j]]
      O_all[indices[i][j]] = indices[i][O[j]];
    }
  }
  vector<expression_ref> v2 = apply_mapping(v,invert(O_all));

  return v2;
}

vector<expression_ref> SortedTableFunction::operator()(const Model& M, long t)
{
  vector<expression_ref> v = (*F)(M,t);

  for(int i=0;i<indices.size();i++)
    v = make_identifiable(v, indices[i]);

  return v;
}

SortedTableFunction::SortedTableFunction(const TableFunction<expression_ref>& f, const std::vector< std::vector< std::vector< int> > >& i_)
  :F(f), indices(i_), sorted_index(f.n_fields(),-1)
{ 
  for(int i=0;i<indices.size();i++)
    for(int j=0;j<indices[i].size();j++)
      for(int k=0;k<indices[i][j].size();k++)
      {
	int index = indices[i][j][k];
	assert(0 <= index and index < sorted_index.size());
	sorted_index[index] = i;
      }
}

FileLogger::FileLogger(const string& filename)
  :log_file(new checked_ofstream(filename,false))
{ }

FileLogger::FileLogger(const std::ostream& o)
  :log_file(new ostream(o.rdbuf()))
{ }

string TableViewerFunction::operator()(const Model& M, long t)
{
  vector<string> fields = function->field_names();
  vector<string> values = (*function)(M,t);
  std::stringstream output;

  for(int i=0;i<values.size();i++)
  {
    output<<"    ";
    output<<fields[i]<<" = "<<values[i];
  }
  output<<"\n";

  return output.str();
}

TableViewerFunction::TableViewerFunction(const TableFunction<string>& f)
  :function(f)
{ }

expression_ref GetComputationFunction::operator()(const Model& M, long)
{
  expression_ref result = M.evaluate(index);

  if (result.is_double())
    return result;
  else if (result.is_int())
    return result;
  else if (result.head().is_a<constructor>())
  {
    auto& c = result.head().as_<constructor>();
    if (c.f_name == "Prelude.True")
      return 1;
    else if (c.f_name == "Prelude.False")
      return 0;
    else
      return -1;
  }
  else
    return -1;
}

GetComputationFunction::GetComputationFunction(int i)
  :index(i)
{ }

string GetPriorFunction::operator()(const Model& M, long)
{
  return convertToString(log(M.prior()));
}

string GetAlignmentPriorFunction::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  return convertToString(log(P[p].prior_alignment()));
}

string GetLikelihoodFunction::operator()(const Model& M, long)
{
  return convertToString(log(M.likelihood()));
}

string GetProbabilityFunction::operator()(const Model& M, long)
{
  return convertToString(log(M.probability()));
}

string Get_Alignment_Length_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  return convertToString(P[p].A().length());
}

string Get_Num_Substitutions_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  return convertToString(n_mutations(P[p].A(), P[p].t(), cost_matrix));
}

string Get_Num_Indels_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  return convertToString(n_indels(P[p].A(), P[p].t()));
}

string Get_Total_Length_Indels_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  return convertToString(total_length_indels(P[p].A(), P[p].t()));
}
//
string Get_Total_Alignment_Length_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);

  int total = 0;
  for(int p=0;p<P.n_data_partitions();p++)
    total += P[p].A().length();
  return convertToString(total);
}

string Get_Total_Num_Substitutions_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);

  int total = 0;
  for(int p=0;p<P.n_data_partitions();p++)
    total += n_mutations(P[p].A(), P[p].t());
  return convertToString(total);
}

string Get_Total_Num_Indels_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);

  int total = 0;
  for(int p=0;p<P.n_data_partitions();p++)
    total += n_indels(P[p].A(), P[p].t());
  return convertToString(total);
}

string Get_Total_Total_Length_Indels_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);

  int total = 0;
  for(int p=0;p<P.n_data_partitions();p++)
    total += total_length_indels(P[p].A(), P[p].t());
  return convertToString(total);
}

double mu_scale(const Parameters& P)
{
  valarray<double> weights(P.n_data_partitions());
  for(int i=0;i<weights.size();i++)
    weights[i] = max(sequence_lengths(P[i].A(), P.t().n_leaves()));
  weights /= weights.sum();
  
  // FIXME - we are just looking at branch 0!
  double mu_scale=0;
  for(int i=0;i<P.n_data_partitions();i++)
    mu_scale += P.get_branch_subst_rate(i,0)*weights[i];

  return mu_scale;
}

  string Get_Rao_Blackwellized_Parameter_Function::operator()(const Model& M, long)
  {
    if (parameter == -1) std::abort();

    owned_ptr<Model> M2 = M;
    vector<log_double_t> Prs;
    log_double_t total = 0;

    // Record probabilities
    for(const auto& v: values)
    {
      M2->set_parameter_value(parameter, v);
      log_double_t Pr = M2->probability();
      total += Pr;
      Prs.push_back(Pr);
    }

    // Rescale probabilities
    for(auto& Pr: Prs)
      Pr /= total;

    // Compute expectation
    log_double_t result = 0;
    for(int i=0;i<values.size();i++)
    {
      const auto& v = values[i];
      log_double_t Pr = Prs[i];
      log_double_t value = 0;
      if (v.head().is_a<constructor>())
      {
	auto& b = v.head().as_<constructor>();
	if (b.f_name == "Prelude.True")
	  value = 1;
	else if (b.f_name == "Prelude.False")
	  value = 0;
      }
      else if (v.is_int())
	value = v.as_int();
      else if (v.is_double())
	value = v.as_double();

      result += Pr*value;
    }

    return convertToString( result );
  }

  Get_Rao_Blackwellized_Parameter_Function::Get_Rao_Blackwellized_Parameter_Function(int p, const vector<expression_ref>& v)
    :parameter(p), values(v)
  { }

string Get_Tree_Length_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);

  return convertToString( mu_scale(P) * tree_length(P.t()) );
}

string TreeFunction::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);

  auto T = P.t();
    
  double scale = mu_scale(P);

  vector<double> L;
  for(int b=0;b<T.n_branches();b++)
    L.push_back(scale*T.branch_length(b));

  vector<string> names = P.get_labels();
  
  return write(T, L, names);
}

string MAP_Function::operator()(const Model& M, long t)
{
  std::ostringstream output;

  log_double_t Pr = M.probability();
  if (Pr < MAP_score)
    goto out;

  MAP_score = Pr;

  output<<"iterations = "<<t<<"       MAP = "<<MAP_score<<"\n";
  output<<(*F)(M,t)<<"\n";
  
 out:
  return output.str();
}



string AlignmentFunction::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  std::ostringstream output;
  output<<P[p].A()<<"\n";
  return output.str();
}

string Show_SModels_Function::operator()(const Model& M, long)
{
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  std::ostringstream output;
  show_smodels(output, P);
  output<<"\n";
  return output.str();
}

string Subsample_Function::operator()(const Model& M, long t)
{
  if (t%subsample == 0) 
    return (*function)(M,t);
  else
    return "";
}

string Mixture_Components_Function::operator()(const Model& M, long)
{
  std::ostringstream output;
  const Parameters& P = dynamic_cast<const Parameters&>(M);
  vector<vector<double> > model_pr = substitution::get_model_probabilities_by_alignment_column(P[p]);
  for(int i=0;i<model_pr.size();i++)
    output<<join(model_pr[i],' ')<<"\n";

  output<<endl;

  return output.str();
}

string Ancestral_Sequences_Function::operator()(const Model& M, long)
{
  std::ostringstream output;

  const Parameters& P = dynamic_cast<const Parameters&>(M);

  const alphabet& a = P[p].get_alphabet();

  alignment A = P[p].A();

  const vector<unsigned> smap = P[p].state_letters();

  vector<vector<pair<int,int> > > states = substitution::sample_ancestral_states(P[p]);
    
  for(int i=0;i<A.n_sequences();i++)
  {
    vector<int> columns = A.get_columns_for_characters(i);
    assert(columns.size() == states[i].size());
    for(int j=0;j<columns.size();j++)
    {
      int c = columns[j];
      assert(A.character(c,i));
      int state = states[i][j].second;
      int letter = smap[state];
      if (a.is_letter(A(c,i)))
	assert( A(c,i) == letter );
      else
	A.set_value(c,i, letter);
    }
  }

  A.print_fasta_to_stream(output);
  output<<endl;

  return output.str();
}

void FunctionLogger::operator()(const Model& M, long t)
{
  (*log_file)<<((*function)(M,t));
}

FunctionLogger::FunctionLogger(const std::string& filename, const LoggerFunction<string>& L)
  :FileLogger(filename),function(L)
{ }

string ConcatFunction::operator()(const Model& M, long t)
{
  string output;

  for(int i=0;i<functions.size();i++)
    output += (*functions[i])(M,t);

  return output;
}

ConcatFunction& operator<<(ConcatFunction& CF,const LoggerFunction<string>& F)
{
  CF.add_function(F);
  return CF;
}

ConcatFunction& operator<<(ConcatFunction& CF,const string& s)
{
  CF.add_function( String_Function(s));
  return CF;
}

ConcatFunction operator<<(const ConcatFunction& CF,const LoggerFunction<string>& F)
{
  ConcatFunction CF2 = CF;
  return CF2<<F;
}

ConcatFunction operator<<(const ConcatFunction& CF,const string& s)
{
  ConcatFunction CF2 = CF;
  return CF2<<s;
}

ConcatFunction operator<<(const LoggerFunction<string>& F1,const LoggerFunction<string>& F2)
{
  ConcatFunction CF;
  CF<<F1<<F2;
  return CF;
}

ConcatFunction operator<<(const LoggerFunction<string>& F,const string& s)
{
  ConcatFunction CF;
  CF<<F<<s;
  return CF;
}


}
