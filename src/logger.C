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

#include "substitution.H"    // for get_model_probabilitiesby_alignment_column( )

#include "monitor.H"         // for show_smodel( )
#include "n_indels.H"
#include "tools/parsimony.H"
#include "alignment-util.H"

using std::endl;

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
vector<double> make_identifiable(const vector<double>& v,const vector< vector<int> >& indices)
{
  assert(indices.size());
  int N = indices[0].size();

  vector<double> v_sub = select(v,indices[0]);

  vector<int> O = iota(N);
  std::sort(O.begin(),O.end(), sequence_order<double>(v_sub));

  vector<int> O_all = iota<int>(v.size());
  for(int i=0;i<indices.size();i++) 
  {
    assert(indices[i].size() == N);
    for(int j=0;j<N;j++) {
      // indices[i][j] -> indices[i][O[j]]
      O_all[indices[i][j]] = indices[i][O[j]];
    }
  }
  vector<double> v2 = apply_mapping(v,invert(O_all));

  return v2;
}

vector<double> SortedTableFunction::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  vector<double> v = (*F)(P,t);

  for(int i=0;i<indices.size();i++)
    v = make_identifiable(v, indices[i]);

  return v;
}

SortedTableFunction::SortedTableFunction(const owned_ptr<TableFunction<double> >& f, const std::vector< std::vector< std::vector< int> > >& i_)
  :F(f), indices(i_), sorted_index(f->n_fields(),-1)
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

int TableLogger::n_fields() const
{
  return TF->n_fields();
}

vector<string> TableLogger::field_names() const
{
  return TF->field_names();
}

void TableLogger::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  if (t==0)
    *log_file<<join(field_names(),'\t')<<endl;

  vector<string> values = (*TF)(P,t);
  *log_file<<join(values,'\t')<<endl;
}

TableLogger::TableLogger(const string& name, const owned_ptr<TableFunction<string> >& tf)
  :FileLogger(name), TF(tf)
{ }

string TableViewerFunction::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  vector<string> fields = function->field_names();
  vector<string> values = (*function)(P,t);
  std::stringstream output;

  for(int i=0;i<values.size();i++)
  {
    output<<"    ";
    output<<fields[i]<<" = "<<values[i];
  }
  output<<"\n";

  return output.str();
}

TableViewerFunction::TableViewerFunction(const owned_ptr<TableFunction<string> >& f)
  :function(f)
{ }

double GetParameterFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  if (P->parameter_has_type<Double>(p))
    return P->get_parameter_value_as<Double>(p);
  else if (P->parameter_has_type<Int>(p))
    return P->get_parameter_value_as<Int>(p);
  else if (P->parameter_has_type<Bool>(p))
    return (int)P->get_parameter_value_as<Bool>(p);
  else
    return -1;
}

string GetPriorFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  return convertToString(log(P->prior()));
}

string GetAlignmentPriorFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters* PP = P.as<Parameters>();
  return convertToString(log(PP[p].prior_alignment()));
}

string GetLikelihoodFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  return convertToString(log(P->likelihood()));
}

string GetProbabilityFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  return convertToString(log(P->probability()));
}

string Get_Alignment_Length_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();
  return convertToString(PP[p].A->length());
}

string Get_Num_Substitutions_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();
  return convertToString(n_mutations(*PP[p].A, *PP[p].T_, cost_matrix));
}

string Get_Num_Indels_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();
  return convertToString(n_indels(*PP[p].A, *PP[p].T_));
}

string Get_Total_Length_Indels_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();
  return convertToString(total_length_indels(*PP[p].A, *PP[p].T_));
}
//
string Get_Total_Alignment_Length_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();

  int total = 0;
  for(int p=0;p<PP.n_data_partitions();p++)
    total += PP[p].A->length();
  return convertToString(total);
}

string Get_Total_Num_Substitutions_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();

  int total = 0;
  for(int p=0;p<PP.n_data_partitions();p++)
    total += n_mutations(*PP[p].A, *PP[p].T_);
  return convertToString(total);
}

string Get_Total_Num_Indels_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();

  int total = 0;
  for(int p=0;p<PP.n_data_partitions();p++)
    total += n_indels(*PP[p].A, *PP[p].T_);
  return convertToString(total);
}

string Get_Total_Total_Length_Indels_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();

  int total = 0;
  for(int p=0;p<PP.n_data_partitions();p++)
    total += total_length_indels(*PP[p].A, *PP[p].T_);
  return convertToString(total);
}

double mu_scale(const Parameters& P)
{
  SequenceTree T = *P.T;
  
  valarray<double> weights(P.n_data_partitions());
  for(int i=0;i<weights.size();i++)
    weights[i] = max(sequence_lengths(*P[i].A, P.T->n_leaves()));
  weights /= weights.sum();
  
  double mu_scale=0;
  for(int i=0;i<P.n_data_partitions();i++)
    mu_scale += P[i].branch_mean()*weights[i];

  return mu_scale;
}

string Get_Tree_Length_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  Parameters& PP = *P.as<Parameters>();

  const SequenceTree& T = *PP.T;

  return convertToString( mu_scale(PP) * tree_length(T) );
}

string TreeFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  const Parameters& PP = *P.as<Parameters>();

  SequenceTree T = *PP.T;
    
  double scale = mu_scale(PP);

  for(int b=0;b<T.n_branches();b++)
    T.branch(b).set_length(scale*T.branch(b).length());

  return T.write();
}

string MAP_Function::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  std::ostringstream output;

  efloat_t Pr = P->probability();
  if (Pr < MAP_score)
    goto out;

  MAP_score = Pr;

  output<<"iterations = "<<t<<"       MAP = "<<MAP_score<<"\n";
  output<<(*F)(P,t)<<"\n";
  
 out:
  return output.str();
}



string AlignmentFunction::operator()(const owned_ptr<Probability_Model>& P, long)
{
  const Parameters& PP = *P.as<Parameters>();
  std::ostringstream output;
  output<<*PP[p].A<<"\n";
  return output.str();
}

string Show_SModels_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  const Parameters& PP = *P.as<Parameters>();
  std::ostringstream output;
  show_smodels(output, PP);
  output<<"\n";
  return output.str();
}

string Subsample_Function::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  if (t%subsample == 0) 
    return (*function)(P,t);
  else
    return "";
}

string Mixture_Components_Function::operator()(const owned_ptr<Probability_Model>& P, long)
{
  std::ostringstream output;
  const Parameters& PP = *P.as<Parameters>();
  vector<vector<double> > model_pr = substitution::get_model_probabilities_by_alignment_column(PP[p]);
  for(int i=0;i<model_pr.size();i++)
    output<<join(model_pr[i],' ')<<"\n";

  output<<endl;

  return output.str();
}

void FunctionLogger::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  (*log_file)<<((*function)(P,t));
}

FunctionLogger::FunctionLogger(const std::string& filename, const owned_ptr<LoggerFunction<string> >& L)
  :FileLogger(filename),function(L)
{ }

string ConcatFunction::operator()(const owned_ptr<Probability_Model>& P, long t)
{
  string output;

  for(int i=0;i<functions.size();i++)
    output += (*functions[i])(*P,t);

  return output;
}

ConcatFunction& operator<<(ConcatFunction& CF,const owned_ptr<LoggerFunction<string> >& F)
{
  CF.add_function(F);
  return CF;
}

ConcatFunction& operator<<(ConcatFunction& CF,const string& s)
{
  CF.add_function( String_Function(s));
  return CF;
}

ConcatFunction operator<<(const ConcatFunction& CF,const owned_ptr<LoggerFunction<string> >& F)
{
  ConcatFunction CF2 = CF;
  return CF2<<F;
}

ConcatFunction operator<<(const ConcatFunction& CF,const string& s)
{
  ConcatFunction CF2 = CF;
  return CF2<<s;
}

ConcatFunction operator<<(const LoggerFunction<string>& F1,const owned_ptr<LoggerFunction<string> >& F2)
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
