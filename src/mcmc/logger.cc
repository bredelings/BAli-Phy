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
#include "util/range.H"
#include "util/mapping.H"
#include "util/string/join.H"

#include "substitution/substitution.H"    // for get_model_probabilitiesby_alignment_column( )

#include "tree-align/n_indels2.H"
#include "substitution/parsimony.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "dp/2way.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"

#include "range/v3/all.hpp"

namespace views = ranges::views;

using std::endl;
using std::pair;

namespace MCMC {
    using std::vector;
    using std::valarray;
    using std::cerr;
    using std::clog;
    using std::string;
    using std::make_shared;
    using std::ostream;


    vector<json> json_to_table_function::operator()(const Model& M, long)
    {
	auto values = parameter_values( M.get_logged_parameters() );

	// Check the number of fields.
	if (values.size() != n_fields())
	    throw myexception()<<"Number of logged fields in "<<values.size()<<" but number of field names is "<<n_fields();

	return values;
    }


    vector<string> SortedTableFunction::field_names() const
    {
	vector<string> names = F->field_names();

//      Was this to uniquify by substitution model?
//	for(int i=0;i<names.size();i++)
//	    if (sorted_index[i] != -1)
//		names[i] += "[S" + convertToString(sorted_index[i]+1) + "]";

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
    vector<json> make_identifiable(const vector<json>& v,const vector< vector<int> >& indices)
    {
	assert(indices.size());
	int N = indices[0].size();

	auto v_sub = select(v,indices[0]);

	vector<int> O = views::ints(0,N) | ranges::to<vector>;
	ranges::sort(O, {}, [&](int i) { return (double)v_sub[i];});

	vector<int> O_all = views::ints(0, (int)v.size()) | ranges::to<vector>;
	for(auto& I: indices)
	{
	    assert(I.size() == N);
	    for(int j=0;j<N;j++) {
		// I[j] -> I[O[j]]
		O_all[I[j]] = I[O[j]];
	    }
	}
	auto v2 = apply_mapping(v,invert(O_all));

	return v2;
    }

    vector<json> SortedTableFunction::operator()(const Model& M, long t)
    {
	auto v = (*F)(M,t);

	for(int i=0;i<indices.size();i++)
	    v = make_identifiable(v, indices[i]);

	return v;
    }

    SortedTableFunction::SortedTableFunction(const TableFunction<json>& f, const std::vector< std::vector< std::vector< int> > >& i_)
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

    int alignment_length(const data_partition& P)
    {
        if (not P.has_pairwise_alignments()) return 0;

	auto branches = P.t().all_branches_from_node(0);

	int total = P.seqlength(0);
	for(int b: branches)
	    total += P.get_pairwise_alignment(b).count_insert();

	return total;
    }

    int alignment_length(const Parameters& P)
    {
	int total = 0;
	for(int p=0;p<P.n_data_partitions();p++)
            total += alignment_length(P[p]);
	return total;
    }

    string Get_Rao_Blackwellized_Parameter_Function::operator()(const Model& M, long)
    {
	if (node == -1) std::abort();

	owned_ptr<Model> M2 = M;
	vector<log_double_t> Prs;
	log_double_t total = 0;

	auto cur_value = M.get_modifiable_value(node);

        // Record probabilities
	for(const auto& value: values)
	{
	    if (value.type() != cur_value.type())
		throw myexception()<<"Rao-Blackwellization: Trying to set parameter with value '"<<cur_value<<"' to '"<<value<<"'";
	    M2->set_modifiable_value(node, value);
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
		if (is_bool_true(b))
		    value = 1;
		else if (is_bool_false(b))
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

    Get_Rao_Blackwellized_Parameter_Function::Get_Rao_Blackwellized_Parameter_Function(int n, const vector<expression_ref>& v)
	:node(n), values(v)
    { }

    string TreeFunction::operator()(const Model& M, long)
    {
	return M.get_logged_parameters()["tree"];
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
	return M.get_logged_parameters().at("alignments")[p];
    }

    string ConcatFunction::operator()(const Model& M, long t)
    {
	string output;

	for(int i=0;i<functions.size();i++)
	    output += (functions[i])(M,t);

	return output;
    }


    ConcatFunction& operator<<(ConcatFunction& CF,const logger_function<string>& F)
    {
	CF.add_function(F);
	return CF;
    }

    ConcatFunction& operator<<(ConcatFunction& CF,const string& s)
    {
	CF.add_function( String_Function(s));
	return CF;
    }

    ConcatFunction operator<<(const ConcatFunction& CF,const logger_function<string>& F)
    {
	ConcatFunction CF2 = CF;
	return CF2<<F;
    }

    ConcatFunction operator<<(const ConcatFunction& CF,const string& s)
    {
	ConcatFunction CF2 = CF;
	return CF2<<s;
    }

    ConcatFunction operator<<(const logger_function<string>& F1,const logger_function<string>& F2)
    {
	ConcatFunction CF;
	CF<<F1<<F2;
	return CF;
    }

    ConcatFunction operator<<(const logger_function<string>& F,const string& s)
    {
	ConcatFunction CF;
	CF<<F<<s;
	return CF;
    }


}
