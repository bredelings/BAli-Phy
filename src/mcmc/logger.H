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
/// \file   logger.H
/// \brief  Provides classes for constructing logging functions for MCMC samplers.
///
/// This file provides classes for constructing logging functions for MCMC samplers.
/// These loggers report samples from the running chain, as well as various statistics.
/// Writing to files is mostly separated from the generation of the text that will be
/// written.
///
/// \author Benjamin Redelings
/// 

#ifndef LOGGER_H
#define LOGGER_H

#include <iostream>
#include <vector>
#include <string>
#include <filesystem>

#include "models/parameters.H"
#include "tools/stats-table.H"
#include "util/io.H"
#include "util/owned-ptr.H"
#include "computation/object.H"
#include "util/string/join.H"
#include "util/json.hh"

struct slice_function;

namespace MCMC {

    typedef std::function<void(const Model& M, const json&, long)> Logger;

    template <typename T>
    using logger_function = std::function<T (const Model&, const json&, long)>;

    int alignment_length(const data_partition& P);

    int alignment_length(const Parameters& P);

    std::string write_newick(const Parameters& P);

    struct TreeFunction
    {
	std::string operator()(const Model& M, const json& j, long t);
    };

    class MAP_Function
    {
	log_double_t MAP_score;
	std::shared_ptr<logger_function<std::string> > F;
    public:
	std::string operator()(const Model& M, const json& j, long t);
	MAP_Function(const logger_function<std::string>& f):MAP_score(0),F(std::make_shared<logger_function<std::string>>(f)) { }
    };

    template <typename T>
    class TableFunction
    {
    public:
	virtual TableFunction* clone() const =0;
	virtual std::vector<std::string> field_names() const =0;
	virtual int n_fields() const {return field_names().size();}
	virtual std::vector<T> operator()(const Model&,const json&,long)=0;
	virtual ~TableFunction() {};
    };

    class json_to_table_function: public TableFunction<json>
    {
	std::vector<std::string> field_names_;
    public:
	virtual json_to_table_function* clone() const {return new json_to_table_function(*this);}
	virtual std::vector<std::string> field_names() const {return field_names_;}
	virtual std::vector<json> operator()(const Model&,const json&,long);
	json_to_table_function(const std::vector<std::string>& fn):field_names_(fn) {}
    };

    template<typename T>
    class UnitTableFunction: public TableFunction<T>
    {
	std::string field_name;
	std::shared_ptr<std::function<T (const Model&,const json&,long)>> F;
    public:
	virtual UnitTableFunction<T>* clone() const {return new UnitTableFunction<T>(*this);}

	int n_fields() const {return 1;}
	std::vector<std::string> field_names() const {return std::vector<std::string>(1,field_name);}

	std::vector<T> operator()(const Model& M, const json& jlog, long t)
	    {
		T output = (*F)(M,jlog,t);
		return std::vector<T>(1,output);
	    }

	UnitTableFunction(const std::string& name, const std::function<T(const Model&,const json&,long)>& f)
	    :field_name(name),F(std::make_shared<std::function<T (const Model&,const json&,long)>>(f))
	    { }
    };

    class SortedTableFunction: public TableFunction<json>
    {
	owned_ptr<TableFunction<json> > F;
	std::vector< std::vector< std::vector< int > > > indices;
	std::vector<int> sorted_index;
    public:
	SortedTableFunction* clone() const {return new SortedTableFunction(*this);}

	std::vector<std::string> field_names() const;

	std::vector<json> operator()(const Model& M, const json& j, long t);

	SortedTableFunction(const TableFunction<json>&, const std::vector< std::vector< std::vector< int> > >& );
    };

    template <typename T>
    class TableGroupFunction: public TableFunction<T>
    {
	std::vector<owned_ptr<TableFunction<T> > > functions;
    public:
	TableGroupFunction* clone() const {return new TableGroupFunction(*this);}

	int n_fields() const
	    {
		int total = 0;

		for(int i=0;i<functions.size();i++)
		    total += functions[i]->n_fields();
      
		return total;
	    }

	std::vector<std::string> field_names() const
	    {
		std::vector<std::string> names;
      
		for(int i=0;i<functions.size();i++)
		{
		    std::vector<std::string> names_i = functions[i]->field_names();
		    names.insert(names.end(),names_i.begin(), names_i.end());
		}
      
		return names;
	    }

	void add_fields(const TableFunction<T>& f)
	    {
		functions.push_back(f);
	    }

	void add_field(const std::string& name, const std::function<T(const Model&,const json& jlog,long)>& f)
	    {
		functions.push_back(UnitTableFunction<T>(name,f));
	    }

	std::vector<T> operator()(const Model& M, const json& jlog, long t)
	    {
		std::vector<T> values;
      
		for(int i=0;i<functions.size();i++)
		{
		    std::vector<T> values_i = (*functions[i])(M,jlog,t);
		    values.insert(values.end(),values_i.begin(), values_i.end());
		}
      
		return values;
	    }
    };

    template <typename T>
    class ConvertTableToStringFunction: public TableFunction<std::string>
    {
	owned_ptr<TableFunction<T> > F;
    public:
	ConvertTableToStringFunction<T>* clone() const {return new ConvertTableToStringFunction<T>(*this);}

	std::vector<std::string> field_names() const {return F->field_names();}

	std::vector<std::string> operator()(const Model& M, const json& jlog, long t)
	    {
		auto values = (*F)(M,jlog,t);

		std::vector<std::string> output(values.size());
		for(int i=0;i<values.size();i++)
		    output[i] = convertToString<T>(values[i]);

		return output;
	    }

	ConvertTableToStringFunction(const TableFunction<T>& f):F(f) { }
    };

    template <typename T>
    class TableLogger
    {
	owned_ptr<TableFunction<T> > TF;
	char separator;

    public:
	TableLogger* clone() const {return new TableLogger<T>(*this);}

	std::string operator()(const Model& M, const json& jlog, long t)
	    {
		using namespace std;
		ostringstream output;
      
		if (t==0)
		    output<<join(TF->field_names(),'\t')<<endl;
      
		vector<T> values = (*TF)(M,jlog,t);
		output<<join(values,separator);
		return output.str();
	    }

	TableLogger(const TableFunction<T>& tf, char sep='\t')
	    :TF(tf),separator(sep)
	    { }
    };

    class Subsample_Function
    {
	std::shared_ptr<logger_function<std::string> > function;
	int subsample;
    public:
	std::string operator()(const Model&, const json&, long t);
	Subsample_Function(const logger_function<std::string>& f, int i)
	    :function(std::make_shared<logger_function<std::string>>(f)),subsample(i) {}
    };

    class Mixture_Components_Function
    {
	int p;
    public:
	std::string operator()(const Model& M, const json& j, long t);
	Mixture_Components_Function(int i):p(i) {}
    };

    class Ancestral_Sequences_Function
    {
	int p;

	// Do we want to overwrite ambiguous letters like W in observed sequences?
	bool infer_ambiguous_observed;
    public:
	std::string operator()(const Model& M, const json& j, long t);

	Ancestral_Sequences_Function(int i, bool b)
	    :p(i),infer_ambiguous_observed(b)
	    {}
    };

    class String_Function
    {
	std::string s;
    public:
	std::string operator()(const Model&, const json&, long) {return s;}
	String_Function(const std::string& s_):s(s_) {}
    };

    class TableViewerFunction
    {
	owned_ptr<TableFunction<std::string> > function;
    public:
	std::string operator()(const Model& M, const json& j, long t);
	TableViewerFunction(const TableFunction<std::string>&);
    };

    template <typename F>
    class append_line_to_file
    {
	std::shared_ptr<std::ostream> log_file;
	F function;
    public:
	void operator()(const Model& M, const json& jlog, long t)
        {
            (*log_file)<<function(M,jlog,t)<<'\n';
            log_file->flush();
        }

	append_line_to_file(const std::filesystem::path& filename, const F& f)
            :log_file(new checked_ofstream(filename,false)), function(f)
            {}
    };

    template <typename F>
    class append_to_file
    {
	std::shared_ptr<std::ostream> log_file;
	F function;
    public:
	void operator()(const Model& M, const json& jlog, long t)
        {
            (*log_file)<<function(M,jlog,t);
            log_file->flush();
        }

	append_to_file(const std::filesystem::path& filename, const F& f)
            :log_file(new checked_ofstream(filename,false)), function(f)
            {}
    };

    class ConcatFunction
    {
	std::vector<logger_function<std::string>> functions;
	std::string separator;
    public:
	void add_function(const logger_function<std::string>& func)
	{
	    functions.push_back(func);
	}
	std::string operator()(const Model& M, const json& j, long t);
	ConcatFunction() {}
	ConcatFunction(const std::string& s):separator(s) {}
    };

    ConcatFunction& operator<<(ConcatFunction&,const logger_function<std::string>&);

    ConcatFunction& operator<<(ConcatFunction&,const std::string& s);

    ConcatFunction operator<<(const ConcatFunction&,const logger_function<std::string>&);

    ConcatFunction operator<<(const ConcatFunction&,const std::string& s);

    ConcatFunction operator<<(const logger_function<std::string>&,const logger_function<std::string>&);

    ConcatFunction operator<<(const logger_function<std::string>&,const std::string& s);
}

#endif
