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

#include "models/parameters.H"
#include "io.H"
#include "owned-ptr.H"
#include "object.H"

struct slice_function;

namespace MCMC {

    typedef std::function<void(const Model& M, long)> Logger;

    // std::function<T(const Model& M, t)>
    template <typename T>
    struct LoggerFunction
    {
	virtual LoggerFunction<T>* clone() const =0;
	virtual T operator()(const Model& P,long t)=0;
	virtual ~LoggerFunction() {}
    };


    template <typename T>
    using logger_function = std::function<T (const Model&, long)>;

    template <typename T>
    logger_function<T> make_logger_function(const LoggerFunction<T>& F)
    {
	return [F2 = owned_ptr<LoggerFunction<T>>(F)](const Model& M,long t) mutable {return (*F2)(M,t);};
    }

    // std::vector<std::function<T(const Model& M, t>>
    template <typename T>
    class FunctionList
    {
    protected:
	std::vector<std::shared_ptr<std::function<T (const Model&,long)>>> functions;
    public:
	void add_function(const LoggerFunction<T>&F)
	    {
		auto func = make_logger_function(F);
		functions.push_back(std::make_shared<std::function<T(const Model&,long)>>(func));
	    }

	void add_function(const std::function<T(const Model&,long)>& func)
	    {
		functions.push_back(std::make_shared<std::function<T(const Model&,long)>>(func));
	    }
    };

    class GetComputationFunction
    {
	int index;
    public:
	GetComputationFunction* clone() const {return new GetComputationFunction(*this);}
	expression_ref operator()(const Model& M,long t);
	GetComputationFunction(int i);
    };

    struct Get_Alignment_Length_Function
    {
	int p;
    public:
	Get_Alignment_Length_Function* clone() const {return new Get_Alignment_Length_Function(*this);}
	std::string operator()(const Model& M, long t);
	Get_Alignment_Length_Function(int i):p(i) {}
    };

    struct Get_Num_Substitutions_Function
    {
	int p;
	matrix<int> cost_matrix;
    public:
	Get_Num_Substitutions_Function* clone() const {return new Get_Num_Substitutions_Function(*this);}
	std::string operator()(const Model& M, long t);
	Get_Num_Substitutions_Function(int i, const matrix<int>& C):p(i), cost_matrix(C) {}
    };

    struct Get_Num_Indels_Function
    {
	int p;
    public:
	Get_Num_Indels_Function* clone() const {return new Get_Num_Indels_Function(*this);}
	std::string operator()(const Model& M, long t);
	Get_Num_Indels_Function(int i):p(i) {}
    };

    struct Get_Total_Length_Indels_Function
    {
	int p;
    public:
	Get_Total_Length_Indels_Function* clone() const {return new Get_Total_Length_Indels_Function(*this);}
	std::string operator()(const Model& M, long t);
	Get_Total_Length_Indels_Function(int i):p(i) {}
    };

    struct Get_Total_Alignment_Length_Function
    {
    public:
	Get_Total_Alignment_Length_Function* clone() const {return new Get_Total_Alignment_Length_Function(*this);}
	std::string operator()(const Model& M, long t);
    };

    struct Get_Total_Num_Substitutions_Function
    {
    public:
	Get_Total_Num_Substitutions_Function* clone() const {return new Get_Total_Num_Substitutions_Function(*this);}
	std::string operator()(const Model& M, long t);
    };

    struct Get_Total_Num_Indels_Function
    {
	int p;
    public:
	Get_Total_Num_Indels_Function* clone() const {return new Get_Total_Num_Indels_Function(*this);}
	std::string operator()(const Model& M, long t);
    };

    struct Get_Total_Total_Length_Indels_Function
    {
	int p;
    public:
	Get_Total_Total_Length_Indels_Function* clone() const {return new Get_Total_Total_Length_Indels_Function(*this);}
	std::string operator()(const Model& M, long t);
    };

    struct Get_Rao_Blackwellized_Parameter_Function
    {
	int parameter;
	std::vector<expression_ref> values;

	Get_Rao_Blackwellized_Parameter_Function* clone() const {return new Get_Rao_Blackwellized_Parameter_Function(*this);}
	std::string operator()(const Model& M, long t);
	Get_Rao_Blackwellized_Parameter_Function(int p, const std::vector<expression_ref>& v);
    };

    struct Get_Tree_Length_Function
    {
	Get_Tree_Length_Function* clone() const {return new Get_Tree_Length_Function(*this);}
	std::string operator()(const Model& M, long t);
    };

    struct TreeFunction
    {
	TreeFunction* clone() const {return new TreeFunction(*this);}
	std::string operator()(const Model& M, long t);
    };

    class MAP_Function
    {
	log_double_t MAP_score;
	std::shared_ptr<logger_function<std::string> > F;
    public:
	MAP_Function* clone() const {return new MAP_Function(*this);}
	std::string operator()(const Model& M, long t);
	MAP_Function(const logger_function<std::string>& f):MAP_score(0),F(std::make_shared<logger_function<std::string>>(f)) { }
    };

    class AlignmentFunction
    {
	int p;
    public:
	AlignmentFunction* clone() const {return new AlignmentFunction(*this);}
	std::string operator()(const Model& M, long t);
	AlignmentFunction(int i):p(i) {}
    };

    template <typename T>
    class TableFunction
    {
    public:
	virtual TableFunction* clone() const =0;
	virtual int n_fields() const =0;
	virtual std::vector<T> operator()(const Model&,long)=0;
	virtual std::vector<std::string> field_names() const =0;
    };

    template<typename T>
    class UnitTableFunction: public TableFunction<T>
    {
	std::string field_name;
	std::shared_ptr<std::function<T (const Model&,long)>> F;
    public:
	virtual UnitTableFunction<T>* clone() const {return new UnitTableFunction<T>(*this);}

	int n_fields() const {return 1;}
	std::vector<std::string> field_names() const {return std::vector<std::string>(1,field_name);}

	std::vector<T> operator()(const Model& M, long t)
	    {
		T output = (*F)(M,t);
		return std::vector<T>(1,output);
	    }

	UnitTableFunction(const std::string& name, const std::function<T(const Model&,long)>& f)
	    :field_name(name),F(std::make_shared<std::function<T (const Model&,long)>>(f))
	    { }
    };

    class SortedTableFunction: public TableFunction<expression_ref>
    {
	owned_ptr<TableFunction<expression_ref> > F;
	std::vector< std::vector< std::vector< int > > > indices;
	std::vector<int> sorted_index;
    public:
	SortedTableFunction* clone() const {return new SortedTableFunction(*this);}

	int n_fields() const;
	std::vector<std::string> field_names() const;

	std::vector<expression_ref> operator()(const Model& M, long t);

	SortedTableFunction(const TableFunction<expression_ref>&, const std::vector< std::vector< std::vector< int> > >& );
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

	void add_field(const std::string& name, const std::function<T(const Model&,long)>& f)
	    {
		functions.push_back(UnitTableFunction<T>(name,f));
	    }

	void add_field(const std::string& name, const std::function<T(const Parameters&)>& f)
	    {
		auto f2 = [f](const Model& M,long) { return f(dynamic_cast<const Parameters&>(M));};
		add_field(name,f2);
	    }

	std::vector<T> operator()(const Model& M, long t)
	    {
		std::vector<T> values;
      
		for(int i=0;i<functions.size();i++)
		{
		    std::vector<T> values_i = (*functions[i])(M,t);
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

	int n_fields() const {return F->n_fields();}

	std::vector<std::string> field_names() const {return F->field_names();}

	std::vector<std::string> operator()(const Model& M, long t)
	    {
		std::vector<T> values = (*F)(M,t);

		std::vector<std::string> output(values.size());
		for(int i=0;i<values.size();i++)
		    output[i] = convertToString<T>(values[i]);

		return output;
	    }

	ConvertTableToStringFunction(const TableFunction<T>& f):F(f) { }
    };

    template<> 
    inline std::vector<std::string> ConvertTableToStringFunction<expression_ref>::operator()(const Model& M, long t)
    {
	std::vector<expression_ref> values = (*F)(M,t);

	std::vector<std::string> output(values.size());
	for(int i=0;i<values.size();i++)
	    output[i] = values[i].print();
    
	return output;
    }

    template <typename T>
    class TableLogger
    {
	owned_ptr<TableFunction<T> > TF;
	char separator;

    public:
	TableLogger* clone() const {return new TableLogger<T>(*this);}

	std::string operator()(const Model& M, long t)
	    {
		using namespace std;
		ostringstream output;
      
		if (t==0)
		    output<<join(TF->field_names(),'\t')<<endl;
      
		vector<T> values = (*TF)(M,t);
		output<<join(values,separator)<<endl;
		return output.str();
	    }

	TableLogger(const TableFunction<T>& tf, char sep='\t')
	    :TF(tf),separator(sep)
	    { }
    };

    class Show_SModels_Function
    {
    public:
	Show_SModels_Function* clone() const {return new Show_SModels_Function(*this);}
	std::string operator()(const Model& M, long t);
    };

    class Subsample_Function
    {
	std::shared_ptr<logger_function<std::string> > function;
	int subsample;
    public:
	Subsample_Function* clone() const {return new Subsample_Function(*this);}
	std::string operator()(const Model&, long t);
	Subsample_Function(const LoggerFunction<std::string>& f, int i)
	    :function(std::make_shared<logger_function<std::string>>(make_logger_function(f))),subsample(i) {}
	Subsample_Function(const logger_function<std::string>& f, int i)
	    :function(std::make_shared<logger_function<std::string>>(f)),subsample(i) {}
    };

    class Mixture_Components_Function
    {
	int p;
    public:
	Mixture_Components_Function* clone() const {return new Mixture_Components_Function(*this);}
	std::string operator()(const Model& M, long t);
	Mixture_Components_Function(int i):p(i) {}
    };

    class Ancestral_Sequences_Function
    {
	int p;
    public:
	Ancestral_Sequences_Function* clone() const {return new Ancestral_Sequences_Function(*this);}
	std::string operator()(const Model& M, long t);
	Ancestral_Sequences_Function(int i):p(i) {}
    };

    class String_Function
    {
	std::string s;
    public:
	String_Function* clone() const {return new String_Function(*this);}
	std::string operator()(const Model&, long) {return s;}
	String_Function(const std::string& s_):s(s_) {}
    };

    class TableViewerFunction: public LoggerFunction<std::string>
    {
	owned_ptr<TableFunction<std::string> > function;
    public:
	TableViewerFunction* clone() const {return new TableViewerFunction(*this);}
	std::string operator()(const Model& M, long t);
	TableViewerFunction(const TableFunction<std::string>&);
    };

    class FunctionLogger
    {
	std::shared_ptr<std::ostream> log_file;
	std::shared_ptr<logger_function<std::string> > function;
    public:
	void operator()(const Model& M, long t);
	FunctionLogger(const std::string& filename, const logger_function<std::string>& L);
	FunctionLogger(const std::string& filename, const LoggerFunction<std::string>& L);
    };

    class ConcatFunction: public LoggerFunction<std::string>, public FunctionList<std::string>
    {
	std::string separator;
    public:
	ConcatFunction* clone() const {return new ConcatFunction(*this);}
	std::string operator()(const Model& M, long t);
	ConcatFunction() {}
	ConcatFunction(const std::string& s):separator(s) {}
    };

    ConcatFunction& operator<<(ConcatFunction&,const LoggerFunction<std::string>&);

    ConcatFunction& operator<<(ConcatFunction&,const logger_function<std::string>&);

    ConcatFunction& operator<<(ConcatFunction&,const std::string& s);

    ConcatFunction operator<<(const ConcatFunction&,const LoggerFunction<std::string>&);

    ConcatFunction operator<<(const ConcatFunction&,const logger_function<std::string>&);

    ConcatFunction operator<<(const ConcatFunction&,const std::string& s);

    ConcatFunction operator<<(const LoggerFunction<std::string>&,const LoggerFunction<std::string>&);

    ConcatFunction operator<<(const logger_function<std::string>&,const logger_function<std::string>&);

    ConcatFunction operator<<(const LoggerFunction<std::string>&,const std::string& s);

    ConcatFunction operator<<(const logger_function<std::string>&,const std::string& s);
}

#endif
