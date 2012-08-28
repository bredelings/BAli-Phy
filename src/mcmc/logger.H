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
#include <boost/shared_ptr.hpp>

#include "parameters.H"
#include "io.H"
#include "owned-ptr.H"


struct slice_function;

namespace MCMC {

  struct Logger
  {
    virtual Logger* clone() const =0;
    virtual void operator()(const owned_ptr<Probability_Model>& P,long t)=0;
    virtual ~Logger() {}
  };

  class FileLogger : public Logger
  {
  protected:
    boost::shared_ptr<std::ostream> log_file;

  public:
    FileLogger* clone() const =0;
    FileLogger(const std::string&);
    FileLogger(const std::ostream&);
  };

  template <typename T>
  struct LoggerFunction
  {
    virtual LoggerFunction<T>* clone() const =0;
    virtual T operator()(const owned_ptr<Probability_Model>& P,long t)=0;
    virtual ~LoggerFunction() {}
  };

  template <typename T>
  class FunctionList
  {
  protected:
    std::vector<owned_ptr<LoggerFunction<T> > > functions;
  public:
    void add_function(const owned_ptr<LoggerFunction<T> >&F)
    {
      functions.push_back(F);
    }
  };

  struct IterationsFunction: public LoggerFunction<long>
  {
    IterationsFunction* clone() const {return new IterationsFunction(*this);}
    long operator()(const owned_ptr<Probability_Model>&,long t) {return t;}
  };

  class GetParameterFunction: public LoggerFunction<double>
  {
    int p;
  public:
    GetParameterFunction* clone() const {return new GetParameterFunction(*this);}
    double operator()(const owned_ptr<Probability_Model>& P,long t);
    GetParameterFunction(int i):p(i) {}
  };

  template <typename T>
  class ConvertToStringFunction: public LoggerFunction<std::string>
  {
    owned_ptr<LoggerFunction<T> > F;
  public:
    ConvertToStringFunction* clone() const {return new ConvertToStringFunction(*this);}

    std::string operator()(const owned_ptr<Probability_Model>& P,long t)
    {
      return convertToString<T>((*F)(P,t));
    }

    ConvertToStringFunction(const owned_ptr<LoggerFunction<T> >& f)
      :F(f)
    { }
  };

  struct GetPriorFunction: public LoggerFunction<std::string>
  {
    GetPriorFunction* clone() const {return new GetPriorFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    GetPriorFunction() {}
  };

  class GetAlignmentPriorFunction: public LoggerFunction<std::string>
  {
    int p;
  public:
    GetAlignmentPriorFunction* clone() const {return new GetAlignmentPriorFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    GetAlignmentPriorFunction(int i):p(i) {}
  };

  struct GetLikelihoodFunction: public LoggerFunction<std::string>
  {
    GetLikelihoodFunction* clone() const {return new GetLikelihoodFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    GetLikelihoodFunction() {}
  };

  struct GetProbabilityFunction: public LoggerFunction<std::string>
  {
    GetProbabilityFunction* clone() const {return new GetProbabilityFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    GetProbabilityFunction() {}
  };

  struct Get_Alignment_Length_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Get_Alignment_Length_Function* clone() const {return new Get_Alignment_Length_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Get_Alignment_Length_Function(int i):p(i) {}
  };

  struct Get_Num_Substitutions_Function: public LoggerFunction<std::string>
  {
    int p;
    ublas::matrix<int> cost_matrix;
  public:
    Get_Num_Substitutions_Function* clone() const {return new Get_Num_Substitutions_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Get_Num_Substitutions_Function(int i, const ublas::matrix<int>& C):p(i), cost_matrix(C) {}
  };

  struct Get_Num_Indels_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Get_Num_Indels_Function* clone() const {return new Get_Num_Indels_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Get_Num_Indels_Function(int i):p(i) {}
  };

  struct Get_Total_Length_Indels_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Get_Total_Length_Indels_Function* clone() const {return new Get_Total_Length_Indels_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Get_Total_Length_Indels_Function(int i):p(i) {}
  };

  struct Get_Total_Alignment_Length_Function: public LoggerFunction<std::string>
  {
  public:
    Get_Total_Alignment_Length_Function* clone() const {return new Get_Total_Alignment_Length_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  struct Get_Total_Num_Substitutions_Function: public LoggerFunction<std::string>
  {
  public:
    Get_Total_Num_Substitutions_Function* clone() const {return new Get_Total_Num_Substitutions_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  struct Get_Total_Num_Indels_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Get_Total_Num_Indels_Function* clone() const {return new Get_Total_Num_Indels_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  struct Get_Total_Total_Length_Indels_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Get_Total_Total_Length_Indels_Function* clone() const {return new Get_Total_Total_Length_Indels_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  struct Get_Rao_Blackwellized_Parameter_Function: public LoggerFunction<std::string>
  {
    int parameter;
    std::vector<object_ref> values;

    Get_Rao_Blackwellized_Parameter_Function* clone() const {return new Get_Rao_Blackwellized_Parameter_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Get_Rao_Blackwellized_Parameter_Function(int p, const std::vector<object_ref>& v);
  };

  struct Get_Tree_Length_Function: public LoggerFunction<std::string>
  {
    Get_Tree_Length_Function* clone() const {return new Get_Tree_Length_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  struct TreeFunction: public LoggerFunction<std::string>
  {
    TreeFunction* clone() const {return new TreeFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  class MAP_Function: public LoggerFunction<std::string>
  {
    efloat_t MAP_score;
    owned_ptr<LoggerFunction<std::string> > F;
  public:
    MAP_Function* clone() const {return new MAP_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    MAP_Function(const owned_ptr<LoggerFunction<std::string> >& f):MAP_score(0),F(f) { }
  };

  class AlignmentFunction: public LoggerFunction<std::string>
  {
    int p;
  public:
    AlignmentFunction* clone() const {return new AlignmentFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    AlignmentFunction(int i):p(i) {}
  };

  template <typename T>
  class TableFunction: public LoggerFunction<std::vector<T> >
  {
  public:
    virtual TableFunction* clone() const =0;
    virtual int n_fields() const =0;
    virtual std::vector<std::string> field_names() const =0;
  };

  template<typename T>
  class UnitTableFunction: public TableFunction<T>
  {
    std::string field_name;
    owned_ptr<LoggerFunction<T> > F;
  public:
    virtual UnitTableFunction<T>* clone() const {return new UnitTableFunction<T>(*this);}

    int n_fields() const {return 1;}
    std::vector<std::string> field_names() const {return std::vector<std::string>(1,field_name);}

    std::vector<T> operator()(const owned_ptr<Probability_Model>& P, long t)
    {
      T output = (*F)(P,t);
      return std::vector<T>(1,output);
    }

    UnitTableFunction(const std::string& name, const owned_ptr<LoggerFunction<T> >& f)
      :field_name(name),F(f)
    { }
  };

  class SortedTableFunction: public TableFunction<double>
  {
    owned_ptr<TableFunction<double> > F;
    std::vector< std::vector< std::vector< int > > > indices;
    std::vector<int> sorted_index;
  public:
    SortedTableFunction* clone() const {return new SortedTableFunction(*this);}

    int n_fields() const;
    std::vector<std::string> field_names() const;

    std::vector<double> operator()(const owned_ptr<Probability_Model>& P, long t);

    SortedTableFunction(const owned_ptr<TableFunction<double> >&, const std::vector< std::vector< std::vector< int> > >& );
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

    void add_fields(const owned_ptr<TableFunction<T> >& f)
    {
      functions.push_back(f);
    }

    void add_field(const std::string& name, const owned_ptr<LoggerFunction<T> >& f)
    {
      functions.push_back(UnitTableFunction<T>(name,f));
    }

    std::vector<T> operator()(const owned_ptr<Probability_Model>& P, long t)
    {
      std::vector<T> values;
      
      for(int i=0;i<functions.size();i++)
	{
	  std::vector<T> values_i = (*functions[i])(P,t);
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

    std::vector<std::string> operator()(const owned_ptr<Probability_Model>& P, long t)
    {
      std::vector<T> values = (*F)(P,t);

      std::vector<std::string> output(values.size());
      for(int i=0;i<values.size();i++)
	output[i] = convertToString<T>(values[i]);

      return output;
    }

    ConvertTableToStringFunction(const owned_ptr<TableFunction<T> >& f):F(f) { }
  };

  class TableLogger: public FileLogger
  {
    owned_ptr<TableFunction<std::string> > TF;

  public:
    TableLogger* clone() const {return new TableLogger(*this);}

    int n_fields() const;

    std::vector<std::string> field_names() const;

    void operator()(const owned_ptr<Probability_Model>& P, long t);

    TableLogger(const std::string& filename, const owned_ptr<TableFunction<std::string> >& tf);
  };

  class Show_SModels_Function: public LoggerFunction<std::string>
  {
  public:
    Show_SModels_Function* clone() const {return new Show_SModels_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
  };

  class Subsample_Function: public LoggerFunction<std::string>
  {
    owned_ptr<LoggerFunction<std::string> > function;
    int subsample;
  public:
    Subsample_Function* clone() const {return new Subsample_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>&, long t);
    Subsample_Function(const owned_ptr<LoggerFunction<std::string> >& f, int i)
      :function(f),subsample(i) {}
  };

  class Mixture_Components_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Mixture_Components_Function* clone() const {return new Mixture_Components_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Mixture_Components_Function(int i):p(i) {}
  };

  class Ancestral_Sequences_Function: public LoggerFunction<std::string>
  {
    int p;
  public:
    Ancestral_Sequences_Function* clone() const {return new Ancestral_Sequences_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    Ancestral_Sequences_Function(int i):p(i) {}
  };

  class String_Function: public LoggerFunction<std::string>
  {
    std::string s;
  public:
    String_Function* clone() const {return new String_Function(*this);}
    std::string operator()(const owned_ptr<Probability_Model>&, long) {return s;}
    String_Function(const std::string& s_):s(s_) {}
  };

  class TableViewerFunction: public LoggerFunction<std::string>
  {
    owned_ptr<TableFunction<std::string> > function;
  public:
    TableViewerFunction* clone() const {return new TableViewerFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    TableViewerFunction(const owned_ptr<TableFunction<std::string> >&);
  };

  class FunctionLogger: public FileLogger
  {
    owned_ptr<LoggerFunction<std::string> > function;
  public:
    FunctionLogger* clone() const {return new FunctionLogger(*this);}
    void operator()(const owned_ptr<Probability_Model>& P, long t);
    FunctionLogger(const std::string& filename, const owned_ptr<LoggerFunction<std::string> >& L);
  };

  class ConcatFunction: public LoggerFunction<std::string>, public FunctionList<std::string>
  {
    std::string separator;
  public:
    ConcatFunction* clone() const {return new ConcatFunction(*this);}
    std::string operator()(const owned_ptr<Probability_Model>& P, long t);
    ConcatFunction() {}
    ConcatFunction(const std::string& s):separator(s) {}
  };

  ConcatFunction& operator<<(ConcatFunction&,const owned_ptr<LoggerFunction<std::string> >&);

  ConcatFunction& operator<<(ConcatFunction&,const std::string& s);

  ConcatFunction operator<<(const ConcatFunction&,const owned_ptr<LoggerFunction<std::string> >&);

  ConcatFunction operator<<(const ConcatFunction&,const std::string& s);

  ConcatFunction operator<<(const LoggerFunction<std::string>&,const owned_ptr<LoggerFunction<std::string> >&);

  ConcatFunction operator<<(const LoggerFunction<std::string>&,const std::string& s);
}

#endif
