/*
   Copyright (C) 2004-2006,2008-2009 Benjamin Redelings

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

//FIXME Can I fix PSRF-80% by finding the SMALLEST 80%-wide interval?
// * well, no, because a much wider 80.001%-interval would still cause a problem.
// Also report Lartillot's (mu1-m2)/(s1+s2) or whatver it was.

#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <cmath>

#include "util.H"
#include "statistics.H"
#include "stats-table.H"
#include "math/log-double.H"

#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

using boost::dynamic_bitset;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("filenames", value<vector<string> >()->composing(),"Filenames to analyze ('-' for STDIN')")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help,h", "Produce help message.")
    ("ignore", value<vector<string> >()->composing(),"Do not analyze these fields.")
    ("select", value<vector<string> >()->composing(),"Analyze only these fields.")
    ("individual,i","Show results for individual files separately also.")
    ("skip,s",value<string>()->default_value("10%"),"Number of initial lines to skip.")
    ("sub-sample,x",value<int>()->default_value(1),"Factor by which to sub-sample.")
    ("max,m",value<int>(),"Maximum number of lines to read.")
    ("mean", "Show mean and standard deviation.")
    ("log-mean", "Show log mean of X given log X.")
    ("median", "Show median and confidence level.")
    ("confidence",value<double>()->default_value(0.95,"0.95"),"Confidence interval level.")
    ("HPD", "Show HPD credible intervals")
    ("precision,p", value<unsigned>()->default_value(4),"Number of significant figures.")
    ("verbose,v","Output more log messages on stderr.")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("filenames", -1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);

  if (args.count("help")) {
    cout<<"Usage: statreport [OPTIONS] file1 [file2 file3 ... ] \n";
    cout<<"Compute summary statistics for tab-delimited data files.\n\n";
    cout<<visible<<"\n";
    cout<<"Default: Report the median and 95% credible interval for each column.\n\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}

bool constant(const vector<double>& values)
{
  for(int i=1;i<values.size();i++)
    if (values[i] != values[0])
      return false;
  return true;
}

bool monotonic_increasing(const vector<double>& values)
{
  for(int i=1;i<values.size();i++)
    if (values[i] < values[i-1])
      return false;

  return true;
}

bool monotonic_decreasing(const vector<double>& values)
{
  for(int i=1;i<values.size();i++)
    if (values[i] > values[i-1])
      return false;
  return true;
}

bool is_integers(const vector<double>& values)
{
  for(int i=0;i<values.size();i++)
  {
    double x = values[i];
    double temp = 0;
    double frac = modf(x,&temp);
    if (std::abs(frac) > 1.0e-9) return false;
  }
  return true;
}

struct var_stats
{
  double Ne;
  double RCI;
  double RNe;
  double RCF;
  bool ignored;
  bool increasing;
  bool decreasing;
  bool constant;
  var_stats()
    :ignored(true), increasing(false),decreasing(false),constant(false)
  {}

  var_stats(double a, double b, double c, double d)
    :Ne(a), RCI(b), RNe(c), RCF(d), ignored(false), increasing(false),decreasing(false),constant(false)
  {}
};



int time_to_cross_above(const vector<double>& data, int start, double x)
{
  for(int i=start+1;i<data.size();i++)
  {
    if (/*data[i-1] < x and */ data[i] >= x)
      return i;
  }
  // never occurs
  return data.size();
}

int time_to_cross_below(const vector<double>& data, int start, double x)
{
  for(int i=start+1;i<data.size();i++)
  {
    if (/*data[i-1] < x and */ data[i] <= x)
      return i;
  }
  // never occurs
  return data.size();
}

int time_to_cross(const vector<double>& data, int start, double x1,double x2,int direction)
{
  if (direction == 1)
    return time_to_cross_above(data,start,x2);
  else if (direction == 0)
    return time_to_cross_below(data,start,x1);
  else
    std::abort();
}

int get_burn_in(const vector<double>& data, double alpha,int n)
{
  using namespace statistics;

  if (constant(data)) return 1;

  if (monotonic_increasing(data)) return 1;

  if (monotonic_decreasing(data)) return 1;

  /// construct the sample representing the equilibrium
  vector<double> equilibrium;

  for(int i=data.size()*2/3;i<data.size();i++)
    equilibrium.push_back(data[i]);

  std::sort(equilibrium.begin(), equilibrium.end());

  double x1 = quantile_sorted(equilibrium, alpha);
  double x2 = quantile_sorted(equilibrium,0.5);
  double x3 = quantile_sorted(equilibrium, 1.0 - alpha);

  int t = 1;

  int direction = 0;
  if (data[t] < x2)
    direction = 1;

  for(;n>0;n--) {
    t = time_to_cross(data,t,x1,x3,direction);
    t = time_to_cross(data,t,x1,x3,!direction);
  }
  return t;
}

string burnin_value(int b,unsigned total)
{
  if (b < total*2/3)
    return convertToString(b);
  else 
    return "Not Converged!";
}

bool monotonic_increasing(const vector<stats_table>& tables, int index)
{
  bool increasing = true;
  for(int i=0;i<tables.size();i++)
    if (not monotonic_increasing(tables[i].column(index)))
	increasing = false;
  return increasing;
}

bool monotonic_decreasing(const vector<stats_table>& tables, int index)
{
  bool increasing = true;
  for(int i=0;i<tables.size();i++)
    if (not monotonic_decreasing(tables[i].column(index)))
	increasing = false;
  return increasing;
}


double log_average_exp(const vector<double>& xs)
{
  log_double_t total = 0;
  for(double x:xs)
  {
    total += exp<log_double_t>(x);
    //    std::cerr<<"x = "<<x<<"  total = "<<total<<"\n";
  }
  total /= double(xs.size());
  return log(total);
}

void show_mean(const string& name, const vector<stats_table>& tables, int index, const vector<double>& total, bool show_individual)
{
  using namespace statistics;

  if (tables.size() > 1 and show_individual)
      for(int i=0;i<tables.size();i++) 
      {
	const vector<double>& values = tables[i].column(index);
	cout<<" E "<<name<<" ["<<i+1<<"] = "<<average(values);
	cout<<"  [+- "<<sqrt(Var(values))<<"]"<<endl;
      }

    const vector<double>& values = total;
    if (show_individual)
      cout<<" E "<<name<<"     = "<<average(values);
    else
      cout<<" E "<<name<<" = "<<average(values);
    cout<<"  [+- "<<sqrt(Var(values))<<"]"<<endl;
}

var_stats show_stats(variables_map& args, const vector<stats_table>& tables,int index,const vector<vector<int> >& burnin, bool HPD)
{
  const string& name = tables[0].names()[index];

  using namespace statistics;

  double compare_level=0.8;

  bool show_individual = (args.count("individual")>0) and (tables.size() >1);

  vector<double> total;
  for(int i=0;i<tables.size();i++)
    total.insert(total.end(),tables[i].column(index).begin(),tables[i].column(index).end());

  if (constant(total)) {
    cout<<"   "<<name<<" = "<<total[0]<<endl;
    // hack to not print out ignored CONSTANTs
    var_stats V;
    V.constant = true;
    return V;
  }

  if (monotonic_increasing(tables,index))
  {
    cout<<"   "<<name<<" = [increasing]"<<endl;
    var_stats V;
    V.increasing = true;
    return V;
  }

  if (monotonic_decreasing(tables,index))
  {
    cout<<"   "<<name<<" = [decreasing]"<<endl;
    var_stats V;
    V.decreasing = true;
    return V;
  }

  bool integers = is_integers(total);

  // Print out mean and standard deviation
  if (args.count("mean"))
    show_mean(name, tables, index, total, show_individual);

  // Print out log(E(exp(X)))
  if (args.count("log-mean"))
  {
    if (tables.size() > 1 and show_individual)
      for(int i=0;i<tables.size();i++)
      {
	const vector<double>& values = tables[i].column(index);
	cout<<" log E exp "<<name<<" ["<<i+1<<"] = "<<log_average_exp(values);
      }

    const vector<double>& values = total;
    if (show_individual)
      cout<<" log E exp "<<name<<"     = "<<log_average_exp(values);
    else
      cout<<" log E exp "<<name<<" = "<<log_average_exp(values);
  }


  // Print out median and confidence interval
  double sum_CI=0;
  double total_CI=0;
  double sum_fraction_contained=0;
  if (args.count("median") or not args.count("mean")) {
    double P = args["confidence"].as<double>();

    if (tables.size() > 1)
      for(int i=0;i<tables.size();i++) {
	const vector<double>& values = tables[i].column(index);
    
	if (tables.size() > 1)
	{
	  pair<double,double> interval_80 = central_confidence_interval(values,compare_level);
	  double x = fraction_in_interval(values,interval_80.first,interval_80.second)/
	    fraction_in_interval(tables.back().column(index),interval_80.first,interval_80.second);

	  sum_fraction_contained += x;
	  sum_CI += std::abs(interval_80.second - interval_80.first);
	}
	pair<double,double> interval = HPD?HPD_confidence_interval(values,P):central_confidence_interval(values,P);
	if (show_individual) {
	  cout<<"   "<<name<<" ["<<i+1<<"] ~ "<<median(values);

	  if ((1.0-P)*values.size() >= 10.0)
	    cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
	  else
	    cout<<"  (NA,NA)"<<endl;
	}
      }
    const vector<double>& values = total;
    
    if (tables.size() > 1)
    {
      pair<double,double> interval_compare = central_confidence_interval(values,compare_level);
      total_CI = std::abs(interval_compare.second - interval_compare.first);
      sum_CI /= tables.size();
      sum_fraction_contained /= tables.size();
    }
    pair<double,double> interval = HPD?HPD_confidence_interval(values,P):central_confidence_interval(values,P);
    if (show_individual)
      cout<<"   "<<name<<"     ~ "<<median(values);
    else
      cout<<"   "<<name<<" ~ "<<median(values);
    
    if ((1.0-P)*values.size() >= 10.0)
      cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
    else
      cout<<"  (NA,NA)"<<endl;
  }

  // Print out autocorrelation times, Ne, and minimum burn-in
  double sum_tau=0;
  index_value<int> worst_burnin;
  if (tables.size() > 1)
    for(int i=0;i<tables.size();i++) {
      const vector<double>& values = tables[i].column(index);

      double tau = autocorrelation_time(values);
      sum_tau += tau;

      int b = burnin[i][index];

      string spacer;spacer.append(name.size()-1,' ');

      if (show_individual) {
	cout<<"   "<<spacer<<"t @ "<<tau;
	cout<<"   Ne = "<<int(values.size()/tau);
	cout<<"   burnin = "<<burnin_value(b,values.size())<<endl;
      }
      worst_burnin.check_max(i,b);
    }
  const vector<double>& values = total;
  double tau = autocorrelation_time(values);

  string spacer;spacer.append(name.size()-1,' ');

  cout<<"   "<<spacer<<"t @ "<<tau;
  double Ne = values.size()/tau;
  cout<<"   Ne = "<<int(Ne);
  int individual_size_worst = values.size();
  if (tables.size() == 1)
    worst_burnin.value = burnin[0][index];
  else
    individual_size_worst = tables[worst_burnin.index].column(index).size();
  cout<<"   burnin = "<<burnin_value(worst_burnin.value,individual_size_worst);
  if (integers) cout<<"   [integer] ";
  cout<<endl;

  // Print out Potential Scale Reduction Factors (PSRFs)
  double RNe = 1;
  double RCI = 1;
  double RCF = 1;
  if (tables.size() > 1) {
    RNe = tau/sum_tau*tables.size();
    //cout<<"   PSRF-Ne = "<<RNe;
    if (sum_CI > 0)
    {
      if (integers)
	RCI = std::max(0.0,total_CI-1)/sum_CI;
      else
	RCI = total_CI/sum_CI;
    }
    cout<<"       PSRF-80%CI = "<<RCI;
    RCF = sum_fraction_contained; //compare_level;
    cout<<"       PSRF-RCF = "<<RCF<<endl;
  }

  cout<<endl;
  return var_stats(Ne,RCI,RNe,RCF);
}

// stats-table can't distinguish double && int

/// FIXME - reduce the numbers of quantile/median/confidence_interval calls?
///       - 543 calls to median for 42*6: 
///       - (Remember that SOME of the calls are only sorting the last THIRD of the data.)

//  FIXME - use scan_lines and an accumulator to read the data?

/// Why is the autocorrelation taking so long?  Can we speed it up for the combined runs anyhow?

int main(int argc,char* argv[]) 
{ 
  std::ios::sync_with_stdio(false);

  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    cout.precision(args["precision"].as<unsigned>());

    bool HPD = args.count("HPD");

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    vector<string> ignore;
    if (args.count("ignore"))
      ignore = args["ignore"].as<vector<string> >();

    vector<string> select;
    if (args.count("select"))
      select = args["select"].as<vector<string> >();

    //------------ Read Data ---------------//
    vector<stats_table> tables;
    vector<string> filenames;

    if (not args.count("filenames"))
      throw myexception()<<"No filenames specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

    filenames = args["filenames"].as< vector<string> >();
    for(int i=0;i<filenames.size();i++) {
      if (filenames[i] == "-")
	tables.push_back(stats_table(std::cin,0,subsample,max,ignore,select));
      else
	tables.push_back(stats_table(filenames[i],0,subsample,max,ignore,select));
      if (not tables.back().n_rows())
	throw myexception()<<"File '"<<filenames[i]<<"' has no samples left after removal of burn-in!";
    }

    if (tables.size() < 1)
      throw myexception()<<"No tables read in!";

    const vector<string> field_names = tables[0].names();

    int min_table_rows = tables[0].n_rows();
    for(int i=0;i<tables.size();i++) 
    {
      min_table_rows = std::min(min_table_rows, tables[i].n_rows());

      if (tables[i].names() != field_names)
	throw myexception()<<filenames[i]<<": Column names differ from names in '"<<filenames[0]<<"'";
      if (tables[i].n_rows() == 0)
	throw myexception()<<filenames[i]<<": No data line read in!";
    }
    int n_columns = tables[0].n_columns();

    //------------- Determine burnin ---------------//
    int skip = 0;
    {
      string s = args["skip"].as<string>();
      if (can_be_converted_to<int>(s,skip))
	skip /= subsample; //FIXME!  This is a side-effect of passing in 0 below.
      else 
      {
	if (not s.size() or s[s.size()-1] != '%')
	  throw myexception()<<"Argument to --skip="<<s<<" is neither an integer nor a percent";

	double f = convertTo<double>(s.substr(0,s.size()-1))/100;
	skip = (int)(f*min_table_rows);
	if (log_verbose)
	  cerr<<"Skipping "<<s<<" of "<<min_table_rows<<" = "<<skip<<endl;
      }
    }
    // FIXME - for the fraction, make a skip-a-fraction reader?

    //------------ Handle Burnin ------------//
    vector< vector<int> > burnin(tables.size(), vector<int>(n_columns,1));

    index_value<int>    worst_burnin(1); 

    for(int i=0;i<tables.size();i++) {
      for(int j=0;j<n_columns;j++) 
      {
	int b = get_burn_in(tables[i].column(j), 0.05, 2);
	burnin[i][j] = b;
	worst_burnin.check_max(j,b);
      }
      tables[i].chop_first_rows(skip);
      if (not tables[i].n_rows())
	throw myexception()<<"File '"<<filenames[i]<<"' has no samples left after removal of burn-in!";
    }

    
    //------------ Generate Report ----------//
    index_value<double> worst_Ne;
    index_value<double> worst_RCI;
    index_value<double> worst_RNe;
    index_value<double> worst_RCF;

    vector<string> increasing_names;
    vector<string> decreasing_names;
    for(int i=0;i<n_columns;i++) 
    {
      var_stats S = show_stats(args, tables, i, burnin, HPD);
      cout<<endl;

      if (not S.ignored) {
	worst_Ne.check_min(i,S.Ne);
	worst_RCI.check_max(i,S.RCI);
	worst_RNe.check_max(i,S.RNe);
	worst_RCF.check_max(i,S.RCF);
      }
      else if (S.increasing)
	increasing_names.push_back(field_names[i]);
      else if (S.decreasing)
	decreasing_names.push_back(field_names[i]);
    }

    if (worst_Ne.index != -1)
      cout<<" Ne  >= "<<worst_Ne.value<<"    ("<<field_names[worst_Ne.index]<<")"<<endl;
    if (worst_burnin.index != -1)
      cout<<" min burnin <= "<<burnin_value(worst_burnin.value,tables.back().n_rows())<<"    ("<<field_names[worst_burnin.index]<<")"<<endl;
    if (tables.size() > 1) {
      if (worst_RCI.index != -1)
	cout<<" PSRF-80%CI <= "<<worst_RCI.value<<"    ("<<field_names[worst_RCI.index]<<")"<<endl;
      //if (worst_RNe.index != -1)
      //cout<<" PSRF-Ne <= "<<worst_RNe.value<<"    ("<<field_names[worst_RNe.index]<<")"<<endl;
      if (worst_RCF.index != -1)
	cout<<" PSRF-RCF <= "<<worst_RCF.value<<"    ("<<field_names[worst_RCF.index]<<")"<<endl;
    }
    if (increasing_names.size())
      cout<<"\nIncreasing: "<<join(increasing_names,' ')<<endl;
    if (decreasing_names.size())
      cout<<"\nDecreasing: "<<join(decreasing_names,' ')<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"statreport: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


