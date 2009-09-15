#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <cmath>

#include "util.H"
#include "statistics.H"
#include "stats-table.H"

#include <boost/program_options.hpp>

using namespace std;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("filenames", value<vector<string> >()->composing(),"Filenames to analyze (empty for STDIN)")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("ignore", value<vector<string> >()->composing(),"which fields to print")
    ("individual","which fields to print")
    ("skip",value<int>()->default_value(0),"number of trees to skip")
    ("max",value<int>(),"maximum number of trees to read")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence level")
    ("autocorrelation", "Show autocorrelation time and effective sample size.")
    ("confidence",value<double>()->default_value(0.95),"Confidence level")
    ("precision", value<unsigned>()->default_value(4),"Number of significant figures")
    ("verbose","Output more log messages on stderr.")
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
    cout<<"Compute summary statistics for Tracer format (tab-delimited) data files.\n\n";
    cout<<visible<<"\n";
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

struct var_stats
{
  double Ne;
  double RCI;
  double RNe;
  double RCF;
  var_stats(double a, double b, double c, double d)
    :Ne(a), RCI(b), RNe(c), RCF(d)
  {}
};


var_stats show_stats(variables_map& args, const vector<stats_table>& tables,int index)
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
    return var_stats(total.size(),1,1,1);
  }

  // Print out mean and standard deviation
  if (args.count("mean")) {
    if (tables.size() > 1) 
      for(int i=0;i<tables.size();i++) {
	const vector<double>& values = tables[i].column(index);
	if (show_individual) {
	  cout<<" E "<<name<<" ["<<i+1<<"] = "<<average(values);
	  cout<<"  [+- "<<sqrt(Var(values))<<"]"<<endl;
	}
      }

    const vector<double>& values = total;
    if (show_individual)
      cout<<" E "<<name<<"     = "<<average(values);
    else
      cout<<" E "<<name<<" = "<<average(values);
    cout<<"  [+- "<<sqrt(Var(values))<<"]"<<endl;
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
	  pair<double,double> interval_80 = confidence_interval(values,compare_level);
	  double x = fraction_in_interval(values,interval_80.first,interval_80.second)/
	    fraction_in_interval(tables.back().column(index),interval_80.first,interval_80.second);

	  sum_fraction_contained += x;
	  sum_CI += std::abs(interval_80.second - interval_80.first);
	}
	pair<double,double> interval = confidence_interval(values,P);
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
      pair<double,double> interval_compare = confidence_interval(values,compare_level);
      total_CI = std::abs(interval_compare.second - interval_compare.first);
      sum_CI /= tables.size();
      sum_fraction_contained /= tables.size();
    }
    pair<double,double> interval = confidence_interval(values,P);
    if (show_individual)
      cout<<"   "<<name<<"     ~ "<<median(values);
    else
      cout<<"   "<<name<<" ~ "<<median(values);
    
    if ((1.0-P)*values.size() >= 10.0)
      cout<<"  ("<<interval.first<<","<<interval.second<<")"<<endl;
    else
      cout<<"  (NA,NA)"<<endl;
  }

  double sum_tau=0;
  if (tables.size() > 1)
    for(int i=0;i<tables.size();i++) {
      const vector<double>& values = tables[i].column(index);

      double tau = autocorrelation_time(values);
      sum_tau += tau;

      string spacer;spacer.append(name.size()-1,' ');

      if (show_individual) {
	cout<<"   "<<spacer<<"t @ "<<tau;
	cout<<"   Ne = "<<values.size()/tau<<endl;
      }
    }
  const vector<double>& values = total;
  double tau = autocorrelation_time(values);

  string spacer;spacer.append(name.size()-1,' ');

  cout<<"   "<<spacer<<"t @ "<<tau;
  double Ne = values.size()/tau;
  cout<<"   Ne = "<<Ne<<endl;
  double RNe = 1;
  double RCI = 1;
  double RCF = 1;
  if (tables.size() > 1) {
    RNe = tau/sum_tau*tables.size();
    cout<<"   RNe = "<<RNe<<endl;
    RCI = total_CI/sum_CI;
    cout<<"   RCI = "<<RCI<<endl;
    RCF = sum_fraction_contained; //compare_level;
    cout<<"   RCF = "<<RCF<<endl;
  }
  cout<<endl;
  return var_stats(Ne,RCI,RNe,RCF);
}

vector<bool> 
get_mask_by_ignoring(const vector<string>& strings,const vector<string>& names, vector<bool> mask)
{
  for(int i=0;i<strings.size();i++) {
    const string& s = strings[i];

    // This is a field name to ignore
    if (s.find(':') == -1) {
      int index = find_index(names,s);
      if (index == -1)
	throw myexception()<<"No field named '"<<s<<"'";
      mask[index] = false;
    }
    // This is a numeric range of fields to ignore
    else {
      vector<string> bounds = split(s,':');
      if (bounds.size() != 2) 
	throw myexception()<<"Can't understand the column range '"<<s<<"'.";

      unsigned start = 1;
      unsigned end = mask.size();
      if (bounds[0].size())
	start = convertTo<unsigned>(bounds[0]);
      if (bounds[1].size())
	start = convertTo<unsigned>(bounds[1]);
      start = max(1U,start);
      end = min((unsigned)(mask.size()),end);

      for(int i=0;i<mask.size();i++)
	if (start <=i+1 and i+1 <= end)
	  mask[i]=false;
    }
  }

  return mask;
}

template <typename T>
struct index_value
{
  int index;
  T value;
  T check_min(int index2, T value2) 
  {
    if (index == -1 or (value2 < value)) {
      index = index2;
      value = value2;
    }
    return value;
  }
  T check_max(int index2, T value2) 
  {
    if (index == -1 or (value2 > value)) {
      index = index2;
      value = value2;
    }
    return value;
  }
  index_value():index(-1) {}
  index_value(const T& t):index(-1),value(t) {}
};
  

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    cout.precision(args["precision"].as<unsigned>());

    int skip = args["skip"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    //------------ Read Data ---------------//
    vector<stats_table> tables;
    vector<string> filenames;

    if (not args.count("filenames")) {
      tables.push_back(stats_table(std::cin,skip,max));
      filenames.push_back("STDIN");
    }
    else {
      filenames = args["filenames"].as< vector<string> >();
      for(int i=0;i<filenames.size();i++) 
	tables.push_back(stats_table(filenames[i],skip,max));
    }

    if (tables.size() < 1)
      throw myexception()<<"No tables read in!";

    const vector<string> field_names = tables[0].names();

    for(int i=0;i<tables.size();i++) {
      if (tables[i].names() != field_names)
	throw myexception()<<filenames[i]<<": Column names differ from names in '"<<filenames[0]<<"'";
      if (tables[i].n_rows() == 0)
	throw myexception()<<filenames[i]<<": No data line read in!";
    }
    int n_columns = tables[0].n_columns();

    //------------ Parse column mask ----------//
    vector<bool> mask(n_columns,true);
    
    if (args.count("ignore"))
      mask = get_mask_by_ignoring(args["ignore"].as<vector<string> >(), field_names, mask);

    //------------ Generate Report ----------//
    index_value<double> worst_Ne;
    index_value<double> worst_RCI;
    index_value<double> worst_RNe;
    index_value<double> worst_RCF;

    for(int i=0;i<n_columns;i++) 
    {
      if (mask[i]) {
	var_stats S = show_stats(args, tables, i);
	cout<<endl;

	worst_Ne.check_min(i,S.Ne);
	worst_RCI.check_max(i,S.RCI);
	worst_RNe.check_max(i,S.RNe);
	worst_RCF.check_max(i,S.RCF);
      }
    }

    cout<<" Ne  >= "<<worst_Ne.value<<"    ("<<field_names[worst_Ne.index]<<")"<<endl;
    if (tables.size() > 1) {
      cout<<" RCI <= "<<worst_RCI.value<<"    ("<<field_names[worst_RCI.index]<<")"<<endl;
      cout<<" RNe <= "<<worst_RNe.value<<"    ("<<field_names[worst_RNe.index]<<")"<<endl;
      cout<<" RCF <= "<<worst_RCF.value<<"    ("<<field_names[worst_RCF.index]<<")"<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"statreport: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


