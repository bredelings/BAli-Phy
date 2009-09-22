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
    ("sub-sample",value<int>()->default_value(1),"factor by which to sub-sample")
    ("max",value<int>(),"maximum number of trees to read")
    ("mean", "Show mean and standard deviation")
    ("median", "Show median and confidence level")
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



int time_to_cross_above(const vector<double>& data, int start, double x)
{
  for(int i=start+2;i<data.size();i++)
  {
    if (data[i-1] < x and data[i] >= x)
      return i;
  }
  // never occurs
  return data.size();
}

int time_to_cross_below(const vector<double>& data, int start, double x)
{
  for(int i=start+1;i<data.size();i++)
  {
    if (data[i-1] > x and data[i] <= x)
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

  t = time_to_cross(data,t,x1,x3,direction);
  direction = !direction;
  n--;
  
  for(;n>0;n--) {
    t = time_to_cross(data,t,x1,x3,direction);
    t = time_to_cross(data,t,x1,x3,!direction);
  }
  return t;
}

string burnin_value(int b,const vector<double>& v)
{
  if (b<v.size()*2/3)
    return convertToString(b);
  else 
    return "Not Converged!";
}


var_stats show_stats(variables_map& args, const vector<stats_table>& tables,int index,const vector<vector<int> >& burnin)
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
	cout<<"   Ne = "<<values.size()/tau;
	cout<<"   burnin = "<<b<<endl;
      }
      worst_burnin.check_max(i,b);
    }
  const vector<double>& values = total;
  double tau = autocorrelation_time(values);

  string spacer;spacer.append(name.size()-1,' ');

  cout<<"   "<<spacer<<"t @ "<<tau;
  double Ne = values.size()/tau;
  cout<<"   Ne = "<<Ne;
  if (tables.size() == 1)
    worst_burnin.value = burnin[0][index];
  cout<<"   burnin = "<<worst_burnin.value<<endl;

  // Print out Potential Scale Reduction Factors (PSRFs)
  double RNe = 1;
  double RCI = 1;
  double RCF = 1;
  if (tables.size() > 1) {
    RNe = tau/sum_tau*tables.size();
    cout<<"   RNe = "<<RNe;
    RCI = total_CI/sum_CI;
    cout<<"       RCI = "<<RCI;
    RCF = sum_fraction_contained; //compare_level;
    cout<<"       RCF = "<<RCF<<endl;
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

// stats-table can't distinguish double && int

/// FIXME - reduce the numbers of quantile/median/confidence_interval calls?
///       - 543 calls to median for 42*6: 
///       - (Remember that SOME of the calls are only sorting the last THIRD of the data.)

//  FIXME - use scan_lines and an accumulator to read the data?

/// Why is the autocorrelation taking so long?  Can we speed it up for the combined runs anyhow?

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    cout.precision(args["precision"].as<unsigned>());

    int skip = args["skip"].as<int>();

    int subsample=args["sub-sample"].as<int>();

    int max = -1;
    if (args.count("max"))
      max = args["max"].as<int>();

    //------------ Read Data ---------------//
    vector<stats_table> tables;
    vector<string> filenames;

    if (not args.count("filenames")) {
      tables.push_back(stats_table(std::cin,0,subsample,max));
      filenames.push_back("STDIN");
    }
    else {
      filenames = args["filenames"].as< vector<string> >();
      for(int i=0;i<filenames.size();i++) {
	tables.push_back(stats_table(filenames[i],0,subsample,max));
	if (not tables.back().n_rows())
	  throw myexception()<<"File '"<<filenames[i]<<"' has no samples left after removal of burn-in!";
      }
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

    //------------ Handle Burnin ------------//
    vector< vector<int> > burnin(tables.size(), vector<int>(n_columns,1));

    index_value<int>    worst_burnin(1); 

    for(int i=0;i<tables.size();i++) {
      for(int j=0;j<n_columns;j++) 
	if (mask[j]) {
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

    for(int i=0;i<n_columns;i++) 
    {
      if (mask[i]) {
	var_stats S = show_stats(args, tables, i, burnin);
	cout<<endl;

	worst_Ne.check_min(i,S.Ne);
	worst_RCI.check_max(i,S.RCI);
	worst_RNe.check_max(i,S.RNe);
	worst_RCF.check_max(i,S.RCF);
      }
    }

    cout<<" Ne  >= "<<worst_Ne.value<<"    ("<<field_names[worst_Ne.index]<<")"<<endl;
    cout<<" min burnin <= "<<worst_burnin.value<<"    ("<<field_names[worst_burnin.index]<<")"<<endl;
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


