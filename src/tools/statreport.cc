/*
  Copyright (C) 2004-2006,2008-2009,2017 Benjamin Redelings

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
#include "util/assert.hh"
#include <vector>

#include <cmath>

#include "util/string/split.hh"
#include "util/string/join.hh"
#include "util/string/convert.hh"
#include "util/index_value.hh"

#include "statistics.hh"
#include "stats-table.hh"
#include "util/math/logsum.hh"

#include <boost/algorithm/string/predicate.hpp>
#include <CLI/CLI.hpp>
#include <optional>
#include <boost/dynamic_bitset.hpp>

extern int log_verbose;

using namespace std;

using boost::dynamic_bitset;

namespace
{
    // Settings shared by the ordinary and conditioned summary printers.
    struct SummaryOptions
    {
        bool mean = false, median = false, mode = false, individual = false;
        optional<double> truth;
        string confidence = "0.95", BCI = "HPD";
        unsigned precision = 4;
    };
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

// Recognize semantic log-odds fields after generated partition and model prefixes.
bool is_log_odds_field(const string& name)
{
    auto separator = name.find_last_of("/:");
    auto component = name.substr(separator == string::npos ? 0 : separator + 1);
    return boost::algorithm::istarts_with(component, "logodds");
}

// If l = log(p/(1-p)), then log(p) = -log1pexp(-l) and log(1-p) = -log1pexp(l).
// Summing these probabilities separately gives the log odds of their posterior mean.
pair<double, double> summarize_log_odds(const string& name, const vector<double>& values)
{
    double log_probability_sum = -infinity<double>;
    double log_complement_sum = -infinity<double>;

    for(double log_odds: values)
    {
	if (std::isnan(log_odds))
	    throw myexception()<<"Field '"<<name<<"' contains NaN log odds.";

	log_probability_sum = logsum(log_probability_sum, -log1pexp(-log_odds));
	log_complement_sum = logsum(log_complement_sum, -log1pexp(log_odds));
    }

    double mean_log_odds = log_probability_sum - log_complement_sum;
    double mean_probability;
    if (mean_log_odds >= 0)
	mean_probability = 1 / (1 + exp(-mean_log_odds));
    else
    {
	double odds = exp(mean_log_odds);
	mean_probability = odds / (1 + odds);
    }
    return {mean_probability, mean_log_odds};
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
    double RCF;
    bool ignored;
    bool increasing;
    bool decreasing;
    bool constant;
    var_stats()
	:ignored(true), increasing(false),decreasing(false),constant(false)
	{}

    var_stats(double a, double b, double c)
	:Ne(a), RCI(b), RCF(c), ignored(false), increasing(false),decreasing(false),constant(false)
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
    {
	std::cerr<<"time_to_cross: direction = "<<direction<<" not recognized\n";
	std::abort();
    }
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

string burnin_value(int b, unsigned total, int subsample)
{
    if (b < total*2/3)
	return convertToString(b*subsample);
    else 
	return "Not Converged!";
}

// Constant chains alone have no direction, even when their different levels make the
// pooled values nonconstant. Require an actual change in at least one monotone chain.
bool monotonic_increasing(const vector<stats_table>& tables, int index)
{
    bool changed = false;
    for (const auto& table: tables)
    {
        const auto& values = table.column(index);
        if (!monotonic_increasing(values)) return false;
        changed |= values.front() != values.back();
    }
    return changed;
}

// Apply the same requirement for decreasing chains without altering the burn-in predicates.
bool monotonic_decreasing(const vector<stats_table>& tables, int index)
{
    bool changed = false;
    for (const auto& table: tables)
    {
        const auto& values = table.column(index);
        if (!monotonic_decreasing(values)) return false;
        changed |= values.front() != values.back();
    }
    return changed;
}


void show_mean(const string& name, const vector<stats_table>& tables, int index, const vector<double>& total, bool show_individual)
{
    using namespace statistics;

    if (tables.size() > 1 and show_individual)
	for(int i=0;i<tables.size();i++) 
	{
	    const vector<double>& values = tables[i].column(index);
	    if (constant(values))
		continue;
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

// Report the probability represented by each chain's log odds and by all chains combined.
void show_log_odds_summary(const string& name, const vector<stats_table>& tables, int index,
			   const vector<double>& total, bool show_individual)
{
    if (tables.size() > 1 and show_individual)
	for(int i=0;i<tables.size();i++)
	{
	    auto [probability, log_odds] = summarize_log_odds(name, tables[i].column(index));
	    cout<<"   "<<name<<" ["<<i+1<<"]:  posterior-probability = "<<probability;
	    cout<<"     [log-odds = "<<log_odds<<"]"<<endl;
	}

    auto [probability, log_odds] = summarize_log_odds(name, total);
    cout<<"   "<<name<<":  posterior-probability = "<<probability;
    cout<<"     [log-odds = "<<log_odds<<"]"<<endl;
}

void show_mode(const string& name, const vector<stats_table>& /*tables*/, int /*index*/, const vector<double>& total, bool /*show_individual*/)
{
    using namespace statistics;

    auto m = mode(total);
    if (m.second >= 0)
	cout<<"   "<<name<<" ^ "<<m.first<<"  [+- "<<m.second<<"]"<<endl;
    else
	cout<<"   "<<name<<" ^ NA"<<endl;
}

const double compare_level=0.8;


void show_median(const SummaryOptions& options, const string& name, const vector<stats_table>& tables, int index,
                 const vector<double>& total, bool show_individual, bool show_summary, double& sum_CI, double& total_CI,
		 double& sum_fraction_contained)
{
    using namespace statistics;

    // Print out median and confidence interval
    vector<double> Ps = convertTo<double>(split(options.confidence,':'));
    sort(Ps.begin(), Ps.end());
  
    bool HPD = (options.BCI == "HPD");

    if (tables.size() > 1)
	for(int i=0;i<tables.size();i++) {
	    const vector<double>& values = tables[i].column(index);
    
	    if (tables.size() > 1)
	    {
		pair<double,double> interval_80 = central_confidence_interval(values,compare_level);
		double x = fraction_in_interval(values,interval_80.first,interval_80.second)/
		    fraction_in_interval(total,interval_80.first,interval_80.second);

		sum_fraction_contained += x;
		sum_CI += std::abs(interval_80.second - interval_80.first);
	    }

	    if (show_summary and not constant(values))
		for(int j=0;j<Ps.size();j++)
		{
		    double P = Ps[j];
		    pair<double,double> interval = HPD?HPD_confidence_interval(values,P):central_confidence_interval(values,P);
		    if (show_individual)
		    {
			if (j==0)
			    cout<<"   "<<name<<" ["<<i+1<<"] ~ "<<median(values);
			else
			    cout<<"    ";
	  
			if ((1.0-P)*values.size() >= 10.0)
			    cout<<"  ("<<interval.first<<","<<interval.second<<")";
			else
			    cout<<"  (NA,NA)";

			cout<<" @ "<<P*100<<"%"<<endl;
		    }
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

    if (not show_summary)
	return;

    for(int i=0,padding=0;i<Ps.size();i++)
    {
	double P = Ps[i];
	pair<double,double> interval = HPD?HPD_confidence_interval(values,P):central_confidence_interval(values,P);

	if (i==0)
	{
	    std::ostringstream o;
	    o.precision(options.precision);
	    if (show_individual)
		o<<"   "<<name<<"     ~ "<<median(values);
	    else
		o<<"   "<<name<<" ~ "<<median(values);
	    cout<<o.str();
	    padding = o.str().size();
	}
	else
	    for(int j=0;j<padding;j++)
		cout<<" ";
    
	if ((1.0-P)*values.size() >= 10.0)
	    cout<<"  ("<<interval.first<<","<<interval.second<<")";
	else
	    cout<<"  (NA,NA)";
	cout<<" @ "<<P*100<<"%"<<endl;
    }
}

void show_error(const SummaryOptions& options, const string& /*name*/, const vector<stats_table>& /*tables*/,
                int /*index*/, const vector<double>& total)
{
    double truth = *options.truth;

    using namespace statistics;

    double bias = statistics::bias(total, truth);
    double bias_overestimate = statistics::overestimate(total, truth);
    double abs_error = statistics::abs_error(total, truth);
    double rms_error = statistics::rms_error(total, truth);
    cout<<"  bias = "<<bias<<endl;
    cout<<"  high = "<<bias_overestimate<<endl;
    cout<<"  absE = "<<abs_error<<endl;
    cout<<"  rmsE = "<<rms_error<<endl;
    cout<<endl;
}

// Treat each table as an independent time-ordered chain with a shared stationary mean.  The
// lag-zero variance uses every retained sample, while positive-lag covariances use only pairs
// within the same chain and weight every valid pair equally.  Repeated values need no special
// handling, and iterating over the tables only through sums makes the result independent of
// chain ordering.  The lag horizon and early stopping rule match the single-chain estimator.
double multi_chain_autocorrelation_time(const vector<stats_table>& tables, int index)
{
    int sample_count = 0;
    int longest_chain = 0;
    double mean = 0;
    for(const auto& table: tables)
    {
	const auto& values = table.column(index);
	int chain_size = values.size();
	sample_count += chain_size;
	longest_chain = max(longest_chain, chain_size);
	for(double value: values)
	    mean += value;
    }
    mean /= sample_count;

    double variance = 0;
    for(const auto& table: tables)
	for(double value: table.column(index))
	    variance += (value - mean)*(value - mean);
    variance /= sample_count;

    int covariance_count = max(1 + sample_count/4, sample_count - 15);
    covariance_count = min(covariance_count, longest_chain - 1);
    double limit = variance*(0.01/sample_count);
    double covariance_sum = 0;
    for(int lag = 1; lag < covariance_count; lag++)
    {
	double covariance = 0;
	int pair_count = 0;
	for(const auto& table: tables)
	{
	    const auto& values = table.column(index);
	    int chain_size = values.size();
	    if (lag >= chain_size)
		continue;
	    for(int i = 0; i + lag < chain_size; i++)
		covariance += (values[i] - mean)*(values[i + lag] - mean);
	    pair_count += chain_size - lag;
	}
	covariance /= pair_count;

	if (covariance < 0 or (lag > 3 and covariance < limit))
	    break;
	covariance_sum += covariance;
    }

    return 1.0 + 2.0*covariance_sum/variance;
}

var_stats show_stats(const SummaryOptions& options, const vector<stats_table>& tables, int index,
		     const vector<vector<int> >& burnin, const vector<unsigned>& sample_counts, int subsample)
{
    const string& name = tables[0].names()[index];

    using namespace statistics;

    bool show_individual = options.individual and (tables.size() >1);

    vector<double> total;
    for(int i=0;i<tables.size();i++)
	total.insert(total.end(),tables[i].column(index).begin(),tables[i].column(index).end());

    if (is_log_odds_field(name))
	show_log_odds_summary(name, tables, index, total, show_individual);

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

    if (show_individual)
	for(int i=0;i<tables.size();i++)
	{
	    const auto& values = tables[i].column(index);
	    if (constant(values))
		cout<<"   "<<name<<" ["<<i+1<<"] = "<<values[0]<<endl;
	}

    // Print out mean and standard deviation
    if (options.mode)
	show_mode(name, tables, index, total, show_individual);

    if (options.mean)
	show_mean(name, tables, index, total, show_individual);

    // Print out median and confidence interval
    double sum_CI=0;
    double total_CI=0;
    double sum_fraction_contained=0;
    bool show_median_summary = options.median or not options.mean;
    show_median(options, name, tables, index, total, show_individual, show_median_summary, sum_CI, total_CI,
		sum_fraction_contained);

    if (options.truth)
	show_error(options, name, tables, index, total);

    // Print out autocorrelation times, Ne, and minimum burn-in
    int maximum_burnin = 0;
    bool burnin_converged = true;
    for(int i=0;i<tables.size();i++) {
	const vector<double>& values = tables[i].column(index);
	int b = burnin[i][index];
	maximum_burnin = max(maximum_burnin, b);
	if (b >= sample_counts[i]*2/3)
	    burnin_converged = false;

	if (show_individual and not constant(values))
	{
	    double tau = autocorrelation_time(values);
	    string spacer;spacer.append(name.size()-1,' ');
	    cout<<"   "<<spacer<<"t @ "<<tau;
	    cout<<"   Ne = "<<int(values.size()/tau);
	    cout<<"   burnin = "<<burnin_value(b,sample_counts[i],subsample)<<endl;
	}
    }
    const vector<double>& values = total;
    double tau;
    if (tables.size() == 1)
	tau = autocorrelation_time(values);
    else
	tau = multi_chain_autocorrelation_time(tables, index);

    string spacer;spacer.append(name.size()-1,' ');

    cout<<"   "<<spacer<<"t @ "<<tau;
    double Ne = values.size()/tau;
    cout<<"   Ne = "<<int(Ne);
    cout<<"   burnin = ";
    if (burnin_converged)
	cout<<maximum_burnin*subsample;
    else
	cout<<"Not Converged!";
    if (integers) cout<<"   [integer] ";
    cout<<endl;

    // Print out Potential Scale Reduction Factors (PSRFs)
    double RCI = 1;
    double RCF = 1;
    if (tables.size() > 1) {
	// An integer interval [a,b] contains b-a+1 possible values, so its width cannot vanish.
	// For continuous values, a positive pooled width over zero within-chain width is infinite;
	// two zero widths conventionally produce the neutral value 1 for this diagnostic.
	if (integers)
	    RCI = (total_CI + 1)/(sum_CI + 1);
	else if (sum_CI > 0)
	    RCI = total_CI/sum_CI;
	else if (total_CI > 0)
	    RCI = infinity<double>;
	cout<<"       PSRF-80%CI = "<<RCI;
	RCF = sum_fraction_contained; //compare_level;
	cout<<"       PSRF-RCF = "<<RCF<<endl;
    }

    cout<<endl;
    return var_stats(Ne,RCI,RCF);
}

// stats-table can't distinguish double && int

// Conditional draws retain their descriptive distribution, but not a regularly sampled timeline.
// Reuse the ordinary summary functions with no chain diagnostics; never pass them an empty sample.
void show_conditioned_stats(const SummaryOptions& options, const string& name, const vector<double>& values)
{
    if (values.empty())
    {
        cout<<"   "<<name<<" = [no matching samples]\n";
        return;
    }
    if (is_log_odds_field(name))
        show_log_odds_summary(name, {}, 0, values, false);
    if (constant(values))
    {
        cout<<"   "<<name<<" = "<<values[0]<<"\n";
        return;
    }
    if (options.mode) show_mode(name, {}, 0, values, false);
    if (options.mean) show_mean(name, {}, 0, values, false);
    double sum_CI = 0, total_CI = 0, fraction = 0;
    show_median(options, name, {}, 0, values, false, options.median or not options.mean,
                sum_CI, total_CI, fraction);
    if (options.truth) show_error(options, name, {}, 0, values);
}

/// FIXME - reduce the numbers of quantile/median/confidence_interval calls?
///       - 543 calls to median for 42*6: 
///       - (Remember that SOME of the calls are only sorting the last THIRD of the data.)

//  FIXME - use scan_lines and an accumulator to read the data?

/// Why is the autocorrelation taking so long?  Can we speed it up for the combined runs anyhow?

int main(int argc,char* argv[]) 
{ 
    std::ios::sync_with_stdio(false);

    try {
        CLI::App app{"Summarize numeric samples from MCMC logs.", "statreport"};
        app.usage("Usage: statreport [OPTIONS] FILE [FILE ...]");
        app.get_formatter()->long_option_alignment_ratio(0.2f);
        app.footer("Default: Report the median and 95% credible interval for each column.");
        SummaryOptions options;
        vector<string> filenames, ignore, select;
        string skip_text = "10%";
        optional<string> condition_expression;
        int subsample = 1, last = -1, min_rows = -1;
        bool verbose = false;
        app.add_option("--filenames,FILE", filenames, "Filenames to analyze ('-' for STDIN)")->required();
        app.add_flag("-V,--verbose", verbose, "Output more log messages on stderr.");
        app.add_option("-s,--skip", skip_text, "Number or percentage of initial samples to skip.")->capture_default_str();
        app.add_option("-x,--subsample", subsample, "Factor by which to sub-sample.")->capture_default_str();
        app.add_option("-u,--until", last, "Read up to this iteration.");
        // Each occurrence takes one field specification so that following filenames remain positional.
        app.add_option("-I,--ignore", ignore, "Do not analyze these fields.")
            ->type_name("FIELD")->type_size(1)->expected(1)->allow_extra_args(false)->take_all();
        app.add_option("-S,--select", select, "Analyze only these fields.")
            ->type_name("FIELD")->type_size(1)->expected(1)->allow_extra_args(false)->take_all();
        app.add_option("--condition", condition_expression,
                       "Summarize samples matching key=value, without time-series diagnostics.");
        app.add_flag("-i,--individual", options.individual, "Show results for individual files separately also.");
        app.add_option("--truth", options.truth, "True value");
        app.add_option("--min", min_rows, "Required minimum number of lines to read.");
        app.add_flag("--mean", options.mean, "Show mean and standard deviation.");
        app.add_flag("--mode", options.mode, "Show mode (with precision)");
        app.add_flag("--median", options.median, "Show median and confidence level.");
        app.add_option("--confidence", options.confidence, "Confidence interval levels (colon-separated).")
            ->capture_default_str();
        app.add_option("--BCI", options.BCI, "Type of Bayesian Credible Interval (BCI): HPD or central")
            ->capture_default_str();
        app.add_option("-p,--precision", options.precision, "Number of significant figures.")->capture_default_str();
        try
        {
            app.parse(argc, argv);
        }
        catch (const CLI::ParseError& error)
        {
            app.exit(error);
            return error.get_exit_code() == 0 ? 0 : 1;
        }
        if (verbose) log_verbose = 1;
        cout.precision(options.precision);

        optional<pair<string, double>> condition;
        if (condition_expression)
        {
            const auto& expression = *condition_expression;
            auto equals = expression.rfind('=');
            if (equals == string::npos or equals == 0)
                throw myexception()<<"Expected --condition key=value.";
            auto value = can_be_converted_to<double>(expression.substr(equals + 1));
            if (not value or not std::isfinite(*value))
                throw myexception()<<"Condition value must be a finite number.";
            condition = {expression.substr(0, equals), *value};
        }

	//------------ Read Data ---------------//
	vector<stats_table> tables;
        int condition_output_count = -1;

	for(auto& filename: filenames)
        {
            istream_or_ifstream f(std::cin, "-", filename, "statistics_file");

            if (condition)
            {
                // JSON columns can disappear as later records change shape, so finish loading
                // before selecting them. TSV has a fixed header; convert only selected fields so
                // unselected text columns remain usable.
                optional<stats_table> json_table;
                optional<TableReader> reader;
                if (f.peek() == '{')
                    json_table.emplace(f, 0, subsample, last, vector<string>{}, vector<string>{});
                else
                    reader.emplace(f, 0, subsample, last, vector<string>{}, vector<string>{});
                const TableBase& source = json_table ? static_cast<const TableBase&>(*json_table) : *reader;
                auto indices = get_indices_for_names(source.names(), ignore, select);
                if (condition_output_count != -1 and condition_output_count != indices.size())
                    throw myexception()<<filename<<": Selected column counts differ.";
                condition_output_count = indices.size();
                auto index = source.maybe_find_column_index(condition->first);
                if (not index)
                    throw myexception()<<filename<<": Condition field '"<<condition->first
                                       <<"' is missing or is not a stable numerical column.";
                if (std::find(indices.begin(), indices.end(), *index) == indices.end())
                    indices.push_back(*index);
                tables.emplace_back(apply_indices(source.names(), indices));
                if (json_table)
                {
                    for (int i = 0; i < indices.size(); ++i)
                        tables.back().column(i) = std::move(json_table->column(indices[i]));
                }
                else while (auto row = reader->get_row())
                {
                    vector<double> values;
                    for (int col: indices)
                    {
                        auto value = can_be_converted_to<double>((*row)[col]);
                        if (not value)
                            throw myexception()<<filename<<": Non-numeric value in '"<<source.names()[col]<<"'.";
                        values.push_back(*value);
                    }
                    tables.back().add_row(values);
                }
            }
            else
                tables.emplace_back(f,0,subsample,last,ignore,select);

	    if (not tables.back().n_rows())
		throw myexception()<<"File '"<<filename<<"' has no samples left after removal of burn-in!";
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
	    const string& s = skip_text;
	    if (auto arg_skip = can_be_converted_to<int>(s))
            {
                if (*arg_skip < 0) throw myexception()<<"skip must be nonnegative";
                // Retained rows have original indices 0, subsample, 2*subsample, ... .
                // Ceiling division removes every retained row whose index is below the cutoff.
                skip = *arg_skip/subsample + (*arg_skip % subsample != 0);
            }
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
	vector<unsigned> sample_counts(tables.size());

	index_value<int> worst_burnin;
	index_value<int> worst_failed_burnin;

	for(int i=0;i<tables.size();i++) {
	    sample_counts[i] = tables[i].n_rows();
	    for(int j=0;not condition and j<n_columns;j++)
	    {
		int b = get_burn_in(tables[i].column(j), 0.05, 2);
		burnin[i][j] = b;
		if (b < sample_counts[i]*2/3)
		    worst_burnin.check_max(j,b);
		else
		    worst_failed_burnin.check_max(j,b);
	    }
	    if (tables[i].n_rows() < min_rows)
		throw myexception()<<"File '"<<filenames[i]<<"' has only "<<tables[i].n_rows()<<" rows, but "<<min_rows<<" are required!";
	    if (tables[i].n_rows() <= skip)
		throw myexception()<<"File '"<<filenames[i]<<"' has no samples left after removal of burn-in!";
	    tables[i].chop_first_rows(skip);
	}

        if (condition)
        {
            int condition_index = tables[0].find_column_index(condition->first);
            cout<<"Condition: "<<*condition_expression<<"\n";
            int total_count = 0;
            for(int chain=0; chain<tables.size(); chain++)
            {
                auto& table = tables[chain];
                vector<int> matching;
                for(int row=0; row<table.n_rows(); row++)
                    if (table.column(condition_index)[row] == condition->second)
                        matching.push_back(row);
                for(int col=0; col<table.n_columns(); col++)
                {
                    auto& values = table.column(col);
                    for(int row=0; row<matching.size(); row++) values[row] = values[matching[row]];
                    values.resize(matching.size());
                }
                cout<<"Matching samples ["<<chain+1<<"] = "<<matching.size()<<"\n";
                total_count += matching.size();
            }
            cout<<"Matching samples = "<<total_count<<"\n\n";
            for(int col=0; col<condition_output_count; col++)
            {
                vector<double> pooled;
                for(int chain=0; chain<tables.size(); chain++)
                {
                    const auto& values = tables[chain].column(col);
                    pooled.insert(pooled.end(), values.begin(), values.end());
                    if (options.individual)
                        show_conditioned_stats(options, field_names[col]+" ["+to_string(chain+1)+"]", values);
                }
                show_conditioned_stats(options, field_names[col], pooled);
                cout<<"\n";
            }
            return 0;
        }

    
	//------------ Generate Report ----------//
	index_value<double> worst_Ne;
	index_value<double> worst_RCI;
	index_value<double> worst_RCF;

	vector<string> increasing_names;
	vector<string> decreasing_names;
	for(int i=0;i<n_columns;i++) 
	{
	    var_stats S = show_stats(options, tables, i, burnin, sample_counts, subsample);
	    cout<<endl;

	    if (not S.ignored) {
		worst_Ne.check_min(i,S.Ne);
		worst_RCI.check_max(i,S.RCI);
		worst_RCF.check_max(i,S.RCF);
	    }
	    else if (S.increasing)
		increasing_names.push_back(field_names[i]);
	    else if (S.decreasing)
		decreasing_names.push_back(field_names[i]);
	}

	if (worst_Ne)
	    cout<<" Ne  >= "<<*worst_Ne.amount()<<"    ("<<field_names[*worst_Ne.index()]<<")"<<endl;
	if (worst_failed_burnin)
	    cout<<" min burnin <= Not Converged!    ("<<field_names[*worst_failed_burnin.index()]<<")"<<endl;
	else if (worst_burnin)
	    cout<<" min burnin <= "<<*worst_burnin.amount()*subsample<<"    ("<<field_names[*worst_burnin.index()]<<")"<<endl;
	if (tables.size() > 1) {
	    if (worst_RCI)
		cout<<" PSRF-80%CI <= "<<*worst_RCI.amount()<<"    ("<<field_names[*worst_RCI.index()]<<")"<<endl;
	    if (worst_RCF)
		cout<<" PSRF-RCF <= "<<*worst_RCF.amount()<<"    ("<<field_names[*worst_RCF.index()]<<")"<<endl;
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
