/*
   Copyright (C) 2007-2008 Benjamin Redelings

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

#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <valarray>
#include <cmath>

#include "util.H"
#include "statistics.H"
#include "stats-table.H"

#include <boost/program_options.hpp>

using std::vector;
using std::valarray;
using std::string;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

namespace po = boost::program_options;
using po::variables_map;

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description invisible("Invisible options");
  invisible.add_options()
    ("columns", value<vector<string> >(),"columns to keep")
    ;

  options_description visible("All options");
  visible.add_options()
    ("help", "Produce help message")
    ("level", value<double>()->default_value(0.5),"confidence level to report")
    ("bins", value<unsigned>()->default_value(100),"number of bins")
    ("smooth", value<unsigned>()->default_value(30),"number of smoothing iterations")
    ("type", value<string>()->default_value("icdf"),"icdf, cdf, pdf")
    ;

  options_description all("All options");
  all.add(invisible).add(visible);

  // positional options
  positional_options_description p;
  p.add("columns", -1);

  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: rates-analyze [OPTIONS] DP-name [column-name ...] < data-file \n";
    cout<<"Summarize distribution of rate distributions.\n\n";
    cout<<visible<<"\n";
    exit(0);
  }

  return args;
}

struct discrete_distribution
{

  valarray<double> f;
  valarray<double> r;

  double mean_rate() const {
    double t = 0;
    for(int i=0;i<size();i++)
      t += f[i]*r[i];
    return t;
  }

  unsigned size() const {return f.size();}

  void normalize() {
    double mr = mean_rate();
    for(int i=0;i<size();i++)
      r[i] /= mr;
  }

  void sort();

  discrete_distribution(int n)
    :f(n),r(n)
  {}
};

void discrete_distribution::sort()
{
  // sort bins to enforce monotonically increasing order of rates
  vector<double> rates;
  for(int i=0;i<size();i++)
    rates.push_back(r[i]);
    
  vector<int> order = iota<int>(size());

  std::sort(order.begin(), order.end(), sequence_order<double>(rates));

  valarray<double> f2 = f;
  valarray<double> r2 = r;

  for(int i=0;i<size();i++) 
  {
    f2[i] = f[order[i]];
    r2[i] = r[order[i]];
  }

  f = f2;
  r = r2;
}

vector<double> smooth(vector<double>& pdf,double f) 
{
  vector<double> pdf2(pdf.size(),0);

  for(int i=1;i<pdf.size()-1;i++) {
    pdf2[i-1] += pdf[i]*(1.0-f)/2.0;
    pdf2[i+1] += pdf[i]*(1.0-f)/2.0;
    pdf2[i  ] += pdf[i]*f;
  }

  pdf2[0] += pdf[0]*0.5*(f+1);
  pdf2[1] += pdf[0]*(1.0-f)/2.0;

  unsigned L = pdf.size()-1;
  pdf2[L  ] += pdf[L]*0.5*(f+1);
  pdf2[L-1] += pdf[L]*(1.0-f)/2.0;

  return pdf2;
}

int main(int argc,char* argv[]) 
{ 
  try {
    //----------- Parse command line  -----------//
    variables_map args = parse_cmd_line(argc,argv);

    //------------ Parse column names ----------//
    vector<string> headers = read_header(std::cin);

    //------------ Parse column mask ----------//
    vector<int> f_index;
    vector<int> r_index;
    
    for(int i=0;;i++) {
      string f_name = "DP::f" + convertToString(i+1);
      string r_name = "DP::rate" + convertToString(i+1);

      int f_loc = find_index(headers,f_name);
      int r_loc = find_index(headers,r_name);

      if (f_loc == -1 or r_loc == -1)
	break;

      f_index.push_back(f_loc);
      r_index.push_back(r_loc);
    }
    
    const int N = f_index.size();

    cerr<<"Found "<<N<<" rate categories (DP::*).\n";
    
    //------------ Read Data ---------------//
    vector< discrete_distribution > data;
    
    vector<double> v;

    int line_number=0;
    string line;
    while(getline(cin,line)) 
    {
      line_number++;

      v = split<double>(line,'\t');

      if (v.size() != headers.size())
	throw myexception()<<"Found "<<v.size()<<"/"<<headers.size()<<" values on line "<<line_number<<".";

      discrete_distribution D(N);

      for(int i=0;i<N;i++) 
      {
	D.f[i] = v[f_index[i]];
	D.r[i] = v[r_index[i]];
      }
      D.normalize();
      D.sort();

      data.push_back(D);
    }

    //------------------------------------//

    const int B = args["bins"].as<unsigned>();

    vector< valarray<double> > rates(B,valarray<double>(data.size()) );

    for(int i=0; i<data.size(); i++) 
    {
      int k = 0;
      double ft = data[i].f[k];

      for(int b = 0; b<B;b++) 
      {
	double bl = (double(b)+0.5)/B;
	while (bl > ft) {
	  k++;
	  ft += data[i].f[k];
	}
	rates[b][i] = data[i].r[k];
      }
    }
    
    //------------------------------------//

    const double l = args["level"].as<double>();

    vector<double> q_rates(B);
    for(int b=0;b<B;b++) 
      q_rates[b] = statistics::quantile(rates[b],l);


    if (args["type"].as<string>() == "icdf")
      for(int b=0;b<B;b++) {
	double bl = (double(b)+0.5)/B;
	cout<<bl<<"\t"<<q_rates[b]<<endl;
      }
    else if (args["type"].as<string>() == "cdf")
      for(int b=0;b<B;b++) {
	double bl = (double(b)+0.5)/B;
	cout<<q_rates[b]<<"\t"<<bl<<endl;
      }
    else if (args["type"].as<string>() == "pdf")
    {
      vector<double> pdf(1000,0);

      double R = max(q_rates);

      for(int b=0;b<B-2;b++) 
      {
	double r1 = q_rates[b];
	double r2 = q_rates[b+1];

	double density = (1.0/B)/(r2-r1);

	for(int r=0;r<pdf.size();r++) 
        {
	  double left  = (double(r))/pdf.size()*R;
	  //	  double center  = (double(r)+0.5)/pdf.size()*R;
	  double right  = (double(r)+1.0)/pdf.size()*R;

	  double ll = std::max(r1,left);
	  double rr = std::min(r2,right);
	  if (ll < rr) 
	    pdf[r] += density*(rr-ll)/(right-left);
	}
      }

      unsigned n_smooth = args["smooth"].as<unsigned>();
      for(int i=0;i<n_smooth;i++)
	pdf = smooth(pdf,0.7);

      for(int r=0;r<pdf.size();r++) {
	double center  = (double(r)+0.5)/pdf.size()*R;
	cout<<center<<"\t"<<pdf[r]<<endl;
      }
    }
  }
  catch (std::exception& e) {
    std::cerr<<"analyze-rates: Error! "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


