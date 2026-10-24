/*
   Copyright (C) 2004-2005,2008 Benjamin Redelings

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
#include <vector>
#include <valarray>
#include <cmath>

#include "bootstrap.hh"
#include "util/myexception.hh"
#include "util/math/logsum.hh"
#include "util/log-level.hh"

#include <CLI/CLI.hpp>
#include <optional>

using namespace std;

double Pr_harmonic(const valarray<double>& v) {
  double sum = 0;

  for(int i=0;i<v.size();i++)
    sum += v[i];

  double denominator=-std::numeric_limits<double>::infinity();

  for(int i=0;i<v.size();i++)
    denominator = logsum(denominator,sum-v[i]);

  return sum - denominator + log(double(v.size()));
}

double Pr_smoothed(const valarray<double>& v,double delta,double Pdata) {

  double log_delta = log(delta);
  double log_inv_delta = log(1-delta);

  double n = v.size();
  double top    = log(n) + log_delta - log_inv_delta + Pdata;
  double bottom = log(n) + log_delta - log_inv_delta;

  for(int i=0;i<v.size();i++) {
    double weight = -logsum(log_delta, log_inv_delta + v[i]-Pdata);
    top = logsum(top,weight + v[i]);

    bottom = logsum(bottom,weight);
  }

  return top - bottom;
}

double Pr_smoothed(const valarray<double>& v) {
  // Sample from the prior w/ probability delta...
  double delta=0.01;

  // Use the harmonic estimator as the starting guess
  double Pdata = Pr_harmonic(v);

  // initialize this to a value which allows it to enter the loop
  double deltaP = 1.0;

  int iterations = 0;
  double dx = 10.0;

  while (std::abs(deltaP) > 1.0e-3) {

    double g1 = Pr_smoothed(v,delta,Pdata) - Pdata;
    //    std::cerr<<"iterations = "<<iterations<<"  Pdata = "<<Pdata<<"   g = "<<g1<<endl;

    double Pdata2 = Pdata + g1;

    dx = g1 * 10;
    double g2 = Pr_smoothed(v,delta,Pdata+dx) - (Pdata+dx);

    double dgdx = (g2-g1)/dx;

    double Pdata3 = Pdata - g1/dgdx;
    if (Pdata3 < 2.0*Pdata or Pdata3 > 0 or Pdata3 > 0.5*Pdata)
      Pdata3 = Pdata + 10*g1;

    double g3 = Pr_smoothed(v,delta,Pdata3) - Pdata3;

    // Try to do Newton's method
    if (std::abs(g3) <= std::abs(g2) and ((g3 > 0) or (std::abs(dgdx)>0.01))) {
      deltaP = Pdata3-Pdata;
      Pdata = Pdata3;
    }
    // Otherwise see if we can go 10 times as far as one step
    else if (std::abs(g2) <= std::abs(g1)) {
      Pdata2 += g2;
      deltaP = Pdata2-Pdata;
      Pdata = Pdata2;
    }
    // Otherwise go one step
    else {
      deltaP = g1;
      Pdata += g1;
    }

    //    std::cerr<<" Pdata2 = "<<Pdata2<<"   g = "<<g2<<endl;
    //    std::cerr<<" Pdata3 = "<<Pdata3<<"   g = "<<g3<<"        dg/dx = "<<dgdx<<endl;

    iterations++;

    // if we aren't converging, warn and don't give an answer
    if (iterations >400) {
      std::cerr<<"model_P: Probabilities not converging!!!\n";
      return -std::numeric_limits<double>::infinity();
      //      exit(1);
    }
    
  }
  return Pdata;
}

int main(int argc,char* argv[]) 
{ 

  try{

    std::cout.precision(3);
    std::cout.setf(ios::fixed);

    CLI::App app{"Compute marginal likelihoods using the stabilized harmonic mean estimator.", "model_P"};
    app.usage("Usage: model_P [OPTIONS] < in-file");
    app.get_formatter()->long_option_alignment_ratio(0.2f);
    app.footer("Input: natural-log likelihoods; output: log marginal likelihood and bootstrap standard deviation.");
    optional<unsigned long> requested_seed;
    bool verbose = false;
    app.add_option("--seed", requested_seed, "Random seed");
    app.add_flag("--verbose", verbose, "Output more log messages on stderr");
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

    //---------- Initialize random seed -----------//
    unsigned long seed;
    if (requested_seed)
    {
      seed = *requested_seed;
      myrand_init(seed);
    }
    else
      seed = myrand_init();

    if (log_verbose)
      cerr<<"model_P: random seed = "<<seed<<endl<<endl;

    vector<double> data;

    double d;
    while(cin>>d) {
      data.push_back(d);
    }

    if (log_verbose)
      cerr<<"model_P: OK. Read "<<data.size()<< " values."<<endl;
 
    if (data.empty())
      throw myexception()<<"No log-likelihood values were read!";

    // Translate vector to valarray...
    valarray<double> values(data.size());
    for(int i=0;i<values.size();i++)
      values[i] = data[i];

    double PM = Pr_smoothed(values);

    if (not std::isfinite(PM))
      throw myexception()<<"The log marginal likelihood estimate is not finite.";

    //---------- Get bootstrap sample --------------/

    int blocksize = data.size()/100+2;
    if (blocksize > values.size())
      blocksize = values.size();

    valarray<double> values2 = bootstrap_apply<double,double>(values,Pr_smoothed,100,blocksize);

    for (double value: values2)
      if (not std::isfinite(value))
        throw myexception()<<"A bootstrap log marginal likelihood estimate is not finite.";

    // Translation does not change variance. Center on the first replicate before averaging
    // to avoid losing small differences at a large common offset; then sum squared deviations
    // instead of subtracting nearly equal moments. Divide by N for the empirical variance.
    double mean_offset = 0;
    for (double value: values2)
      mean_offset += value - values2[0];
    mean_offset /= values2.size();
    double sum_squares = 0;
    for (double value: values2)
    {
      double deviation = (value - values2[0]) - mean_offset;
      sum_squares += deviation * deviation;
    }
    double stddev = sqrt(sum_squares / values2.size());
    if (not std::isfinite(stddev))
      throw myexception()<<"The bootstrap standard deviation is not finite.";

    std::cout<<"log P(data|M) = "<<PM<<"    +- "<<stddev<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"model_P: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
