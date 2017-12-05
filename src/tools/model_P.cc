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
#include <algorithm>

#include "statistics.H"
#include "bootstrap.H"
#include "math/logsum.H"
#include "util.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using namespace std;

double Pr_harmonic(const valarray<double>& v) {
  double sum = 0;

  for(int i=0;i<v.size();i++)
    sum += v[i];

  double denominator=log_0;

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
      return log_0;
      //      exit(1);
    }
    
  }
  return Pdata;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("seed", value<unsigned long>(),"random seed")
    ("verbose","Output more log messages on stderr.")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: model_P < in-file\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("verbose")) log_verbose = 1;

  return args;
}




int main(int argc,char* argv[]) 
{ 

  try{

    std::cout.precision(3);
    std::cout.setf(ios::fixed);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
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
 
    // Translate vector to valarray...
    valarray<double> values(data.size());
    for(int i=0;i<values.size();i++)
      values[i] = data[i];

    double PM = Pr_smoothed(values);

    cout<<"P(data|M) = "<<PM<<"  ";
    cout.flush();

    //---------- Get bootstrap sample --------------/

    int blocksize = data.size()/100+2;
    if (blocksize > values.size())
      blocksize = values.size();

    valarray<double> values2 = bootstrap_apply<double,double>(values,Pr_smoothed,100,blocksize);

    double stddev = sqrt( statistics::Var(values2) );

    std::cout<<"  +- "<<stddev<<endl;
  }
  catch (std::exception& e) {
    std::cerr<<"model_P: Error! "<<e.what()<<endl;
    exit(1);
  }
  return 0;
}
