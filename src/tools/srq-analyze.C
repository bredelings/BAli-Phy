#include <iostream>
#include <vector>
#include <valarray>
#include <cassert>
#include <cmath>

#include "util.H"
#include "bootstrap.H"
#include "statistics.H"

#include <boost/program_options.hpp>

namespace po = boost::program_options;
using po::variables_map;

using std::string;
using std::cout;


using namespace std;

vector<double> confidence_interval(const valarray<bool>& sample,
				   double (*statistic)(const valarray<bool>&),
				   double P,
				   int n=10000,
				   int blocksize=100) 
{
  valarray<double> values = bootstrap_apply<bool,double>(sample,statistic,n,blocksize);

  return statistics::confidence_interval(values,P);
}

double lower_confidence_bound(const valarray<bool>& sample,
			      double (*statistic)(const valarray<bool>&),
			      double P,
			      int n=10000,
			      int blocksize=100) 
{

  valarray<double> values = bootstrap_apply<bool,double>(sample,statistic,n,blocksize);

  return statistics::lower_confidence_bound(values,P);
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description all("Allowed options");
  all.add_options()
    ("help", "produce help message")
    ("seed", value<unsigned long>(),"random seed")
    ("replicates",value<int>()->default_value(10000),"number of replicates for block bootstrap")
    ("blocksize",value<int>()->default_value(10000),"block size for block bootstrap")
    ("pseudocount",value<int>()->default_value(10000),"pseudocount for block bootstrap")
    ;

  variables_map args;     
  store(parse_command_line(argc, argv, all), args);
  notify(args);    

  if (args.count("help")) {
    cout<<"Usage: srq-analyze [OPTIONS] < in-file\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}



int main(int argc,char* argv[]) 
{ 
  try {
    cerr.precision(10);
    cout.precision(10);

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
    cout<<"random seed = "<<seed<<endl<<endl;

    int nreplicates = args["replicates"].as<int>();
    std::cout<<"nreplicates = "<<nreplicates<<endl<<endl;

    int blocksize = args["blocksize"].as<int>();
    std::cout<<"blocksize = "<<blocksize<<endl<<endl;

    int pseudocount = args["pseudocount"].as<int>();
    std::cout<<"pseudocount = "<<pseudocount<<endl<<endl;
  
    /*----------------------- Read in data ---------------------------*/
    vector<bool> data;
    int i;
    while(cin>>i) {
      assert(i==0 or i==1);
      data.push_back(i);
    } 
    for(int i=0;i<pseudocount;i++) {
      data.push_back(true);
      data.push_back(false);
    }

    valarray<bool> sample(data.size());
    for(int i=0;i<sample.size();i++)
      sample[i] = data[i];
    

    /*----------------------- Basic Statistic ---------------------------*/
    cout<<"Read "<<sample.size()<<" values.    Y = "<<statistics::count(sample)<<"       N = "<<sample.size() - statistics::count(sample)<<endl;

    double mean = statistics::Pr(sample);
    cout<<"mean = "<<mean<<endl;
    cout<<"var  = "<<mean*(1.0-mean)/sample.size()<<" (assumes independant samples && mean is right)"<<endl;
    cout<<"log odds = "<<log10(mean/(1.0-mean))<<" = log("<<mean/(1.0-mean)<<")"<<endl;
    cout<<endl;
    cout<<endl;

    /*------------------------- Really analyze the distribution ------------------------*/

    double lowerb = lower_confidence_bound(sample,statistics::Pr,0.95,nreplicates,blocksize);
    cout<<"95% confidence P > "<<lowerb<<endl;
    cout<<"95% confidence log odds > "<<log10(lowerb/(1.0-lowerb))<<" = log("<<lowerb/(1.0-lowerb)<<")"<<endl;
    cout<<endl;
    cout<<endl;

    double lowerb2 = lower_confidence_bound(sample,statistics::Pr,0.99,nreplicates,blocksize);
    cout<<"99% confidence P > "<<lowerb2<<endl;
    cout<<"99% confidence log odds > "<<log10(lowerb2/(1.0-lowerb2))<<" = log("<<lowerb2/(1.0-lowerb2)<<")"<<endl;
    cout<<endl;
    cout<<endl;
  
    vector<double> interval = confidence_interval(sample,statistics::Pr,0.99,nreplicates,blocksize);
    cout<<"99% confidence interval = ("<<interval[0]<<","<<interval[1]<<")\n";
    cout<<"                  width = "<<interval[1] - interval[0]<<" = 2.0*"<<(interval[1] - interval[0])/2.0<<endl;
    cout<<endl;
    cout<<endl;
  
    /*
      std::valarray<double> values(nreplicates);
      for(int i=1;i<=500;i++) {
      values = bootstrap_apply<bool,double>(sample,statistics::Pr,nreplicates,i);
      cout<<"------------------\n";
      double var = statistics::Var(values);
      cout<<"blocksize = "<<i<<"   sigma = "<<sqrt(var)<<endl;
      }
    */

  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<std::endl;
    exit(1);
  }
}

