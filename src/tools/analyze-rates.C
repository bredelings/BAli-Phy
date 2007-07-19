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

using namespace std;

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

  discrete_distribution(int n)
    :f(n),r(n)
  {}
};

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
    {
      assert(rates[b].size() == data.size());
      double bl = (double(b)+0.5)/B;
      cout<<bl<<"\t"<<statistics::quantile(rates[b],l)<<endl;
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}


