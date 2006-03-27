#include <cmath>
#include <cassert>
#include "statistics.H"

using std::valarray;
using std::vector;
using std::pair;

//FIXME - add 'hat'?
namespace statistics {

  double fraction(const valarray<bool>& v) {
    return fraction(count(v),v.size(),0);
  }
  
  double Pr(const valarray<bool>& v) {
    return fraction(v);
  }
  
  double odds(const valarray<bool>& v) {
    double y = count(v);
    double n = v.size() - count(v);
    return y/n;
  }

  double log_odds(const valarray<bool>& v) {
    return log(odds(v));
  }

  double quantile(const valarray<double>& values, double Q)
  {
    assert(0 <= Q and Q <= 1.0);
    assert(values.size() > 0);

    if (values.size() == 1)
      return values[0];

    // sort values
    vector<double> values2(values.size());
    for(int i=0;i<values.size();i++)
      values2[i] = values[i];
    sort(values2.begin(),values2.end());

    // sort compute indices
    double index = Q*values2.size() - 0.5;
    int index1 = (int)index;
    int index2 = index1+1;
    double p1 = index2-index;
    double p2 = 1.0-p1;

    // compute quantile
    if (index1 < 0)                      // index <= 0
      return values2[0];
    else if (index2 >= values2.size())   // index >= values2.size() - 1
      return values2.back();
    else {
      assert(index1 >= 0 and index1 < values2.size());
      assert(index2 >= 0 and index2 < values2.size());
      return p1*values2[index1] + p2*values2[index2];
    }
  }

  double median(const valarray<double>& values)
  {
    return quantile(values,0.5);
  }

  pair<double,double> confidence_interval(const valarray<double>& values,double P) 
  {
    double alpha = (1.0-P)/2.0;

    pair<double,double> interval;
    interval.first = quantile(values,alpha);
    interval.second = quantile(values,1.0-alpha);
    
    return interval;
  }

  valarray<bool> add_pseudocount(const valarray<bool>& sample1,int pseudocount) {
    valarray<bool> sample2(sample1.size() + 2*pseudocount);
    
    int i=0;
    for(int j=0;j<pseudocount;i++,j++)
      sample2[i] = true;
    
    for(int j=0;j<sample1.size();i++,j++)
      sample2[i] = sample1[j];
    
    for(int j=0;j<pseudocount;i++,j++)
      sample2[i] = false;
    
    return sample2;
  }

  vector<int> total_times(const valarray<bool>& v) 
  {
    vector<int> total(v.size()+1);

    total[0] = 0;
    for(int i=0;i<v.size();i++) {
      total[i+1] = total[i];
      if (v[i]) total[i+1]++;
    }
      
    return total;
  }

  vector<int> regeneration_times(const valarray<bool>& v) 
  {
    vector<int> times;

    times.push_back(0);
    for(int i=1;i<v.size();i++) {
      if (v[i] and not v[i-1])
	times.push_back(i);
    }
    times.push_back(v.size());
      
    return times;
  }

  vector<double> autocovariance(valarray<double> x,unsigned N)
  {
    // specify sample size if unspecified
    if (N==0) N = x.size()/4;
    if (N==0) N = 1;

    // shift data for zero mean
    double xm = average(x);
    for(int i=0;i<x.size();i++)
      x[i] -= xm;
	  
    // allocate covariances
    vector<double> rho(N);

    // compute each autocorrelation rho[k]
    for(int k=0;k<N;k++) {

      for(int i=0;i<N;i++)
	rho[k] += x[i]*x[i+k];
      rho[k] /= (N-k);

      if (rho[k] <= 0.0) {
	rho.resize(k);
	break;
      }
    }

    return rho;
  }

  vector<double> autocorrelation(const valarray<double>& x,unsigned N)
  {
    vector<double> rho = autocovariance(x,N);
    const double V = rho[0];
    for(int i=0;i<rho.size();i++)
      rho[i] /= V;
    return rho;
  }

  double autocorrelation_time_zero(const valarray<double>& x,unsigned max)
  {
    return autocorrelation(x,max).size();
  }

  double autocorrelation_time_sum(const valarray<double>& x,unsigned max)
  {
    vector<double> r = autocorrelation(x,max);

    double sum = 0;
    for(int i=0;i<r.size();i++)
      sum += r[i];

    return sum;
  }
}

