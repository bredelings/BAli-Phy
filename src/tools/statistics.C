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

  double quantile_sorted(const vector<double>& values, double Q)
  {
    assert(0 <= Q and Q <= 1.0);
    assert(values.size() > 0);

    if (values.size() == 1)
      return values[0];

    // sort compute indices
    double index = Q*values.size() - 0.5;
    int index1 = (int)index;
    int index2 = index1+1;
    double p1 = index2-index;
    double p2 = 1.0-p1;

    // compute quantile
    if (index1 < 0)                      // index <= 0
      return values[0];
    else if (index2 >= values.size())   // index >= values.size() - 1
      return values.back();
    else {
      assert(index1 >= 0 and index1 < values.size());
      assert(index2 >= 0 and index2 < values.size());
      return p1*values[index1] + p2*values[index2];
    }
  }

  double quantile(vector<double> values, double Q)
  {
    assert(0 <= Q and Q <= 1.0);
    assert(values.size() > 0);

    if (values.size() == 1)
      return values[0];

    // sort values
    sort(values.begin(),values.end());

    return quantile_sorted(values,Q);
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

    return quantile(values2,Q);
  }

  double median(const valarray<double>& values)
  {
    return quantile(values,0.5);
  }

  double median(const vector<double>& values)
  {
    return quantile(values,0.5);
  }

  /// FIXME - this could be faster by a factor of 2 - we are sorting TWICE!
  pair<double,double> confidence_interval(const valarray<double>& values,double P) 
  {
    vector<double> values2(values.size());
    for(int i=0;i<values.size();i++)
      values2[i] = values[i];

    return confidence_interval(values2,P);
  }

  pair<double,double> confidence_interval(vector<double> values,double P) 
  {
    double alpha = (1.0-P)/2.0;

    sort(values.begin(),values.end());

    pair<double,double> interval;
    interval.first = quantile_sorted(values,alpha);
    interval.second = quantile_sorted(values,1.0-alpha);
    
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

  // Would it be faster to simply compute \sum x[i]*x[i+k]/(N-k) - mu^2?>
  // Well, the problem with this is that you never stop when rho[k] gets negative.

  vector<double> autocovariance(const valarray<double>& x,unsigned max)
  {
    const int N = x.size();

    // specify sample size if unspecified
    if (max==0) 
      max = std::max(1+N/4,N-15);

    if (max >= N) 
      max = N-1;

    // compute mean of X
    double mean = 0;
    for(int i=0;i<N;i++)
      mean += x[i];
    mean /= N;

    // allocate covariances
    vector<double> rho(max);

    // compute each autocorrelation rho[k]
    for(int k=0;k<max;k++) 
    {
      double total = 0;
      for(int i=0;i<N-k;i++)
	total += (x[i]-mean)*(x[i+k]-mean);

      rho[k] = total/(N-k);

      if (rho[k] < 0.0 and k>0) {
	rho.resize(k);
	break;
      }
    }

    return rho;
  }

  vector<double> autocovariance(const vector<double>& x,unsigned max)
  {
    const int N = x.size();

    // specify sample size if unspecified
    if (max==0) 
      max = std::max(1+N/4,N-15);

    if (max >= N) 
      max = N-1;

    // compute mean of X
    double mean = 0;
    for(int i=0;i<N;i++)
      mean += x[i];
    mean /= N;

    // allocate covariances
    vector<double> rho(max);

    // compute each autocorrelation rho[k]
    for(int k=0;k<max;k++) 
    {
      double total = 0;
      for(int i=0;i<N-k;i++)
	total += (x[i]-mean)*(x[i+k]-mean);

      rho[k] = total/(N-k);

      if (rho[k] < 0.0 and k>0) {
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

  vector<double> autocorrelation(const vector<double>& x,unsigned N)
  {
    vector<double> rho = autocovariance(x,N);
    const double V = rho[0];
    for(int i=0;i<rho.size();i++)
      rho[i] /= V;
    return rho;
  }

  double autocorrelation_time(const valarray<double>& x,unsigned max)
  {
    if (x.size() < 2) return 1.0;

    vector<double> cv = autocovariance(x,max);

    double V = cv[0];

    double sum = 0;
    for(int i=1;i<cv.size();i++)
      sum += cv[i];

    return (1.0 + 2.0*sum/V);
  }

  double autocorrelation_time(const vector<double>& x,unsigned max)
  {
    if (x.size() < 2) return 1.0;

    vector<double> cv = autocovariance(x,max);

    double V = cv[0];

    double sum = 0;
    for(int i=1;i<cv.size();i++)
      sum += cv[i];

    return (1.0 + 2.0*sum/V);
  }

  double probability_x_less_than_y(const std::valarray<double>& x, const std::valarray<double>& y)
  {
    vector<double> x2(x.size());
    for(int i=0;i<x2.size();i++)
      x2[i] = x[i];
    sort(x2.begin(),x2.end());

    vector<double> y2(y.size());
    for(int i=0;i<y2.size();i++)
      y2[i] = y[i];
    sort(y2.begin(),y2.end());

    valarray<double> FX(y.size());
    int dx=0;
    for(int dy=0;dy<FX.size();dy++) {
      while((dx<x2.size()) and (x2[dx]<y2[dy]))
	dx++;
      FX[dy] = double(dx)/x2.size();
    }
    return FX.sum()/FX.size();
  }
}

