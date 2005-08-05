#include <cmath>
#include <cassert>
#include "statistics.H"

using std::valarray;
using std::vector;

//FIXME - add 'hat'?
namespace statistics {

  double fraction(const valarray<bool>& v) {
    return double(count(v))/v.size();
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

  vector<double> confidence_interval(const valarray<double>& values,double P) {
    assert(values.size() > 4);

    vector<double> values2(values.size());
    for(int i=0;i<values.size();i++)
      values2[i] = values[i];
    std::sort(values2.begin(),values2.end());
    
    int skip = 1+(int)(values.size()*(1.0-P)/2);
    
    vector<double> interval(2);
    interval[0] = values2[skip];
    interval[1] = values2[values.size()-1-skip];
    
    return interval;
  }

  double lower_confidence_bound(const valarray<double>& values,double P) {
    assert(values.size() > 4);

    vector<double> values2(values.size());
    for(int i=0;i<values.size();i++)
      values2[i] = values[i];
    std::sort(values2.begin(),values2.end());
    
    int skip = 1+(int)(values.size()*(1.0-P));
    return values2[skip];
  }
  
  double upper_confidence_bound(const valarray<double>& values,double P) {
    assert(values.size() > 4);

    vector<double> values2(values.size());
    for(int i=0;i<values.size();i++)
      values2[i] = values[i];
    std::sort(values2.begin(),values2.end());
    
    int skip = 1+(int)(values.size()*(1.0-P));
    return values2[values2.size()-1-skip];
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

  std::vector<int> total_times(const std::valarray<bool>& v) 
  {
    vector<int> total(v.size()+1);

    total[0] = 0;
    for(int i=0;i<v.size();i++) {
      total[i+1] = total[i];
      if (v[i]) total[i+1]++;
    }
      
    return total;
  }

  std::vector<int> regeneration_times(const std::valarray<bool>& v) 
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

}

