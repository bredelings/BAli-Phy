#include <cmath>
#include <cassert>
#include "statistics.H"

using std::valarray;
using std::vector;

//FIXME - add 'hat'?
namespace statistics {

  unsigned count(const valarray<bool>& v) {
    int sum=0;
    for(int i=0;i<v.size();i++)
      if (v[i]) sum++;
    return sum;
  }
  
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

  double Var(const valarray<double>& v) {
    double m1 = moment(v,1);
    double m2 = moment(v,2);
    return m2 - m1*m1;
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
      sample2[i] = sample1[i];
    
    for(int j=0;j<pseudocount;i++,j++)
      sample2[i] = false;
    
    return sample2;
  }


}

