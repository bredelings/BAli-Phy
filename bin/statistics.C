#include <cmath>
#include "statistics.H"

using std::valarray;

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
    log(odds(v));
  }

  double Var(const valarray<double>& v) {
    double m1 = moment(v,1);
    double m2 = moment(v,2);
    return m2 - m1*m1;
  }

}

