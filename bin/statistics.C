#include "statistics.H"

using std::valarray;

//FIXME - add 'hat'?
namespace statistics {

  double Pr(const valarray<bool>& v) {
    int sum=0;
    for(int i=0;i<v.size();i++)
      if (v[i]) sum++;
    return double(sum)/v.size();
  }
  
  double Var(const valarray<double>& v) {
    double m1 = moment(v,1);
    double m2 = moment(v,2);
    return m2 - m1*m1;
  }

}

