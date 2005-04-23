#include "dp-engine.H"

using std::abs;
using std::max;


efloat_t DPengine::Pr_sum_all_paths() const {
  return Pr_total;
}

DPengine::DPengine(const vector<int>& v1,const vector<double>& v2, const Matrix&M, double Temp)
  :HMM(v1,v2,M,Temp),
   Pr_total(0)
{ }


