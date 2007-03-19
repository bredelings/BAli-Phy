#include <iostream>
#include "dp-engine.H"
#include "myexception.H"

using std::cerr;
using std::endl;

efloat_t DPengine::Pr_sum_all_paths() const {
  return Pr_total;
}

void DPengine::check_sampling_probability(const vector<int>& g_path) const
{
  efloat_t P = path_P(g_path);
  efloat_t ratio = path_Q(g_path)/Pr_sum_all_paths();
  double diff = std::abs(log(ratio) - log(P));
  if (std::abs(diff) > 1.0e-9) {
    throw myexception()
      <<" Incorrect sampling probabilities!\n"
      <<" P(sample) = "<<log(P)<<"     P(path)/P(ALL paths) = "<<log(ratio)<<"   diff = "<<diff;
  }
}

DPengine::DPengine(const vector<int>& v1,const vector<double>& v2, const Matrix&M, double Beta)
  :HMM(v1,v2,M,Beta),
   Pr_total(0)
{ }


