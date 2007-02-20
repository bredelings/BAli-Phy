#ifndef DPMATRIX_H
#define DPMATRIX_H

#include "hmm.H"


class DPengine: public HMM 
{
protected:
  efloat_t Pr_total;

public:
  /// Sample a path from the HMM
  virtual vector<int> sample_path() const =0;

  /// Calculates the (log) probability of all possible paths through theHMM
  virtual efloat_t Pr_sum_all_paths() const;

  /// Calculates the (log) probability that this path is sampled by the generalized HMM
  virtual efloat_t path_P(const vector<int>& g_path) const =0;

  /// Calculate the (log) substitution probability along the path
  virtual efloat_t path_Q_subst(const vector<int>& g_path) const=0;

  /// Calculate the (log) probability of this path
  virtual efloat_t path_Q(const vector<int>& g_path) const {
    return path_GQ_path(g_path) * this->path_Q_subst(g_path);
  }

  void check_sampling_probability(const vector<int>& g_path) const;

  DPengine(const vector<int>&,const vector<double>&,const Matrix&,double Temp);
};

#endif
