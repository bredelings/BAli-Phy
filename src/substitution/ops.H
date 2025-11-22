#ifndef SUBSTITUTION_OPS_H
#define SUBSTITUTION_OPS_H

#include <utility>
#include "util/matrix.H"
#include "computation/expression/expression_ref.H"
#include "substitution/cache.H"
#include <Eigen/Dense>

inline void element_assign(Matrix& M1,double d)
{
    M1.fill(d);
}

inline void element_assign(Matrix& M1,const Matrix& M2)
{
    assert(M1.size1() == M2.size1());
    assert(M1.size2() == M2.size2());

    const int size = M1.size();
    double * __restrict__ m1 = M1.begin();
    const double * __restrict__ m2 = M2.begin();

    for(int i=0;i<size;i++)
        m1[i] = m2[i];
}

inline void element_prod_modify(Matrix& M1,const Matrix& M2)
{
    assert(M1.size1() == M2.size1());
    assert(M1.size2() == M2.size2());

    const int size = M1.size();
    double * __restrict__ m1 = M1.begin();
    const double * __restrict__ m2 = M2.begin();

    for(int i=0;i<size;i++)
        m1[i] *= m2[i];
}

inline void element_prod_assign(Matrix& M1,const Matrix& M2,const Matrix& M3)
{
    assert(M1.size1() == M2.size1());
    assert(M1.size2() == M2.size2());

    assert(M1.size1() == M3.size1());
    assert(M1.size2() == M3.size2());

    const int size = M1.size();
    double * __restrict__ m1 = M1.begin();
    const double * __restrict__ m2 = M2.begin();
    const double * __restrict__ m3 = M3.begin();

    for(int i=0;i<size;i++)
        m1[i] = m2[i]*m3[i];
}

inline double element_sum(const Matrix& M1)
{
    const int size = M1.size();
    const double * __restrict__ m1 = M1.begin();

    double sum = 0;
    for(int i=0;i<size;i++)
        sum += m1[i];
    return sum;
}


inline double element_prod_sum(Matrix& M1,const Matrix& M2)
{
    assert(M1.size1() == M2.size1());
    assert(M1.size2() == M2.size2());

    const int size = M1.size();
    const double * __restrict__ m1 = M1.begin();
    const double * __restrict__ m2 = M2.begin();

    double sum = 0;
    for(int i=0;i<size;i++)
        sum += m1[i] * m2[i];

    return sum;
}

inline double element_prod_sum(Matrix& M1,const Matrix& M2,const Matrix& M3)
{
    assert(M1.size1() == M2.size1());
    assert(M1.size2() == M2.size2());

    assert(M1.size1() == M3.size1());
    assert(M1.size2() == M3.size2());

    const int size = M1.size();
    const double * __restrict__ m1 = M1.begin();
    const double * __restrict__ m2 = M2.begin();
    const double * __restrict__ m3 = M3.begin();

    double sum = 0;
    for(int i=0;i<size;i++)
        sum += m1[i] * m2[i] * m3[i];

    return sum;
}

inline double element_prod_sum(Matrix& M1,const Matrix& M2,const Matrix& M3,const Matrix& M4)
{
    assert(M1.size1() == M2.size1());
    assert(M1.size2() == M2.size2());

    assert(M1.size1() == M3.size1());
    assert(M1.size2() == M3.size2());

    assert(M1.size1() == M4.size1());
    assert(M1.size2() == M4.size2());

    const int size = M1.size();
    const double * __restrict__ m1 = M1.begin();
    const double * __restrict__ m2 = M2.begin();
    const double * __restrict__ m3 = M3.begin();
    const double * __restrict__ m4 = M4.begin();

    double sum = 0;
    for(int i=0;i<size;i++)
        sum += m1[i] * m2[i] * m3[i] * m4[i];

    return sum;
}

inline void element_assign(double* M1, double d, int size)
{
    for(int i=0;i<size;i++)
        M1[i] = d;
}

inline void element_assign(double* __restrict__ M1, const double* __restrict__ M2, int size)
{
    for(int i=0;i<size;i++)
        M1[i] = M2[i];
}

inline void element_prod_modify(double* __restrict__ M1, const double* __restrict__ M2, int size)
{
    for(int i=0;i<size;i++)
        M1[i] *= M2[i];
}

inline void element_prod_assign(double* __restrict__ M1,
                                const double* __restrict__ M2,
                                int size)
{
    for(int i=0;i<size;i++)
        M1[i] *= M2[i];
}

inline void element_prod_assign(double* __restrict__ M1,
                                const double* __restrict__ M2,
                                const double* __restrict__ M3, int size)
{
    for(int i=0;i<size;i++)
        M1[i] = M2[i]*M3[i];
}

inline void element_prod_assign(double* __restrict__ M1,
                                const double* __restrict__ M2,
                                const double* __restrict__ M3,
                                const double* __restrict__ M4,
                                int size)
{
    for(int i=0;i<size;i++)
        M1[i] = M2[i]*M3[i]*M4[i];
}

inline void element_prod_assign(double* __restrict__ M1,
                                const double* __restrict__ M2,
                                const double* __restrict__ M3,
                                const double* __restrict__ M4,
                                const double* __restrict__ M5,
                                int size)
{
    for(int i=0;i<size;i++)
        M1[i] = M2[i]*M3[i]*M4[i]*M5[i];
}

inline double element_sum(const double* M1, int size)
{
    double sum = 0;
    for(int i=0;i<size;i++)
        sum += M1[i];
    return sum;
}


inline double element_prod_sum(const double* __restrict__ M1, const double* __restrict__ M2, int size)
{
    double sum = 0;
    for(int i=0;i<size;i++)
        sum += M1[i] * M2[i];

    return sum;
}

inline double element_prod_sum(const double* __restrict__ M1,
                               const double* __restrict__ M2,
                               const double* __restrict__ M3,
                               int size)
{
    double sum = 0;
    for(int i=0;i<size;i++)
        sum += M1[i] * M2[i] * M3[i];

    return sum;
}

inline double element_prod_sum(const double* __restrict__ M1,
                               const double* __restrict__ M2,
                               const double* __restrict__ M3,
                               const double* __restrict__ M4,
                               int size)
{
    double sum = 0;
    for(int i=0;i<size;i++)
        sum += M1[i] * M2[i] * M3[i] * M4[i];

    return sum;
}
template <int N>
inline void propagate_toward_root_fixed(double* R, int n_models, int& scale, const EVector& transition_P, const double* S)
{
    using Eigen::Map;
    using Eigen::ArrayXd;
    using Eigen::RowMajor;

    // 1. Define Types
    //    Using RowMajor matches your data layout exactly.
    typedef Eigen::Matrix<double, N, N, RowMajor> FixedMatrix;
    typedef Eigen::Matrix<double, N, 1> FixedVector;

    // Map the full output array for the final batch scaling
    Map<ArrayXd> r_all(R, n_models * N);
    
    bool need_scale = true;

    for(int m = 0; m < n_models; m++)
    {
        // 1. Get pointer to raw data
        const double* q_data = transition_P[m].as_<Box<Matrix>>().begin();
        
        // 2. Map Q as Row-Major
        Map<const FixedMatrix> Q(q_data);

        // 3. Map S and R vectors for the current model
        int offset = m * N;
        Map<const FixedVector> s_vec(S + offset);
        Map<FixedVector> r_vec(R + offset);

        // 4. Calculate R = Q * S
        r_vec = Q * s_vec;

        // 5. Check Scaling
        if (r_vec.maxCoeff() >= scale_min) {
            need_scale = false;
        }
    }

    // 6. Batch Scale if needed
    if (need_scale)
    {
        scale++;
        r_all *= scale_factor;
    }
}

inline void propagate_toward_root(double* R, int n_models, int n_states, int& scale, const EVector& transition_P, const double* S)
{
    if (n_states==4)
        return propagate_toward_root_fixed<4 >(R, n_models, scale, transition_P, S);
    else if (n_states==20)
        return propagate_toward_root_fixed<20>(R, n_models, scale, transition_P, S);

    using Eigen::Map;
    using Eigen::VectorXd;
    using Eigen::ArrayXd;
    using Eigen::Dynamic;
    using Eigen::RowMajor;

    bool need_scale = true;

    for(int m = 0; m < n_models; m++)
    {
        // 1. Get pointer to raw data
        const double* q_data = transition_P[m].as_<Box<Matrix>>().begin();
        
        // 2. Map Q as Row-Major
        Map<const Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> Q(q_data, n_states, n_states);

        // 3. Map S and R vectors for the current model
        int offset = m * n_states;
        Map<const VectorXd> s_vec(S + offset, n_states);
        Map<VectorXd> r_vec(R + offset, n_states);

        // 4. Calculate R = Q * S
        r_vec = Q * s_vec;

        // 5. Check Scaling
        if (r_vec.maxCoeff() >= scale_min) {
            need_scale = false;
        }
    }

    // 6. Batch Scale if necessary
    if (need_scale)
    {
        scale++;

        Map<ArrayXd> r_all(R, n_models * n_states);
        r_all *= scale_factor;
    }
}

template <int N>
inline void propagate_away_from_root_fixed(double* R, int n_models, int& scale, const EVector& transition_P, const double* S)
{
    using Eigen::Map;
    using Eigen::ArrayXd;
    using Eigen::RowMajor;

    // 1. Define Types
    typedef Eigen::Matrix<double, N, N, RowMajor> FixedMatrix;
    typedef Eigen::Matrix<double, N, 1> FixedVector;

    bool need_scale = true;

    for(int m = 0; m < n_models; m++)
    {
        // 1. Get pointer to raw data
        const double* q_data = transition_P[m].as_<Box<Matrix>>().begin();
        
        // 2. Map Q as Row-Major
        Map<const FixedMatrix> Q(q_data);

        // 3. Map S and R vectors for the current model
        int offset = m * N;
        Map<const FixedVector> s_vec(S + offset);
        Map<FixedVector> r_vec(R + offset);

        // 4. Calculate R = Q.transpose() * S
        r_vec = Q.transpose() * s_vec;

        // 5. Check Scaling
        if (r_vec.maxCoeff() >= scale_min) {
            need_scale = false;
        }
    }

    // 6. Batch Scale if needed
    if (need_scale)
    {
        scale++;

        Map<ArrayXd> r_all(R, n_models * N);
        r_all *= scale_factor;
    }
}

inline void propagate_away_from_root(double* R, int n_models, int n_states, int& scale, const EVector& transition_P, const double* S)
{
    if (n_states==4)
        return propagate_away_from_root_fixed<4 >(R, n_models, scale, transition_P, S);
    else if (n_states==20)
        return propagate_away_from_root_fixed<20>(R, n_models, scale, transition_P, S);

    using Eigen::Map;
    using Eigen::VectorXd;
    using Eigen::ArrayXd;
    using Eigen::Dynamic;
    using Eigen::RowMajor;

    bool need_scale = true;

    for(int m = 0; m < n_models; m++)
    {
        // 1. Get pointer to raw data
        const double* q_data = transition_P[m].as_<Box<Matrix>>().begin();
        
        // 2. Map Q as Row-Major
        Map<const Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> Q(q_data, n_states, n_states);

        // 3. Map S and R vectors for the current model
        int offset = m * n_states;
        Map<const VectorXd> s_vec(S + offset, n_states);
        Map<VectorXd> r_vec(R + offset, n_states);

        // 4. Calculate R = Q.transpose() * S
        r_vec = Q.transpose() * s_vec;

        // 5. Check scaling
        if (r_vec.maxCoeff() >= scale_min) {
            need_scale = false;
        }
    }

    // 6. Batch Scale if necessary
    if (need_scale)
    {
        scale++;

        Map<ArrayXd> r_all(R, n_models * n_states);
        r_all *= scale_factor;
    }
}

Matrix propagate_frequencies(const Matrix& F, const EVector& transition_P);

void calc_transition_prob_from_parent(Matrix& S, const std::pair<int,int>& state_model_parent, const EVector& Ps);
void calc_transition_prob_from_parent(Matrix& S, const std::pair<int,int>& state_model_parent, const EVector& Ps, const Matrix& WF);

std::pair<int,int> sample(const Matrix& M);

#endif
