#ifndef SUBSTITUTION_OPS_H
#define SUBSTITUTION_OPS_H

#include <utility>
#include "util/matrix.H"

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

std::pair<int,int> sample(const Matrix& M);

#endif
