#ifndef DENSE_MATRIX_H
#define DENSE_MATRIX_H

#include <Eigen/Core>

namespace bali_phy
{

    // Store dynamically sized numeric vectors in the representation used by
    // Numeric.LinearAlgebra.Vector runtime values.
    template <typename T>
    using DenseVector = Eigen::Matrix<T, Eigen::Dynamic, 1>;

    // Store Numeric.LinearAlgebra.Matrix runtime values contiguously in
    // row-major order while exposing Eigen's numerical operations.
    template <typename T>
    using DenseMatrix =
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;

}

using bali_phy::DenseVector;
using bali_phy::DenseMatrix;

#endif
