module Numeric.Matrix (optimiseMult) where

import qualified Data.Array as A
import Compiler.Floating
import Data.List.NonEmpty
import Data.Monoid
import Numeric.LinearAlgebra.Data

foreign import bpcall "Matrix:" matrixFloatingUnary :: Int -> Matrix Double -> Matrix Double
foreign import bpcall "Matrix:" matrixFloatingBinary :: Int -> Matrix Double -> Matrix Double -> Matrix Double

-- Separate singleton scalar matrices from a chain while preserving the
-- order of every non-scalar matrix.
separateScalars :: Element a => [Matrix a] -> (Maybe (Matrix a), [Matrix a])
separateScalars [] = (Nothing, [])
separateScalars (matrix:matrices)
    | rows matrix == 1 && cols matrix == 1 =
        case separateScalars matrices of
            (Nothing, remaining) -> (Just matrix, remaining)
            (Just factor, remaining) -> (Just (matrixProduct matrix factor), remaining)
    | otherwise =
        case separateScalars matrices of
            (factor, remaining) -> (factor, matrix:remaining)

-- Check every adjacent dimension before any products in the chain are
-- evaluated.
validateChain :: [Matrix a] -> ()
validateChain [] = ()
validateChain [_] = ()
validateChain (left:right:remaining)
    | cols left == rows right = validateChain (right:remaining)
    | otherwise = error $ "Numeric.LinearAlgebra.optimiseMult: incompatible matrix dimensions "
                          ++ show (rows left, cols left) ++ " and "
                          ++ show (rows right, cols right)

-- Find and evaluate the minimum-cost association for a validated,
-- nonempty chain using a memoized cost-and-split table.
multiplyChain :: Element a => [Matrix a] -> Matrix a
multiplyChain [] = error "Numeric.LinearAlgebra.optimiseMult: internal empty matrix chain"
multiplyChain [matrix] = matrix
multiplyChain matrices = evaluate 0 (count-1)
  where
    count = length matrices
    values = A.listArray' matrices
    dimensions = A.mkArray count (\i -> (rows (values A.! i), cols (values A.! i)))
    cellIndex i j = i * count + j
    costs = A.mkArray (count * count) costAt

    -- Compute one dynamic-programming cell from shorter memoized subchains.
    costAt index
        | i >= j = (0, i)
        | otherwise = minimum [candidate i j split | split <- [i..j-1]]
      where
        i = index `div` count
        j = index `mod` count

    -- Estimate one split with Integer arithmetic to avoid dimension-cost
    -- overflow even though native matrix dimensions use Int.
    candidate i j split =
        let (leftCost, _) = costs A.! cellIndex i split
            (rightCost, _) = costs A.! cellIndex (split+1) j
            (leftRows, _) = dimensions A.! i
            (_, leftColumns) = dimensions A.! split
            (_, rightColumns) = dimensions A.! j
            multiplicationCost = toInteger leftRows * toInteger leftColumns * toInteger rightColumns
        in (leftCost + rightCost + multiplicationCost, split)

    -- Follow the recorded splits and delegate each binary product to Eigen.
    evaluate i j
        | i == j = values A.! i
        | otherwise =
            let (_, split) = costs A.! cellIndex i j
            in matrixProduct (evaluate i split) (evaluate (split+1) j)

-- Optimize a nonempty chain after validating all non-scalar dimensions,
-- then apply its combined singleton factor once.
optimiseNonEmpty :: Element a => [Matrix a] -> Matrix a
optimiseNonEmpty matrices =
    case separateScalars matrices of
        separated@(_, chain) -> validateChain chain `seq`
            case separated of
                (Nothing, []) -> error "Numeric.LinearAlgebra.optimiseMult: internal empty matrix chain"
                (Just scalarMatrix, []) -> scalarMatrix
                (Nothing, _) -> multiplyChain chain
                (Just scalarMatrix, _) -> matrixProduct scalarMatrix (multiplyChain chain)

-- Multiply a matrix list using its least-cost association, with a singleton
-- one as the identity for an empty chain.
optimiseMult :: (Element a, Num a) => [Matrix a] -> Matrix a
optimiseMult [] = scalar 1
optimiseMult matrices = optimiseNonEmpty matrices

instance Show (Matrix a) where
    show value = unpack_cpp_string (showMatrix value)

instance (Element a, Num a) => Num (Matrix a) where
    fromInteger value = scalar (fromInteger value)
    negate = mat_negate
    abs = mat_abs
    signum = mat_signum
    (+) = elementwise_add
    (-) = elementwise_sub
    (*) = elementwise_multiply

instance Fractional (Matrix Double) where
    (/) = matrixFloatingBinary 0
    recip = matrixFloatingUnary 19

instance Floating (Matrix Double) where
    pi = scalar 3.14159265358979323846
    exp = matrixFloatingUnary 0
    sqrt = matrixFloatingUnary 1
    log = matrixFloatingUnary 2
    (**) = matrixFloatingBinary 1
    logBase = matrixFloatingBinary 2
    sin = matrixFloatingUnary 3
    tan = matrixFloatingUnary 4
    cos = matrixFloatingUnary 5
    asin = matrixFloatingUnary 6
    atan = matrixFloatingUnary 7
    acos = matrixFloatingUnary 8
    sinh = matrixFloatingUnary 9
    tanh = matrixFloatingUnary 10
    cosh = matrixFloatingUnary 11
    asinh = matrixFloatingUnary 12
    atanh = matrixFloatingUnary 13
    acosh = matrixFloatingUnary 14
    log1p = matrixFloatingUnary 15
    expm1 = matrixFloatingUnary 16
    log1pexp = matrixFloatingUnary 17
    log1mexp = matrixFloatingUnary 18

instance Element a => Semigroup (Matrix a) where
    (<>) = matrixProduct
    sconcat (matrix :| matrices) = optimiseNonEmpty (matrix:matrices)

instance (Element a, Num a) => Monoid (Matrix a) where
    mempty = scalar 1
    mconcat = optimiseMult
