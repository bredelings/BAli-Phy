{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet (dna)
import Bio.Sequence (bitmaskFromSequence)
import Compiler.Floating (Pow(ln))
import Compiler.Num
import Data.Bool (Bool)
import Data.Ord
import qualified Data.Vector.Unboxed as U
import Foreign.Pair (c_pair)
import Foreign.Vector (EVector, listToVector)
import Numeric.LinearAlgebra (Matrix, ident, (><))
import Numeric.LinearAlgebra.Data (NativeMatrix, nativeMatrix)
import SModel.Likelihood.CLV (CondLikes)
import SModel.Likelihood.FixedA (calcProbAtRoot, peelBranchTowardRoot,
                                 simpleSequenceLikelihoods)
import System.IO (print)

-- Build a scaled conditional-likelihood cache and compare one compressed
-- pattern of multiplicity two with two copies of multiplicity one.
main = do
    let epsilon = 1.0e-100
        frequencies = (1 >< 4) [0.25, 0.25, 0.25, 0.25] :: Matrix Double
        tinyTransition = (4 >< 4)
            [ epsilon, 1-epsilon, 0, 0
            , epsilon, 1-epsilon, 0, 0
            , epsilon, 1-epsilon, 0, 0
            , epsilon, 1-epsilon, 0, 0
            ] :: Matrix Double
        tinyTransitions = listToVector [nativeMatrix tinyTransition]
            :: EVector (NativeMatrix Double)
        identityTransitions = listToVector [nativeMatrix (ident 4)]
            :: EVector (NativeMatrix Double)
        letters = listToVector [0] :: EVector Int
        smap = listToVector [0,1,2,3] :: EVector Int
        leaf = simpleSequenceLikelihoods dna smap 1
            (c_pair letters (bitmaskFromSequence letters))
        emptyLikes = listToVector [] :: EVector CondLikes
        leafLikes = listToVector [leaf] :: EVector CondLikes
        tinyBranch = peelBranchTowardRoot leafLikes emptyLikes tinyTransitions
        tinyBranchLikes = listToVector [tinyBranch] :: EVector CondLikes
        scaledBranch = peelBranchTowardRoot emptyLikes tinyBranchLikes
            identityTransitions
        scaledBranchLikes = listToVector [scaledBranch] :: EVector CondLikes
        once = calcProbAtRoot emptyLikes scaledBranchLikes frequencies
            (U.fromList [1])
        twice = calcProbAtRoot emptyLikes scaledBranchLikes frequencies
            (U.fromList [2])
        scaleError = abs (ln twice - 2 * ln once)

    print (scaleError < 1.0e-12 :: Bool)
