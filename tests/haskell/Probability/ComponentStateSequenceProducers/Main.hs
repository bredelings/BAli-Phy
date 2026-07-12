{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (ComponentStateSequence(ComponentStateSequence))
import Bio.Alignment.Pairwise (left_aligned_pairwise_alignment)
import Bio.Alphabet (dna)
import Bio.Sequence (bitmaskFromSequence)
import Compiler.Num
import qualified Data.Vector.Unboxed as U
import Foreign.Pair (c_pair)
import Foreign.Vector (EVector, listToVector)
import Numeric.LinearAlgebra (Matrix, (><))
import Numeric.LinearAlgebra.Data (NativeMatrix, nativeMatrix)
import qualified Probability.Distribution.PhyloCTMC.FixedA.Sample as FixedSample
import qualified Probability.Distribution.PhyloCTMC.VariableA.Sample as VariableSample
import SModel.Likelihood.CLV (CondLikes)
import SModel.Likelihood.FixedA
    (sampleRootSequence, sampleSequence, simpleSequenceLikelihoods)
import System.IO (print)

-- Exercise every component/state producer not reached by the MCMC cat-states
-- test using deterministic one-component, one-state probabilities.
main = do
    let frequencies = (1 >< 1) [1] :: Matrix Double
        transition = (1 >< 1) [1] :: Matrix Double
        transitions = listToVector [nativeMatrix transition]
            :: EVector (NativeMatrix Double)
        letters = listToVector [0] :: EVector Int
        smap = listToVector [0] :: EVector Int
        leaf = simpleSequenceLikelihoods dna smap 1
            (c_pair letters (bitmaskFromSequence letters))
        nodeLikes = listToVector [leaf] :: EVector CondLikes
        branchLikes = listToVector [] :: EVector CondLikes
        compressedColumns = U.slice 1 1 (U.fromList [99,0,99])
        slicedParent = ComponentStateSequence
            (U.zip (U.slice 1 1 (U.fromList [99,0,99]))
                   (U.slice 2 1 (U.fromList [99,99,0,99])))
        posteriorRoot = sampleRootSequence nodeLikes branchLikes
            frequencies compressedColumns
        posteriorChild = sampleSequence slicedParent nodeLikes transitions
            branchLikes compressedColumns

    print posteriorRoot
    print posteriorChild

    simulatedRoot <- FixedSample.simulateRootSequence 1 frequencies
    simulatedFixed <- FixedSample.simulateFixedSequenceFrom slicedParent
        transitions frequencies
    simulatedVariable <- VariableSample.simulateSequenceFrom slicedParent
        (left_aligned_pairwise_alignment 1 1) transitions frequencies

    print simulatedRoot
    print simulatedFixed
    print simulatedVariable
