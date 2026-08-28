{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment (ComponentStateSequence(ComponentStateSequence))
import Bio.Alignment.Pairwise (left_aligned_pairwise_alignment)
import Bio.Alphabet (dna, gapCharIndex, missingCharIndex, mkNumeric)
import Bio.Sequence (bitmaskFromSequence, emptyAmbiguityDatabase)
import Compiler.Num
import qualified Data.Vector.Unboxed as U
import Foreign.Vector (EVector, listToVector)
import Numeric.LinearAlgebra (Matrix, (><))
import Numeric.LinearAlgebra.Data (NativeMatrix, nativeMatrix)
import Numeric.ProbDensity (collapseDensity)
import qualified Probability.Distribution.PhyloCTMC.FixedA.Sample as FixedSample
import qualified Probability.Distribution.PhyloCTMC.VariableA.Sample as VariableSample
import SModel.Likelihood.CLV (CondLikes)
import SModel.Likelihood.FixedA
    (calcProb, calcProbAtRoot, calcProbAtRootVariable, peelBranchAwayFromRoot,
     sampleRootSequence, sampleSequence, sampleSequenceTowardRoot,
     simpleSequenceLikelihoods)
import System.IO (print)

-- Exercise every component/state producer not reached by the MCMC cat-states
-- test using deterministic one-component, one-state probabilities.
main = do
    let frequencies = (1 >< 1) [1] :: Matrix Double
        transition = (1 >< 1) [1] :: Matrix Double
        transitions = listToVector [nativeMatrix transition]
            :: EVector (NativeMatrix Double)
        letters = U.fromList [0] :: U.Vector Int
        smap = U.fromList [0] :: U.Vector Int
        leaf = simpleSequenceLikelihoods dna (emptyAmbiguityDatabase dna) smap 1
            (letters, bitmaskFromSequence letters)
        nodeLikes = listToVector [leaf] :: EVector CondLikes
        branchLikes = listToVector [] :: EVector CondLikes
        compressedColumns = U.slice 1 1 (U.fromList [99,0,99])
        compressedCounts = U.slice 1 1 (U.fromList [99,1,99])
        slicedParent = ComponentStateSequence
            (U.zip (U.slice 1 1 (U.fromList [99,0,99]))
                   (U.slice 2 1 (U.fromList [99,99,0,99])))
        deletionParent = ComponentStateSequence
            (U.zip (U.slice 1 2 (U.fromList [99,0,0,99]))
                   (U.slice 2 2 (U.fromList [99,99,0,0,99])))
        posteriorRoot = sampleRootSequence nodeLikes branchLikes
            frequencies compressedColumns
        posteriorChild = sampleSequence slicedParent nodeLikes transitions
            branchLikes compressedColumns

    print posteriorRoot
    print posteriorChild
    print (collapseDensity (calcProbAtRoot nodeLikes branchLikes frequencies compressedCounts))
    print (collapseDensity (calcProbAtRootVariable nodeLikes branchLikes frequencies
                                                   compressedCounts))
    print (collapseDensity (calcProb nodeLikes branchLikes frequencies compressedCounts))

    -- Protect arbitrary-root non-reversible sampling with deterministic transitions; a row/column
    -- reversal or an extra root-frequency factor changes these probability-one results.
    let nonRevAlphabet = mkNumeric 3
        nonRevFrequencies = (1 >< 3) [1,0,0] :: Matrix Double
        nonRevTransition = (3 >< 3) [0,1,0, 0,0,1, 1,0,0] :: Matrix Double
        nonRevTransitions = listToVector [nativeMatrix nonRevTransition]
            :: EVector (NativeMatrix Double)
        nonRevSMap = U.fromList [0,1,2] :: U.Vector Int
        unknown = U.fromList [missingCharIndex] :: U.Vector Int
        gap = U.fromList [gapCharIndex] :: U.Vector Int
        nonRevUnknown = simpleSequenceLikelihoods nonRevAlphabet
            (emptyAmbiguityDatabase nonRevAlphabet) nonRevSMap 1
            (unknown, bitmaskFromSequence unknown)
        nonRevGap = simpleSequenceLikelihoods nonRevAlphabet
            (emptyAmbiguityDatabase nonRevAlphabet) nonRevSMap 1
            (U.empty, bitmaskFromSequence gap)
        nonRevUnknownLikes = listToVector [nonRevUnknown] :: EVector CondLikes
        nonRevGapLikes = listToVector [nonRevGap] :: EVector CondLikes
        rootward = peelBranchAwayFromRoot nonRevUnknownLikes branchLikes
            nonRevTransitions nonRevFrequencies
        missingRootward = peelBranchAwayFromRoot nonRevGapLikes branchLikes
            nonRevTransitions nonRevFrequencies
        sampledChild = sampleRootSequence nonRevUnknownLikes (listToVector [rootward])
            nonRevFrequencies compressedColumns
        sampledChildWithMissingRoot = sampleRootSequence nonRevUnknownLikes
            (listToVector [missingRootward]) nonRevFrequencies compressedColumns
        sampledRoot = sampleSequenceTowardRoot sampledChild nonRevUnknownLikes
            nonRevTransitions branchLikes nonRevFrequencies compressedColumns

    print sampledChild
    print sampledChildWithMissingRoot
    print sampledRoot

    simulatedRoot <- FixedSample.simulateRootSequence 1 frequencies
    simulatedFixed <- FixedSample.simulateFixedSequenceFrom slicedParent
        transitions frequencies
    simulatedVariable <- VariableSample.simulateSequenceFrom slicedParent
        (left_aligned_pairwise_alignment 1 1) transitions frequencies
    simulatedDeletion <- VariableSample.simulateSequenceFrom deletionParent
        (left_aligned_pairwise_alignment 2 1) transitions frequencies

    print simulatedRoot
    print simulatedFixed
    print simulatedVariable
    print simulatedDeletion
