{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alphabet (dna)
import Bio.Sequence (bitmaskFromSequence, emptyAmbiguityDatabase, stripGaps)
import Compiler.Floating (Pow(ln))
import Compiler.Num
import Data.Bool (Bool)
import Data.Ord
import qualified Data.Vector.Unboxed as U
import Foreign.Vector (EVector, listToVector)
import Numeric.LinearAlgebra (Matrix, ident, (><))
import Numeric.LinearAlgebra.Data (NativeMatrix, nativeMatrix)
import Numeric.ProbDensity (collapseDensity, reciprocalDensity)
import SModel.Likelihood.CLV (CondLikes)
import SModel.Likelihood.FixedA (calcProbAtRoot, calcProbAtRootVariable,
                                 calcProbVariable, peelBranchAwayFromRoot,
                                 peelBranchTowardRoot, simpleSequenceLikelihoods)
import System.IO (print)

-- Protect scaling of compressed likelihood patterns and the opaque density
-- reciprocal used by conditioned PhyloCTMC distributions.
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
        letters = U.fromList [0] :: U.Vector Int
        smap = U.fromList [0,1,2,3] :: U.Vector Int
        leaf = simpleSequenceLikelihoods dna (emptyAmbiguityDatabase dna) smap 1
            (letters, bitmaskFromSequence letters)
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
        scaleError = abs (ln (collapseDensity twice) - 2 * ln (collapseDensity once))
        reciprocalError = abs (ln (collapseDensity (reciprocalDensity once))
                               + ln (collapseDensity once))

    print (max scaleError reciprocalError < 1.0e-12 :: Bool)

    -- Protect variable-site normalization from depending on the collection node, which root-only
    -- producer coverage cannot test. These calculations also exercise a missing rootward CLV.
    let nonEqFrequencies = (1 >< 4) [0.1, 0.4, 0.25, 0.25] :: Matrix Double
        nonRevTransition = (4 >< 4)
            [ 0.7, 0.1, 0.1, 0.1
            , 0.2, 0.5, 0.2, 0.1
            , 0.1, 0.2, 0.4, 0.3
            , 0.3, 0.2, 0.1, 0.4
            ] :: Matrix Double
        nonRevTransitions = listToVector [nativeMatrix nonRevTransition]
            :: EVector (NativeMatrix Double)
        constantLetters = U.fromList [0,1,2,3] :: U.Vector Int
        constantLeaf = simpleSequenceLikelihoods dna (emptyAmbiguityDatabase dna) smap 1
            (constantLetters, bitmaskFromSequence constantLetters)
        constantLeafLikes = listToVector [constantLeaf] :: EVector CondLikes
        missingLetters = U.fromList [-1,-1,-1,-1] :: U.Vector Int
        missingLeaf = simpleSequenceLikelihoods dna (emptyAmbiguityDatabase dna) smap 1
            (stripGaps missingLetters, bitmaskFromSequence missingLetters)
        missingLeafLikes = listToVector [missingLeaf] :: EVector CondLikes
        variableCounts = U.fromList [1]
        childTowardRoot = peelBranchTowardRoot constantLeafLikes emptyLikes
            nonRevTransitions
        rootAway = peelBranchAwayFromRoot constantLeafLikes emptyLikes
            nonRevTransitions nonEqFrequencies
        missingRootAway = peelBranchAwayFromRoot missingLeafLikes emptyLikes
            nonRevTransitions nonEqFrequencies
        atRoot = calcProbAtRootVariable constantLeafLikes (listToVector [childTowardRoot])
            nonEqFrequencies variableCounts
        genericAtRoot = calcProbVariable constantLeafLikes (listToVector [childTowardRoot])
            nonEqFrequencies variableCounts
        awayFromRoot = calcProbVariable constantLeafLikes (listToVector [rootAway])
            nonEqFrequencies variableCounts
        awayFromMissingRoot = calcProbVariable constantLeafLikes
            (listToVector [missingRootAway, childTowardRoot]) nonEqFrequencies variableCounts
        -- At the root, constant x has probability fₓPₓₓ. With the rootward observation missing,
        -- f propagates to [0.25,0.31,0.215,0.225], so constant x instead has probability (fP)ₓPₓₓ.
        expected = 1 - (0.1*0.7 + 0.4*0.5 + 0.25*0.4 + 0.25*0.4) :: Double
        expectedMissingRoot = 1 - (0.25*0.7 + 0.31*0.5 + 0.215*0.4 + 0.225*0.4) :: Double
        atRootError = abs (ln (collapseDensity atRoot) - ln expected)
        genericRootError = abs (ln (collapseDensity genericAtRoot) - ln expected)
        awayRootError = abs (ln (collapseDensity awayFromRoot) - ln expected)
        missingRootError = abs (ln (collapseDensity awayFromMissingRoot) - ln expectedMissingRoot)

    print (max atRootError (max genericRootError (max awayRootError missingRootError))
           < 1.0e-12 :: Bool)
