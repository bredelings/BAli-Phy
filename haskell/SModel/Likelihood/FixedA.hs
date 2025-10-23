module SModel.Likelihood.FixedA  where 

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Data.Matrix
import Data.Maybe (maybeToList)
import Data.Array
import Foreign.Vector
import Numeric.LogDouble
import Bio.Sequence

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import SModel.Likelihood.CLV

-- peeling for SEV
foreign import bpcall "LikelihoodSEV:" calcProbAtRoot :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "LikelihoodSEV:" calcProbAtRootVariable :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "LikelihoodSEV:" calcProb :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "LikelihoodSEV:" peelBranchTowardRoot :: EVector CondLikes -> EVector CondLikes -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "LikelihoodSEV:" peelBranchAwayFromRoot :: EVector CondLikes -> EVector CondLikes -> EVector (Matrix Double) -> Matrix Double -> CondLikes

peelBranch toward nodeCLs branchCLs ps f | toward    = peelBranchTowardRoot   nodeCLs branchCLs ps
                                         | otherwise = peelBranchAwayFromRoot nodeCLs branchCLs ps f

-- ancestral sequence sampling for SEV
foreign import bpcall "LikelihoodSEV:" sampleRootSequence :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "LikelihoodSEV:" sampleSequence :: VectorPairIntInt -> EVector CondLikes -> EVector (Matrix Double) -> EVector CondLikes -> EVector Int -> VectorPairIntInt

foreign import bpcall "LikelihoodSEV:" simpleSequenceLikelihoods :: Alphabet -> EVector Int -> Int -> EPair (EVector Int) CBitVector -> CondLikes

-- Could we move the conversion from sequence-with-gaps to (sequence,bitvector) into here?
simpleNodeCLVs :: Alphabet -> EVector Int -> Int -> IntMap (Maybe (EVector Int, CBitVector)) -> IntMap (Maybe CondLikes)
simpleNodeCLVs alpha smap nModels seqs = (sequenceToCL <$>) <$> seqs
    where sequenceToCL = simpleSequenceLikelihoods alpha smap nModels . c_pair'

{- Note:
   An alternative implementation would be to pass in a frequency matrix full of 1.0s.
   But actually, maybe in that case we want to collect the matrices and multiply them all at once, and there
     isn't really a matrix of 1s in the mix.
 -}

cachedConditionalLikelihoodsWith peelBranch t nodeCLVs ps f
    = let clvs = getEdgesSet t & IntMap.fromSet clvForBranch
          clvForBranch b = let p = ps IntMap.! b
                               inEdges = edgesBeforeEdgeSet t b
                               clsIn = IntMap.restrictKeysToVector clvs inEdges
                               node = sourceNode t b
                               nodeCLs = toVector $ maybeToList $ nodeCLVs IntMap.! node
                           in peelBranch b nodeCLs clsIn p f
      in clvs

peelLikelihoodWith calcProb nodeCLVs t cls f alpha smap root counts
    = let inEdges = edgesTowardNodeSet t root
          nModels = nrows f
          nodeCLs = toVector $ maybeToList $ nodeCLVs IntMap.! root
          clsIn = IntMap.restrictKeysToVector cls inEdges
      in calcProb nodeCLs clsIn f counts

-- EqRev
cachedConditionalLikelihoods = cachedConditionalLikelihoodsWith (\b nodeCLs clsIn p f -> peelBranchTowardRoot nodeCLs clsIn p)

peelLikelihood = peelLikelihoodWith calcProbAtRoot

-- EqNonRev + NonEq
                 
cachedConditionalLikelihoodsNonRev t = cachedConditionalLikelihoodsWith (\b -> peelBranch (towardRoot t b)) t

peelLikelihoodNonRev = peelLikelihoodWith calcProb

-- EqRev | variable
                       
peelLikelihoodVariable = peelLikelihoodWith calcProbAtRootVariable

--

sampleAncestralSequences t root nodeCLVs alpha ps f cl smap col_to_compressed =
    let rt = addRoot root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node (getNodesSet t)
        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)
        ancestor_for_branch n Nothing = let nodeCLs = toVector $ maybeToList $ nodeCLVs IntMap.! n
                                            inEdges = edgesTowardNodeSet t n
                                            clsIn = IntMap.restrictKeysToVector cl inEdges
                                        in sampleRootSequence nodeCLs
                                                              clsIn
                                                              f
                                                              col_to_compressed
        ancestor_for_branch n (Just to_p) = let parent_seq = ancestor_seqs IntMap.! (targetNode t to_p)
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                inEdges = edgesBeforeEdgeSet t to_p
                                                clsIn = IntMap.restrictKeysToVector cl inEdges
                                                nodeCLs = toVector $ maybeToList $ nodeCLVs IntMap.! n
                                            in sampleSequence parent_seq
                                                              nodeCLs
                                                              ps_for_b0
                                                              clsIn
                                                              col_to_compressed
    in ancestor_seqs
                                                                       
