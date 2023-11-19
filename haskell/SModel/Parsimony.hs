module SModel.Parsimony where

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Data.Matrix
import Data.Array
import Foreign.Vector
import Numeric.LogDouble
import Data.Maybe (maybeToList)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data CondPars

type MutCosts = Matrix Int

foreign import bpcall "Parsimony:" unitCostMatrix :: Alphabet -> MutCosts

foreign import bpcall "Parsimony:" peelMuts :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" mutsRoot :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> MutCosts -> Int


cached_conditional_muts t seqs as alpha cost = let pc    = IntMap.fromSet pcf $ getEdgesSet t
                                                   pcf b = let inEdges = edgesBeforeEdgeSet t b
                                                               cpsIn = IntMap.restrictKeysToVector pc inEdges
                                                               asIn  = IntMap.restrictKeysToVector as inEdges
                                                               node = sourceNode t b
                                                               sequences = maybeToList $ seqs IntMap.! node
                                                           in peelMuts (list_to_vector sequences) alpha asIn cpsIn cost
                                               in pc

peel_muts t cp as root seqs alpha cost = let inEdges = edgesTowardNodeSet t root
                                             cpsIn = IntMap.restrictKeysToVector cp inEdges
                                             asIn  = IntMap.restrictKeysToVector as inEdges
                                             sequences = maybeToList $ seqs IntMap.! root
                                         in mutsRoot (list_to_vector sequences) alpha asIn cpsIn cost

parsimony t seqs as alpha cost = let pc = cached_conditional_muts t seqs as alpha cost
                                     root = head $ getNodes t
                                 in peel_muts t pc as root seqs alpha cost
----
type ColumnCounts = EVector Int
foreign import bpcall "Parsimony:" calc_root_muts_fixed_A :: CondPars -> CondPars -> CondPars -> MutCosts -> ColumnCounts -> Int
foreign import bpcall "Parsimony:" calc_leaf_muts_fixed_A :: Alphabet -> EVector Int -> CBitVector -> CondPars -> MutCosts -> ColumnCounts -> Int

foreign import bpcall "Parsimony:" peelMutsFixedA :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector CondPars -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" mutsRootFixedA :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector CondPars -> MutCosts -> EVector Int -> Int

cached_conditional_muts_fixed_A t seqs alpha cost =
    let pc    = IntMap.fromSet pcf $ getEdgesSet t
        pcf b = let inEdges = edgesBeforeEdgeSet t b
                    clsIn = IntMap.restrictKeysToVector pc inEdges
                    node = sourceNode t b
                    sequences = if is_leaf_node t node then [c_pair' $ seqs IntMap.! node] else []
                in peelMutsFixedA (list_to_vector sequences) alpha clsIn cost
    in pc

peel_muts_fixed_A t cp root seqs alpha cost counts = let inEdges = edgesTowardNodeSet t root
                                                         clsIn = IntMap.restrictKeysToVector cp inEdges
                                                         sequences = if is_leaf_node t root then [c_pair' $ seqs IntMap.! root] else []
                                                     in mutsRootFixedA (list_to_vector sequences) alpha clsIn cost counts

parsimony_fixed_A :: IsTree t => t -> IntMap (EVector Int, CBitVector) -> Alphabet -> MutCosts -> ColumnCounts -> Int
parsimony_fixed_A t seqs alpha cost counts = let pc = cached_conditional_muts_fixed_A t seqs alpha cost
                                                 root = head $ getNodes t
                                             in peel_muts_fixed_A t pc root seqs alpha cost counts
{-
parsimony_SEV :: IsTree t => t -> IntMap (EVector Int) -> IntMap PairwiseAlignment -> Alphabet -> MutCosts -> Int
parsimony_SEV t seqs as alpha cost = let pc = cached_conditional_muts_SEV t seqs as alpha cost
                                        root = head $ getNodes t
                                     in peel_muts_SEV t pc as root seqs alpha cost
-}
