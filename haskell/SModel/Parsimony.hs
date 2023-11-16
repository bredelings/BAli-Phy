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

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data CondPars

type MutCosts = Matrix Int

foreign import bpcall "Parsimony:" unitCostMatrix :: Alphabet -> MutCosts

foreign import bpcall "Parsimony:" peel_muts_leaf_branch :: EVector Int -> Alphabet -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" peel_muts_internal_branch :: CondPars -> CondPars -> PairwiseAlignment -> PairwiseAlignment -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" calc_root_muts :: CondPars -> CondPars -> CondPars -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> MutCosts -> Int
foreign import bpcall "Parsimony:" calc_leaf_muts :: Alphabet -> EVector Int -> PairwiseAlignment -> MutCosts -> CondPars -> Int
foreign import bpcall "Parsimony:" peelMuts :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" mutsRoot :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> MutCosts -> Int


cached_conditional_muts t seqs as alpha cost = let pc    = IntMap.fromSet pcf $ getEdgesSet t
                                                   pcf b = let inEdges = edgesBeforeEdgeSet t b
                                                               cpsIn = IntMap.restrictKeysToVector pc inEdges
                                                               asIn  = IntMap.restrictKeysToVector as inEdges
                                                               node = sourceNode t b
                                                               sequences = if is_leaf_node t node then [seqs IntMap.! node] else []
                                                           in peelMuts (list_to_vector sequences) alpha asIn cpsIn cost
                                               in pc

peel_muts t cp as root seqs alpha cost = let inEdges = edgesTowardNodeSet t root
                                             cpsIn = IntMap.restrictKeysToVector cp inEdges
                                             asIn  = IntMap.restrictKeysToVector as inEdges
                                             sequences = if is_leaf_node t root then [seqs IntMap.! root] else []
                                         in mutsRoot (list_to_vector sequences) alpha asIn cpsIn cost

parsimony :: IsTree t => t -> IntMap (EVector Int) -> IntMap PairwiseAlignment -> Alphabet -> MutCosts -> Int
parsimony t seqs as alpha cost = let pc = cached_conditional_muts t seqs as alpha cost
                                     root = head $ getNodes t
                                 in peel_muts t pc as root seqs alpha cost
----
type ColumnCounts = EVector Int
foreign import bpcall "Parsimony:" peel_muts_leaf_branch_fixed_A :: EVector Int -> CBitVector -> Alphabet -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" peel_muts_internal_branch_fixed_A :: CondPars -> CondPars -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" calc_root_muts_fixed_A :: CondPars -> CondPars -> CondPars -> MutCosts -> ColumnCounts -> Int
foreign import bpcall "Parsimony:" calc_leaf_muts_fixed_A :: Alphabet -> EVector Int -> CBitVector -> CondPars -> MutCosts -> ColumnCounts -> Int

cached_conditional_muts_fixed_A t seqs alpha cost =
    let pc    = IntMap.fromSet pcf $ getEdgesSet t
        pcf b = case edgesBeforeEdge t b of
                     [] -> let n = sourceNode t b
                               (seq,mask) = seqs IntMap.! n
                           in peel_muts_leaf_branch_fixed_A seq mask alpha cost
                     [_] -> error "cached_conditional_muts_fixed_A: knuckles not handled yet."
                     [b1,b2] -> peel_muts_internal_branch_fixed_A (pc IntMap.! b1) (pc IntMap.! b2) cost
                     e -> error $ "cached_conditional_muts: " ++ show (length e)  ++ "edges before edge not handled."
    in pc

peel_muts_fixed_A t cp root seqs alpha cost counts = let muts = IntMap.fromSet peel_muts' $ getNodesSet t
                                                         peel_muts' root = case edgesTowardNode t root of
                                                                                [b1] -> let n = targetNode t b1
                                                                                            (seq,mask) = seqs IntMap.! n
                                                                                        in calc_leaf_muts_fixed_A alpha seq mask (cp IntMap.! b1) cost counts
                                                                                [b1,b2,b3] -> calc_root_muts_fixed_A (cp IntMap.! b1) (cp IntMap.! b2) (cp IntMap.! b3) cost counts
                                                                                [] -> 0
                                                                                [_,_] -> error "peel_muts: root node with 2 edges"
                                                                                e -> error $ "peel_muts: root node has degree " ++ show (length e)
                                                      in muts IntMap.! root

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
