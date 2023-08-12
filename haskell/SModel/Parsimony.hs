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


cached_conditional_muts t seqs as alpha cost = let pc    = IntMap.fromSet pcf $ getEdgesSet t
                                                   pcf b = let edges = edgesBeforeEdgeArray t b
                                                               b1 = edges!0
                                                               b2 = edges!1
                                                           in case numElements edges of
                                                                0 -> let n=sourceNode t b
                                                                     in peel_muts_leaf_branch (seqs IntMap.! n) alpha cost
                                                                1 -> error "cached_conditional_muts: knuckles not handled yet."
                                                                2 -> peel_muts_internal_branch (pc IntMap.! b1) (pc IntMap.! b2) (as IntMap.! b1) (as IntMap.! b2) cost
                                                                _ -> error $ "cached_conditional_muts: " ++ show (numElements edges)  ++ "edges before edge not handled."
                                               in pc

peel_muts t cp as root seqs alpha cost = let muts = IntMap.fromSet peel_muts' $ getNodesSet t
                                             peel_muts' root = let branches_in = edgesTowardNodeArray t root
                                                                   b1 = branches_in!0
                                                                   b2 = branches_in!1
                                                                   b3 = branches_in!2
                                                               in case numElements branches_in of
                                                                    1 -> let n=targetNode t b1
                                                                         in calc_leaf_muts alpha (seqs IntMap.! n) (as IntMap.! b1) cost (cp IntMap.! b1)
                                                                    3 -> calc_root_muts (cp IntMap.! b1) (cp IntMap.! b2) (cp IntMap.! b3) (as IntMap.! b1) (as IntMap.! b2) (as IntMap.! b3) cost
                                                                    0 -> 0
                                                                    2 -> error "peel_muts: root node with 2 edges"
                                                                    _ -> error $ "peel_muts: root node has degree " ++ show (numElements branches_in)
                         in muts IntMap.! root


parsimony :: Tree t => t -> IntMap (EVector Int) -> IntMap PairwiseAlignment -> Alphabet -> MutCosts -> Int
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
        pcf b = let edges = edgesBeforeEdgeArray t b
                    b1 = edges!0
                    b2 = edges!1
                in case numElements edges of
                     0 -> let n = sourceNode t b
                              (seq,mask) = seqs IntMap.! n
                          in peel_muts_leaf_branch_fixed_A seq mask alpha cost
                     1 -> error "cached_conditional_muts_fixed_A: knuckles not handled yet."
                     2 -> peel_muts_internal_branch_fixed_A (pc IntMap.! b1) (pc IntMap.! b2) cost
                     _ -> error $ "cached_conditional_muts: " ++ show (numElements edges)  ++ "edges before edge not handled."
    in pc

peel_muts_fixed_A t cp root seqs alpha cost counts = let muts = IntMap.fromSet peel_muts' $ getNodesSet t
                                                         peel_muts' root = let branches_in = edgesTowardNodeArray t root
                                                                               b1 = branches_in!0
                                                                               b2 = branches_in!1
                                                                               b3 = branches_in!2
                                                                           in case numElements branches_in of
                                                                                1 -> let n = targetNode t b1
                                                                                         (seq,mask) = seqs IntMap.! n
                                                                                     in calc_leaf_muts_fixed_A alpha seq mask (cp IntMap.! b1) cost counts
                                                                                3 -> calc_root_muts_fixed_A (cp IntMap.! b1) (cp IntMap.! b2) (cp IntMap.! b3) cost counts
                                                                                0 -> 0
                                                                                2 -> error "peel_muts: root node with 2 edges"
                                                                                _ -> error $ "peel_muts: root node has degree " ++ show (numElements branches_in)
                                                      in muts IntMap.! root

parsimony_fixed_A :: Tree t => t -> IntMap (EVector Int, CBitVector) -> Alphabet -> MutCosts -> ColumnCounts -> Int
parsimony_fixed_A t seqs alpha cost counts = let pc = cached_conditional_muts_fixed_A t seqs alpha cost
                                                 root = head $ getNodes t
                                             in peel_muts_fixed_A t pc root seqs alpha cost counts
{-
parsimony_SEV :: Tree t => t -> IntMap (EVector Int) -> IntMap PairwiseAlignment -> Alphabet -> MutCosts -> Int
parsimony_SEV t seqs as alpha cost = let pc = cached_conditional_muts_SEV t seqs as alpha cost
                                        root = head $ getNodes t
                                     in peel_muts_SEV t pc as root seqs alpha cost
-}
