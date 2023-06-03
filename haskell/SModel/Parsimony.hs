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

foreign import bpcall "Parsimony:unitCostMatrix" unitCostMatrix' :: Int -> MutCosts
unitCostMatrix a = unitCostMatrix' $ length $ letters $ a

foreign import bpcall "Parsimony:" peel_muts_leaf_branch :: EVector Int -> Alphabet -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" peel_muts_internal_branch :: CondPars -> CondPars -> PairwiseAlignment -> PairwiseAlignment -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" calc_root_muts :: CondPars -> CondPars -> CondPars -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> MutCosts -> Int
foreign import bpcall "Parsimony:" calc_leaf_muts :: Alphabet -> EVector Int -> PairwiseAlignment -> MutCosts -> CondPars -> Int


cached_conditional_muts t seqs as alpha cost = let pc    = IntMap.fromSet pcf $ getEdgesSet t
                                                   pcf b = let edges = edgesBeforeEdge t b
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
                                             peel_muts' root = let branches_in = edgesTowardNode t root
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
