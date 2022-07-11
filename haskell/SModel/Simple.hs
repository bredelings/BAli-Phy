module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree

data SingleBranchLengthModel t a = SingleBranchLengthModel t a
get_tree' (SingleBranchLengthModel t _) = t        -- Avoid aliasing with get_tree from DataPartition

class SimpleSModel m where
    get_smap :: m -> EVector Int
    branch_transition_p :: BranchLengthTree t => SingleBranchLengthModel t m -> Int -> [Matrix Double]
    distribution :: m -> [Double]
    weighted_frequency_matrix :: m -> Matrix Double
    frequency_matrix :: m -> Matrix Double
    nBaseModels :: m -> Int
    stateLetters :: m -> EVector Int
    getAlphabet :: m -> Alphabet
    componentFrequencies :: m -> Int -> EVector Double

