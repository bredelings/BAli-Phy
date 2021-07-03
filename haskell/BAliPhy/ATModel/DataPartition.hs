module BAliPhy.ATModel.DataPartition where

import Data.Maybe
import Tree
import Bio.Alignment

data IModel
data SModel
data HMM
data PairwiseAlignment

data Partition = Partition {
      get_smodel :: SModel,

-- FIXME: Should we delete the imodel, or register it as an in_edge?
-- We only need the imodel for computing sequence length probabilities.
-- And we only use them in other_prior and A{3,5}::correction to check stuff.
      get_imodel :: Maybe IModel,

      get_tree :: BranchLengthTree,
      get_alignment :: AlignmentOnTree
    }


get_branch_lengths = branch_lengths . get_tree
