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
      get_imodel :: Maybe IModel,
      get_tree :: BranchLengthTree,
      get_alignment :: AlignmentOnTree,
      get_hmms :: Maybe (Array Int HMM)
    }


get_branch_lengths = branch_lengths . get_tree
