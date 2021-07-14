module BAliPhy.ATModel where

import BAliPhy.ATModel.DataPartition
import BAliPhy.ATModel.DataPartition as DP
import Foreign.Vector
import Tree
import SModel

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel {
      tree:: Tree ,
      smodels :: [MixtureModels],
      scales :: [Double],
      branch_lengths :: [Double],
      branch_categories :: [Int],
      partitions :: [Partition]
    }

data ConditionalLikelihoodVector
data Matrix
data LeafSequence
