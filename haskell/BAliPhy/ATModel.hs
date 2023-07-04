module BAliPhy.ATModel where

import Foreign.Vector
import Tree
import SModel

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel t = ATModel {
      tree:: t
    }

data ConditionalLikelihoodVector
data LeafSequence
