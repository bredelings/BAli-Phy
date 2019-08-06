module BAliPhy.ATModel where

import BAliPhy.ATModel.DataPartition
import BAliPhy.ATModel.DataPartition as DP
import Foreign.Vector

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel Tree [MixtureModels] [IndelModel] [Double] [Double] [Int] [Partition]
tree               (ATModel t _  _ _ _ _ _  ) = t
smodels            (ATModel _ ss _ _ _ _ _  ) = ss
imodels            (ATModel _ _ is _ _ _ _  ) = is
scales             (ATModel _ _ _ rs _ _ _  ) = rs
branch_lengths     (ATModel _ _ _ _ ls _ _  ) = ls
branch_categories  (ATModel _ _ _ _ _ cs _  ) = cs
partitions         (ATModel _ _ _ _ _  _ ps ) = ps

data ConditionalLikelihoodVector
data Matrix
data ATModelExport = ATModelExport ATModel [Array Int Matrix] [Array Int ConditionalLikelihoodVector] [Double] Bool Double Bool Int EVector

get_atmodel           (ATModelExport at _  _  _  _ _ _ _ _ ) = at
get_all_transition_ps (ATModelExport _  ps _  _  _ _ _ _ _ ) = ps
get_all_cond_likes    (ATModelExport _  _  cl _  _ _ _ _ _ ) = cl
get_all_likelihoods   (ATModelExport _  _  _  ls _ _ _ _ _ ) = ls
imodel_training       (ATModelExport _  _  _  _  t _ _ _ _ ) = t
heat                  (ATModelExport _  _  _  _  _ h _ _ _ ) = h
variable_alignment    (ATModelExport _  _  _  _  _ _ v _ _ ) = v
subst_root            (ATModelExport _  _  _  _  _ _ _ r _ ) = r
sequence_names        (ATModelExport _  _  _  _  _ _ _ _ ns) = ns
