module BAliPhy.ATModel where

import BAliPhy.ATModel.DataPartition
import BAliPhy.ATModel.DataPartition as DP

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel Tree [MixtureModels] [IndelModel] [Double] [Double] [Int] [Partition] Bool Double Bool Int
tree               (ATModel t _  _ _ _ _ _  _ _ _ _) = t
smodels            (ATModel _ ss _ _ _ _ _  _ _ _ _) = ss
imodels            (ATModel _ _ is _ _ _ _  _ _ _ _) = is
scales             (ATModel _ _ _ rs _ _ _  _ _ _ _) = rs
branch_lengths     (ATModel _ _ _ _ ls _ _  _ _ _ _) = ls
branch_categories  (ATModel _ _ _ _ _ cs _  _ _ _ _) = cs
partitions         (ATModel _ _ _ _ _  _ ps _ _ _ _) = ps
imodel_training    (ATModel _ _ _ _ _  _ _  t _ _ _) = t
heat               (ATModel _ _ _ _ _  _ _  _ h _ _) = h
variable_alignment (ATModel _ _ _ _ _  _ _  _ _ v _) = v
subst_root         (ATModel _ _ _ _ _  _ _  _ _ _ r) = r
