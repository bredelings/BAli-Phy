module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

model seqData = do

    let taxa = zip [0..] $ getTaxa seqData

    age    <- sample $ gamma 0.5 2
    tree   <- add_labels taxa <$> sample (uniform_time_tree age (length taxa))

    freqs  <- sample $ symmetric_dirichlet_on ["A", "C", "G", "T"] 1
    kappa1 <- sample $ log_normal 0 1
    kappa2 <- sample $ log_normal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seqData $ ctmc_on_tree tree (alignmentLength seqData) tn93_model

    return ["tree" %=% write_newick tree,
            "age" %=% age,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs]

main = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    return $ model seqData

