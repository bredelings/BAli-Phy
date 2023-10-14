module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

model seq_data = do

    let taxa = zip [0..] $ map fst seq_data

    age    <- sample $ gamma 0.5 2
    tree   <- add_labels taxa <$> sample (uniform_time_tree age (length taxa))

    freqs  <- sample $ symmetric_dirichlet_on ["A", "C", "G", "T"] 1
    kappa1 <- sample $ log_normal 0 1
    kappa2 <- sample $ log_normal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seq_data $ ctmc_on_tree_fixed_A tree tn93_model

    return ["tree" %=% write_newick tree,
            "age" %=% age,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs]

main = do
    [filename] <- getArgs

    seq_data <- load_sequences filename

    return $ model seq_data
