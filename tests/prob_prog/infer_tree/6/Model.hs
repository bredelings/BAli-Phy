module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

-- Non-zero branches are slightly longer to keep to average length correct.
-- The division means that we can't set the value though.
maybe_zero p dist = do
    is_zero <- prior $ bernoulli p
    length  <- prior $ dist
    if is_zero == 1 then return 0 else return (length / (1 - p))

branch_length_dist zero_p topology b | is_internal_branch topology b = branch_dist_internal
                                     | otherwise                     = branch_dist_leaf
  where
    n                    = numBranches topology
    branch_dist_leaf     = prior $ gamma 0.5 (2 / fromIntegral n)
    branch_dist_internal = maybe_zero zero_p branch_dist_leaf

model seq_data = do

    let taxa = map sequenceName seq_data

    zero_p <- prior $ beta 0.1 1

    scale  <- prior $ gamma 0.5 2

    tree   <- scale_branch_lengths scale <$> prior (uniform_labelled_tree taxa (branch_length_dist zero_p))

    freqs  <- prior $ symmetric_dirichlet_on (letters dna) 1

    kappa1 <- prior $ log_normal 0 1

    kappa2 <- prior $ log_normal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seq_data $ ctmc_on_tree_fixed_A tree tn93_model

    return
        [ "tree" %=% write_newick tree
        , "scale" %=% scale
        , "zero_p" %=% zero_p
        , "kappa1" %=% kappa1
        , "kappa2" %=% kappa2
        , "frequencies" %=% freqs
        ]

main = do
    [filename] <- getArgs

    seq_data <- load_sequences filename

    return $ model seq_data
