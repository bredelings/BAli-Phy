module Model where

import           Probability
import           Bio.Alignment
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           IModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

branch_length_dist topology branch = gamma (1/2) (2/fromIntegral n) where n = numBranches topology

model seq_data = do
    let taxa            = getTaxa seq_data
        tip_seq_lengths = get_sequence_lengths seq_data

    -- Tree
    scale1 <- prior $ gamma (1/2) 2
    tree   <- prior $ uniform_labelled_tree taxa branch_length_dist
    let tree1 = scale_branch_lengths scale1 tree

    -- Indel model
    indel_rate   <- prior $ log_laplace (-4) 0.707
    mean_length <- (1 +) <$> sample (exponential 10)
    let imodel = rs07 indel_rate mean_length tree

    -- Substitution model
    freqs  <- prior $ symmetric_dirichlet_on ["A", "C", "G", "T"] 1
    kappa1 <- prior $ log_normal 0 1
    kappa2 <- prior $ log_normal 0 1
    let tn93_model = tn93' dna kappa1 kappa2 freqs

    -- Alignment
    alignment <- prior $ random_alignment tree1 imodel tip_seq_lengths

    -- Observation
    observe seq_data $ phyloCTMC tree1 alignment tn93_model

    return
        [ "tree1" %=% write_newick tree1
        , "log(indel_rate)" %=% log indel_rate
        , "mean_length" %=% mean_length
        , "kappa1" %=% kappa1
        , "kappa2" %=% kappa2
        , "frequencies" %=% freqs
        , "scale1" %=% scale1
        , "|T|" %=% tree_length tree
        , "scale1*|T|" %=% tree_length tree1
        , "|A|" %=% alignmentLength alignment
        ]

main = do
    [filename] <- getArgs

    seq_data <- mkUnalignedCharacterData dna <$> load_sequences filename

    return $ model seq_data
