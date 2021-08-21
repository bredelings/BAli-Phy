import           Probability
import           Bio.Alignment
import           Bio.Alphabet
import           Tree
import           Tree.Newick
import           SModel
import           IModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

branch_length_dist topology branch = gamma 0.5 (2.0 / intToDouble n) where n = numBranches topology

model seq_data = do
    let taxa            = map sequence_name seq_data
        tip_seq_lengths = get_sequence_lengths dna seq_data

    -- Tree
    scale1 <- gamma 0.5 2.0
    tree   <- uniform_labelled_tree taxa branch_length_dist
    let tree1 = scale_branch_lengths scale1 tree

    -- Indel model
    indel_rate   <- log_laplace (-4.0) 0.707
    mean_length <- (1.0 +) <$> exponential 10.0
    let imodel = rs07 indel_rate mean_length tree

    -- Substitution model
    freqs  <- symmetric_dirichlet_on ["A", "C", "G", "T"] 1.0
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0
    let tn93_model = tn93' dna kappa1 kappa2 freqs

    -- Alignment
    alignment <- random_alignment tree1 imodel tip_seq_lengths

    -- Observation
    seq_data ~> ctmc_on_tree tree1 alignment tn93_model

    return
        [ "tree1" %=% write_newick tree1
        , "indel_rate" %=% indel_rate
        , "mean_length" %=% mean_length
        , "kappa1" %=% kappa1
        , "kappa2" %=% kappa2
        , "frequencies" %=% freqs
        , "scale1" %=% scale1
        , "|T|" %=% tree_length tree
        , "scale1*|T|" %=% tree_length tree1
        , "|A|" %=% alignment_on_tree_length alignment
        ]

main = do
    [filename] <- getArgs

    let seq_data = load_sequences filename

    mcmc $ model seq_data
