import           Probability
import           Bio.Alignment               hiding ( sample_alignment )
import           Bio.Alphabet
import           Tree
import           Tree.Newick
import           SModel
import           IModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

-- issues: 1. likelihood seems wrong - -1300 vs -700.
--         2. no topology moves included.
imodel_prior topology = do
    logLambda   <- log_laplace (-4.0) 0.707
    mean_length <- (1.0+) <$> exponential 10.0

    let imodel  = rs07 logLambda mean_length topology
    let loggers = ["logLambda" %=% logLambda, "mean_length" %=% mean_length]

    return (imodel, loggers)

smodel_prior = do
    freqs  <- symmetric_dirichlet_on ["A", "C", "G", "T"] 1.0
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0

    let tn93_model = tn93' dna kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)

branch_length_dist topology b = gamma 0.5 (2.0 / intToDouble n) where n = numBranches topology

model seq_data = do
    let taxa            = map sequence_name seq_data
        tip_seq_lengths = get_sequence_lengths dna seq_data

    scale                     <- gamma 0.5 2.0

    tree <- scale_branch_lengths scale <$> uniform_labelled_tree taxa branch_length_dist

    (smodel, smodel_loggers) <- smodel_prior

    (imodel, imodel_loggers) <- imodel_prior tree

    alignment                <- random_alignment tree imodel tip_seq_lengths

    seq_data ~> ctmc_on_tree tree alignment smodel

    return ["tree" %=% write_newick tree,
            "tn93" %>% smodel_loggers,
            "rs07" %>% imodel_loggers ,
            "scale" %=% scale,
            "|T|" %=% tree_length tree,
            "scale*|T|" %=% scale * tree_length tree]

main = do
    [filename] <- getArgs

    let seq_data = load_sequences filename

    mcmc $ model seq_data
