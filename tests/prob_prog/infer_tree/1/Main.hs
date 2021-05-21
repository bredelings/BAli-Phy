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

-- Here, hmms should be a property of the imodel on the tree, like the transition_ps
--   are a property of smodelOnTree (or however it is spelled).
-- However, we currently pass them in to random_alignment because we need to export
--   them, and its not clear how to get them out if we generate them inside random-alignment.
sample_alignment tree imodel tip_seq_lengths = do
    let n_branches = numBranches tree
        ds         = Tree.branch_lengths tree
        hmms       = branch_hmms imodel ds n_branches
    alignment_on_tree <- random_alignment tree hmms imodel tip_seq_lengths
    return alignment_on_tree

tree_prior taxa = do
    topology <- uniform_labelled_topology taxa

    let b           = numBranches topology

    times                     <- iid b (gamma 0.5 (2.0 / intToDouble b))

    scale                     <- gamma 0.5 2.0

    let distances = map (scale*) times

    let loggers = ["scale" %=% scale]

    return (branch_length_tree topology distances, loggers)

prior taxa tip_seq_lengths = do

    (tree,tree_loggers)       <- tree_prior taxa

    (smodel, smodel_loggers) <- smodel_prior

    (imodel, imodel_loggers) <- imodel_prior tree

    alignment                <- Main.sample_alignment tree imodel tip_seq_lengths

    let loggers = ["tree" %=% write_newick tree, "tree" %>% tree_loggers, "tn93" %>% smodel_loggers, "rs07" %>% imodel_loggers]

    return (ctmc_on_tree tree alignment smodel, loggers)

observe_data seq_data = do
    let taxa            = map sequence_name seq_data
        tip_seq_lengths = get_sequence_lengths dna seq_data

    (seq_dist, loggers) <- sample $ prior taxa tip_seq_lengths

    seq_data ~> seq_dist

    return loggers

main = do
    args <- getArgs

    let filename = args !! 0
        seq_data = load_sequences filename

    mcmc $ observe_data seq_data
