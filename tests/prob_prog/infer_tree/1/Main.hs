import           Bio.Alignment               hiding ( sample_alignment )
import           Bio.Alphabet
import           Probability
import           Tree
import           Tree.Newick
import           Data.BitVector
import           SModel.Likelihood
import           SModel
import           SModel.Nucleotides
import           SModel.Frequency -- for frequencies_from_dict
import           IModel
import           Probability.Distribution.OnTree

-- issues: 1. likelihood seems wrong - -1300 vs -700.
--         2. no topology moves included.
sample_imodel topology = do
    logLambda   <- log_laplace (-4.0) 0.707
    mean_length <- (1.0+) <$> exponential 10.0

    let imodel  = rs07 logLambda mean_length topology
    let loggers = ["logLambda" %=% logLambda, "mean_length" %=% mean_length]

    return (imodel, loggers)

sample_smodel = do
    pi     <- dirichlet_on ["A", "C", "G", "T"] [1.0, 1.0, 1.0, 1.0]
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0

    let tn93_model = tn93 dna kappa1 kappa2 (frequencies_from_dict dna pi)
    let loggers = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "pi" %=% pi]

    return (tn93_model, loggers)

sample_alignment topology ts imodel scale tip_seq_lengths = do
    let n_branches = numBranches topology
        ds         = listArray' $ map (* scale) ts
        hmms       = branch_hmms imodel ds n_branches
    alignment_on_tree <- random_alignment topology hmms imodel tip_seq_lengths
    return $ Bio.Alignment.pairwise_alignments alignment_on_tree

prior taxa tip_seq_lengths = do

    topology <- uniform_labelled_topology taxa

    let b           = numBranches topology
        root        = targetNode topology 0

    ts                        <- iid b (gamma 0.5 (2.0 / intToDouble b))

    scale                     <- gamma 0.5 2.0

    (smodel, smodel_loggers) <- sample_smodel

    (imodel, imodel_loggers) <- sample_imodel topology

    as                       <- Main.sample_alignment topology ts imodel scale tip_seq_lengths

    let loggers =
            ["topology" %=% write_newick topology, "T" %=% ts, "scale" %=% scale, "tn93" %>% smodel_loggers, "rs07" %>% imodel_loggers]

    return (ctmc_on_tree topology root as smodel ts scale, loggers)

observe_data seq_data = do
    let taxa            = map sequence_name seq_data
        tip_seq_lengths = get_sequence_lengths dna seq_data

    (seq_dist, loggers) <- sample $ prior taxa tip_seq_lengths

    seq_data ~> seq_dist

    return loggers

main = do
  let seq_data        = load_sequences "5d-muscle.fasta"
  mcmc $ observe_data seq_data
