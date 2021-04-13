import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

smodel_prior = do
    freqs  <- symmetric_dirichlet_on ["A", "C", "G", "T"] 1.0
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0

    let tn93_model = tn93' dna kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    topology <- uniform_labelled_topology taxa

    let b = numBranches topology
    times <- iid b (gamma 0.5 (2.0 / intToDouble b))
    scale <- gamma 0.5 2.0
    let distances = map (scale *) times

    let tree      = branch_length_tree topology distances

    let loggers   = ["tree" %=% write_newick tree, "scale" %=% scale]
    return (tree, loggers)


prior taxa = do

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior

    let loggers = tree_loggers ++ ["tn93" %>% sloggers]

    return (tree, smodel, loggers)


observe_data seq_data = do
    let taxa = map sequence_name seq_data

    (tree, smodel, loggers) <- sample $ prior taxa

    seq_data ~> ctmc_on_tree_fixed_A tree smodel

    return loggers

main = do
    args <- getArgs

    let filename = args !! 0
        seq_data = load_sequences filename

    mcmc $ observe_data seq_data
