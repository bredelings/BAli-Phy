import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           Probability.Distribution.Tree
import           System.Environment  -- for getArgs

smodel_prior nucleotides =  do
    freqs  <- symmetric_dirichlet_on (letters nucleotides) 1.0
    kappa1 <- log_normal (log 2.0) 0.25
    kappa2 <- log_normal (log 2.0) 0.25

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    theta <- log_laplace (-5.0) 2.0
    tree <- add_labels taxa <$> coalescent_tree theta (length taxa)

    let loggers   = ["tree" %=% write_newick tree, "theta" %=% theta]
    return (tree, loggers)


model seq_data = do
    let taxa = map sequence_name seq_data

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    let loggers = tree_loggers ++ ["tn93" %>% sloggers]

    seq_data ~> ctmc_on_tree_fixed_A tree smodel

    return loggers

main = do
    [filename] <- getArgs

    seq_data <- load_sequences filename

    mcmc $ model seq_data
