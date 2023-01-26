import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

model seq_data = do

    let taxa = zip [0..] $ map sequence_name seq_data

    age    <- gamma 0.5 2.0
    tree   <- add_labels taxa <$> uniform_time_tree age (length taxa)

    freqs  <- symmetric_dirichlet_on ["A", "C", "G", "T"] 1.0
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0
    let tn93_model = tn93' dna kappa1 kappa2 freqs

    seq_data ~> ctmc_on_tree_fixed_A tree tn93_model

    return ["tree" %=% write_newick (make_rooted tree),
            "age" %=% age,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs]

main = do
    [filename] <- getArgs

    seq_data <- load_sequences filename

    mcmc $ model seq_data
