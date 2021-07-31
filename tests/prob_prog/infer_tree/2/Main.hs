import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

branch_length_dist topology b = gamma 0.5 (2.0 / intToDouble n) where n = numBranches topology

model seq_data = do

    let taxa = map sequence_name seq_data

    scale  <- gamma 0.5 2.0

    tree   <- scaled_branch_lengths scale <$> uniform_labelled_tree taxa branch_length_dist

    freqs  <- symmetric_dirichlet_on ["A", "C", "G", "T"] 1.0
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0
    let tn93_model = tn93' dna kappa1 kappa2 freqs

    seq_data ~> ctmc_on_tree_fixed_A tree tn93_model

    return
        ["tree" %=% write_newick tree, "scale" %=% scale, "tn93:kappa1" %=% kappa1, "tn93:kappa2" %=% kappa2, "tn93:frequencies" %=% freqs]

main = do
    [filename] <- getArgs

    let seq_data = load_sequences filename

    mcmc $ model seq_data
