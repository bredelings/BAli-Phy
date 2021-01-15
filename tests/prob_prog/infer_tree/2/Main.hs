import           Probability
import           Bio.Alphabet
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree

sample_smodel = do
    freqs  <- dirichlet_on ["A", "C", "G", "T"] [1.0, 1.0, 1.0, 1.0]
    kappa1 <- log_normal 0.0 1.0
    kappa2 <- log_normal 0.0 1.0

    let tn93_model = tn93 dna kappa1 kappa2 (frequencies_from_dict dna freqs)
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)

prior taxa = do

    topology <- uniform_labelled_topology taxa

    let b = numBranches topology
    times <- iid b (gamma 0.5 (2.0 / intToDouble b))
    scale <- gamma 0.5 2.0
    let distances = map (scale *) times

    let tree      = branch_length_tree topology distances

    (smodel, sloggers) <- sample_smodel

    let loggers = ["tree" %=% write_newick tree, "T" %=% times, "scale" %=% scale, "tn93" %>% sloggers]

    return (tree, smodel, loggers)

observe_data seq_data = do
    let taxa = map sequence_name seq_data

    (tree, smodel, loggers) <- sample $ prior taxa

    seq_data ~> ctmc_on_tree_fixed_A tree smodel

    return loggers

main = do
    let seq_data = load_sequences "5d-muscle.fasta"
    mcmc $ observe_data seq_data
